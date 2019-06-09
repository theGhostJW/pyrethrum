module LogTransformation.Iteration (
  emptyIterationAccum,
  iterationStep,
  IterationAccum(..),
  IterationLogElement(..),
  Issues(..),
  ExecutionStatus(..),
  IterationRecord(..),
  IterationAuxEvent(..),
  LogTransformError(..),
  IterationSummary(..),
  IterationError(..),
  IterationWarning(..),
  ItemInfo(..),
  ApStateInfo(..),
  PrepStateInfo(..)
) where

import Common as C (AppError(..))
import LogTransformation.Common
import Check as CK
import Pyrelude as P
import Data.DList as D
import DSL.LogProtocol as L
import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import RunElementClasses
import PrettyPrintCommon

-- TODO: creation relational records
-- relational records from Iteration records and use reporting service
-- to provide full report - use sql lite locally 
-- see https://www.oreilly.com/library/view/microservices-antipatterns-and/9781492042716/ch04.html

data IterationLogElement = Iteration IterationRecord |
                    BoundaryItem IterationAuxEvent  |
                    LineError LogTransformError 
                    deriving (Show, Eq)

--------------------------------------------------------
----------------- Iteration Aggregation ----------------
--------------------------------------------------------

data IterationAuxEvent = 
  FilterLog [FilterResult] |

  StartRun RunTitle A.Value | 
  EndRun |

  StartGroup GroupTitle |
  EndGroup GroupTitle |

  StartTest TestDisplayInfo |
  EndTest TestModule 
  deriving (Eq, Show)

data IterationPhase = OutOfIteration | 
                      PreInteractor | 
                      Interactor | 
                      PrePrepState |
                      PrepState |
                      PreChecks |
                      Checks 
                      deriving (Eq, Ord, Show)

data ExecutionStatus = Inconclusive |
                       Pass |
                       Warning IterationPhase |
                       Fail IterationPhase 
                       deriving (Eq, Show)
                      
instance Ord ExecutionStatus where
     (<=) :: ExecutionStatus -> ExecutionStatus -> Bool 
     Inconclusive <= _ = True

     LogTransformation.Iteration.Pass <= Inconclusive = False
     LogTransformation.Iteration.Pass <= LogTransformation.Iteration.Pass = True
     LogTransformation.Iteration.Pass <= LogTransformation.Iteration.Warning _ = True
     LogTransformation.Iteration.Pass <= LogTransformation.Iteration.Fail _ = True

     LogTransformation.Iteration.Warning _ <= Inconclusive = False
     LogTransformation.Iteration.Warning _ <= LogTransformation.Iteration.Pass = True
     LogTransformation.Iteration.Warning p0 <= LogTransformation.Iteration.Warning p1 = p0 > p1 -- if phase is greater then warning is smaller (favour earlier warnings)
     LogTransformation.Iteration.Warning _ <= LogTransformation.Iteration.Fail _ = True

     LogTransformation.Iteration.Fail _ <= Inconclusive = False
     LogTransformation.Iteration.Fail _ <= LogTransformation.Iteration.Pass = False
     LogTransformation.Iteration.Fail _ <= LogTransformation.Iteration.Warning _ = False
     LogTransformation.Iteration.Fail p0 <= LogTransformation.Iteration.Fail p1 = p0 > p1 -- if phase is greater then warning is smaller (favour earlier failures)

isFailure :: ExecutionStatus -> Bool
isFailure = \case 
              Inconclusive -> False
              LogTransformation.Iteration.Pass -> False
              LogTransformation.Iteration.Warning _ -> False
              LogTransformation.Iteration.Fail _ -> True

isWarning :: ExecutionStatus -> Bool
isWarning = \case 
              Inconclusive -> False
              LogTransformation.Iteration.Pass -> False
              LogTransformation.Iteration.Warning _ -> True
              LogTransformation.Iteration.Fail _ -> False

calcStatus :: Issues -> (IterationPhase -> ExecutionStatus)
calcStatus issues  
   | type2Failure issues > 0 = LogTransformation.Iteration.Fail 
   | LogTransformation.Iteration.fail issues > 0 = LogTransformation.Iteration.Fail 
   | regression issues > 0 = LogTransformation.Iteration.Fail 

   | (LogTransformation.Iteration.warning :: Issues -> Int) issues > 0 = LogTransformation.Iteration.Warning
   | expectedFailure issues > 0 = LogTransformation.Iteration.Warning

   | otherwise = const LogTransformation.Iteration.Pass

data IterationSummary = IterationSummary {
                          iid :: ItemId,
                          pre :: WhenClause,
                          post:: ThenClause,
                          status :: ExecutionStatus,
                          issues :: Issues
                        } deriving (Eq, Show)

data Issues = Issues {
  expectedFailure :: Int,
  warning :: Int,
  type2Failure :: Int,
  fail :: Int,
  regression :: Int
}  deriving (Show, Eq)

instance Semigroup Issues where 
  s0 <> s1 = 
    let 
      plus :: (Issues -> Int) -> Int
      plus f = f s0 + f s1 
    in 
      Issues {
                warning = plus warning,
                expectedFailure = plus expectedFailure,
                type2Failure = plus type2Failure,
                fail = plus LogTransformation.Iteration.fail,
                regression = plus regression
              } 

instance Monoid Issues where 
  mempty = Issues 0 0 0 0 0

data IterationError = IterationError {
    phase :: IterationPhase,
    error :: LogProtocol
  } deriving (Eq, Show)
                          
data IterationWarning = IterationWarning {
    phase :: IterationPhase,
    warning :: LogProtocol
  } deriving (Eq, Show)
                                        
data IterationRecord = IterationRecord {
  summary :: IterationSummary,
  validation :: [CheckReport],
  otherErrorsDesc :: [IterationError],
  otherWarningsDesc :: [IterationWarning],
  item :: Maybe ItemInfo,
  apState :: Maybe ApStateInfo,
  domainState :: Maybe PrepStateInfo,
  rawLog :: DList LogProtocol
} deriving (Eq, Show)

data ItemInfo = ItemInfo ItemId WhenClause ThenClause A.Value deriving (Eq, Show)

data ApStateInfo = SucceededInteractor ApStateDisplay | 
                   FailedInteractor AppError
                   deriving (Eq, Show)

data PrepStateInfo = SucceededPrepState DStateDisplay |
                     FailedPrepState AppError
                     deriving (Eq, Show)

data FailStage = InteractorFailed |
                 PrepStateFailed |
                 NoFailure
                 deriving (Eq, Show)
     
data IterationAccum = IterationAccum {
  phase :: IterationPhase,
  stageFailure :: FailStage,
  rec :: Maybe IterationRecord
} deriving (Eq, Show)

emptyIterationAccum :: IterationAccum
emptyIterationAccum = IterationAccum {
  phase = OutOfIteration,
  stageFailure = NoFailure,
  rec = Nothing
}


updateIterationErrsWarnings:: IterationPhase -> LogProtocol -> IterationRecord -> IterationRecord
updateIterationErrsWarnings p lp iRec = 
  let
    updatedIssues :: Issues
    updatedIssues = updateIssues . issues $ summary iRec

    worstStatus :: ExecutionStatus
    worstStatus = calcStatus updatedIssues p

    stepStatus :: ExecutionStatus
    stepStatus = calcStatus (updateIssues mempty) p

    updateIssues :: Issues -> Issues
    updateIssues oldIssues = 
      let 
        modifier :: Issues -> Issues
        modifier = 
          -- good motivating case for lens
          let 
            incFailure :: Issues -> Issues
            incFailure s = s {LogTransformation.Iteration.fail = LogTransformation.Iteration.fail s + 1}
          
            incExpectedFailure :: Issues -> Issues
            incExpectedFailure s = s {LogTransformation.Iteration.expectedFailure = LogTransformation.Iteration.expectedFailure s + 1}
          
            incRegresssion :: Issues -> Issues
            incRegresssion s = s {LogTransformation.Iteration.regression = LogTransformation.Iteration.regression s + 1}

            incWarning :: Issues -> Issues
            incWarning s = s {LogTransformation.Iteration.warning = (LogTransformation.Iteration.warning :: Issues -> Int) s + 1 }
          in
            case lp of
              BoundaryLog bl -> id  -- this should not happen and will cause a phase error to be logged
              IterationLog (Doc dp) -> id -- this should not happen and will cause a phase error to be logged
              IterationLog (Run rp) -> 
                case rp of
                  StartPrepState -> id
                  IOAction _ -> id

                  StartInteraction -> id
                  InteractorSuccess {} -> id 
                  InteractorFailure {} -> incFailure

                  L.PrepStateSuccess {} -> id
                  PrepStateFailure {} -> incFailure

                  StartChecks{} -> id
                  CheckOutcome _ (CheckReport reslt _) -> 
                    case reslt of
                      CK.Pass -> id
                      CK.Fail -> incFailure
                      GateFail -> incFailure
                      FailExpected _ -> incExpectedFailure
                      GateFailExpected _ -> incExpectedFailure
                      PassWhenFailExpected _ -> 
                        \s -> s {LogTransformation.Iteration.type2Failure = LogTransformation.Iteration.type2Failure s + 1}
                      Regression _ -> incRegresssion
                      GateRegression _ -> incRegresssion
                      Skip -> id

                  Message _ -> id
                  Message' _ -> id
                                          
                  L.Warning _ -> incWarning
                  Warning' _ -> incWarning
                  L.Error _ -> incFailure
        in 
          modifier oldIssues

    notCheckPhase :: Bool
    notCheckPhase = p /= Checks

  in 
    iRec {
      summary = (summary iRec) {
                                status = worstStatus,
                                issues = updatedIssues
                               }

      , otherErrorsDesc = isFailure stepStatus && notCheckPhase 
                              ? IterationError p lp : otherErrorsDesc iRec  
                              $ otherErrorsDesc iRec

      , otherWarningsDesc = isWarning stepStatus && notCheckPhase 
                              ? IterationWarning p lp : otherWarningsDesc iRec 
                              $ otherWarningsDesc iRec
    }

apppendRaw :: LogProtocol -> IterationRecord -> IterationRecord
apppendRaw lp iRec = iRec {rawLog = D.snoc (rawLog iRec) lp} 

failStage :: LogProtocol -> FailStage
failStage = \case
                BoundaryLog bl -> case bl of 
                                      L.StartRun{} -> NoFailure
                                      L.EndRun -> NoFailure
                                      
                                      L.FilterLog _ -> NoFailure
                                      L.StartGroup _ -> NoFailure
                                      L.EndGroup _ -> NoFailure
                                      L.StartTest _ -> NoFailure
                                      L.EndTest _ -> NoFailure

                                      L.StartIteration{} -> NoFailure
                                      L.EndIteration _ -> NoFailure

                -- should never happen
                IterationLog (Doc _) -> NoFailure
                IterationLog (Run rp) -> case rp of
                                      StartPrepState -> NoFailure
                                      IOAction _ -> NoFailure
                                      StartInteraction -> NoFailure
                                      InteractorSuccess{} -> NoFailure
                                      InteractorFailure{}  -> InteractorFailed
                                      L.PrepStateSuccess{}  -> NoFailure
                                      PrepStateFailure iid err -> PrepStateFailed
                                      StartChecks{} -> NoFailure
                                      CheckOutcome{} -> NoFailure
                                      Message _ -> NoFailure
                                      Message' _ -> NoFailure
                                      L.Warning{} -> NoFailure
                                      Warning' _ -> NoFailure
                                      L.Error _ -> NoFailure

expectedCurrentPhase :: IterationPhase -> FailStage -> LogProtocol -> IterationPhase
expectedCurrentPhase current fs lp = case lp of
                                      BoundaryLog bl -> case bl of 
                                                            L.StartRun{} -> OutOfIteration
                                                            L.EndRun -> OutOfIteration
                                                            
                                                            L.FilterLog _ -> OutOfIteration
                                                            L.StartGroup _ -> OutOfIteration
                                                            L.EndGroup _ -> OutOfIteration
                                                            L.StartTest _ -> OutOfIteration
                                                            L.EndTest _ -> OutOfIteration

                                                            L.StartIteration{} -> OutOfIteration
                                                            L.EndIteration _ -> case fs of 
                                                                                NoFailure -> Checks  -- TODO: ensure raw file test with no checks
                                                                                InteractorFailed -> Interactor
                                                                                PrepStateFailed -> PrepState

                                      -- should never happen
                                      IterationLog (Doc _) -> current
                                      IterationLog (Run rp) -> case rp of
                                                            StartPrepState -> PrePrepState
                                                            IOAction _ -> current

                                                            StartInteraction -> PreInteractor
                                                            InteractorSuccess{} -> Interactor
                                                            InteractorFailure{}  -> Interactor

                                                            L.PrepStateSuccess{}  -> PrepState
                                                            PrepStateFailure iid err -> PrepState

                                                            StartChecks{} -> PreChecks
                                                            CheckOutcome{} -> Checks
                                                            Message _ -> current
                                                            Message' _ -> current
                                                            L.Warning{} -> current
                                                            Warning' _ -> current
                                                            L.Error _ -> current

nextPhase :: IterationPhase -> LogProtocol -> IterationPhase
nextPhase current lp = case lp of
                          BoundaryLog bl -> case bl of 
                                              L.StartRun{} -> OutOfIteration
                                              L.EndRun -> OutOfIteration

                                              L.FilterLog _ -> OutOfIteration
                                              L.StartGroup _ -> OutOfIteration
                                              L.EndGroup _ -> OutOfIteration
                                              L.StartTest _ -> OutOfIteration
                                              L.EndTest _ -> OutOfIteration
                                              L.StartIteration{} -> PreInteractor
                                              L.EndIteration _ -> OutOfIteration

                                              -- should never happen
                          IterationLog (Doc _) -> current
                          
                          IterationLog (Run rp) -> case rp of
                                                L.Error _ -> current
                                                StartPrepState -> PrepState
                                                IOAction _ -> current
                                                StartInteraction -> Interactor
                                                InteractorSuccess{} -> PrePrepState 
                                                InteractorFailure{}  -> Interactor  -- leave in failed phase
                                                L.PrepStateSuccess{}  -> PreChecks
                                                PrepStateFailure iid err -> PrepState -- leave in failed phase
                                                StartChecks{} -> Checks
                                                Message _ -> current
                                                Message' _ -> current
                                                L.Warning s -> current
                                                Warning' detailedInfo -> current
                                                CheckOutcome{}  -> Checks

iterationStep ::
              LineNo                                                                -- lineNo
              -> IterationAccum                                                     -- accum
              -> LogProtocol                                                        -- parse error or apperror
              -> (IterationAccum, Either LogTransformError (Maybe [IterationLogElement])) -- (newAccum, err / result)
iterationStep lineNo accum@(IterationAccum lastPhase stageFailure mRec) lp = 
  let
    isStartIteration :: Bool 
    isStartIteration = case lp of
                          BoundaryLog StartIteration{} -> True
                          _ -> False

    isEndIteration ::  Bool 
    isEndIteration = case lp of 
                        BoundaryLog EndIteration{} -> True
                        _ -> False

    -- this is the wrong kind of IterationLog
    -- should never happen                                                 
    isDocLog ::  Bool
    isDocLog = case lp of
                  IterationLog (Doc _) -> True
                  _ -> False

    phaseChangeIsValid :: Bool
    phaseChangeIsValid =  expectedCurrentPhase lastPhase stageFailure lp == lastPhase
                          && ((isStartIteration || (lastPhase == OutOfIteration)) == isNothing mRec)
                          && not isDocLog

    invalidPhaseStep :: (IterationAccum, Either LogTransformError (Maybe [IterationLogElement])) 
    invalidPhaseStep =
      let 
        nextAccum :: IterationAccum
        nextAccum = IterationAccum {
          phase = OutOfIteration,         
          stageFailure = NoFailure,
          rec = Nothing
        } 

        err :: IterationLogElement
        err = LineError LogTransformError {
                            linNo = lineNo,
                            logItem = lp,
                            info = isDocLog 
                                      ? "A documentation log has been encountered during a run - this type of log should message not be generated during a run"
                                      $ "Unexpected log message encounterred - Messages have either been lost or received out of order.\n"
                                          <> "Test and Interation summaries may not reflect true results of the test"
                          } 
      in
        (nextAccum, Right . Just $ maybef mRec 
                                    [err] 
                                    (\r -> [Iteration r , err]) -- if the record exists close it off add add a error record after
        )

    -- the only Log prtocol that produces a new rec is StartIteration
    newRec :: Maybe IterationRecord
    newRec = case lp of 
                BoundaryLog (StartIteration iid pre post val) -> pure $ IterationRecord {
                                                    summary = IterationSummary {
                                                      iid = iid,
                                                      pre = pre,
                                                      post = post,
                                                      status = Inconclusive,
                                                      issues = mempty
                                                    },
                                                    validation = [],
                                                    otherErrorsDesc = [],
                                                    otherWarningsDesc = [],
                                                    item = Just $ ItemInfo iid pre post val,
                                                    apState = Nothing,
                                                    domainState = Nothing,
                                                    rawLog = D.empty
                                                  }
                _ -> Nothing


    outOfIteration :: LogProtocol -> IterationLogElement
    outOfIteration = \case 
                        logp@(BoundaryLog be) -> 
                          let 
                            bug = LineError LogTransformError {
                                                                linNo = lineNo,
                                                                logItem = logp,
                                                                info = "Logic bug in iteration record generation start or end iteration record should not be processed by this function"
                                                              }
                          in
                            case be of 
                              L.FilterLog lg -> BoundaryItem $ LogTransformation.Iteration.FilterLog lg

                              L.StartRun t v -> BoundaryItem $ LogTransformation.Iteration.StartRun t v
                              L.EndRun -> BoundaryItem LogTransformation.Iteration.EndRun
                          
                              L.StartGroup t -> BoundaryItem $ LogTransformation.Iteration.StartGroup t
                              L.EndGroup t -> BoundaryItem $ LogTransformation.Iteration.EndGroup t
                          
                              L.StartTest d -> BoundaryItem $ LogTransformation.Iteration.StartTest d
                              L.EndTest tm -> BoundaryItem $ LogTransformation.Iteration.EndTest tm 
                          
                              L.StartIteration{} -> bug 
                              L.EndIteration{} -> bug
                          
                        logp@IterationLog{} -> LineError LogTransformError {
                                                linNo = lineNo,
                                                logItem = logp,
                                                info = ""
                                              }

    validPhaseStep :: (IterationAccum, Either LogTransformError (Maybe [IterationLogElement])) 
    validPhaseStep = let 
                      nextRec:: IterationRecord -> Maybe IterationRecord
                      nextRec thisRec =
                        updateIterationErrsWarnings lastPhase lp  
                        . apppendRaw lp <$>  
                              case lp of
                                BoundaryLog bl -> case bl of 
                                    L.StartRun{} -> Nothing
                                    L.EndRun -> Nothing
                                    L.FilterLog _ -> Nothing
                                    L.StartGroup _ -> Nothing
                                    L.EndGroup _ -> Nothing
                                    L.StartTest _ -> Nothing
                                    L.EndTest _ -> Nothing
                                    
                                    StartIteration{} -> newRec
                                    
                                    EndIteration _ -> Nothing -- special processing for end iteration elsewhere

                                IterationLog (Doc _) -> Nothing
                                IterationLog (Run rp) -> case rp of
                                  StartInteraction -> pure thisRec
                                  StartChecks -> pure thisRec
                                  StartPrepState -> pure thisRec
                
                                  IOAction m -> pure thisRec
                                  
                                  InteractorSuccess iid displayInfo -> pure $ thisRec {apState = Just $ SucceededInteractor displayInfo}
                                    
                                  InteractorFailure iid err -> pure $ thisRec {apState = Just $ FailedInteractor err}

                                  L.PrepStateSuccess iid dStateDisplayInfo -> pure $ thisRec {domainState = Just $ SucceededPrepState dStateDisplayInfo}
                                  PrepStateFailure iid err -> pure $ thisRec {domainState = Just $ FailedPrepState err}

                                  CheckOutcome iid cr@(CheckReport reslt (CheckInfo chkhdr mbInfo)) -> 
                                    pure $ thisRec {validation = P.snoc (validation thisRec) cr}
                                  
                                  Message _ -> pure thisRec
                                  Message' _ -> pure thisRec
                                  L.Warning _ -> pure thisRec
                                  Warning' detailedInfo -> pure thisRec
                                  L.Error _ -> pure thisRec
                      in 
                        (
                        IterationAccum {
                          phase = nextPhase lastPhase lp,
                          stageFailure = failStage lp,
                          rec = isStartIteration ? newRec $ mRec >>= nextRec 
                        }, Right $ 
                              maybef mRec
                                (isStartIteration ? Nothing $ Just [outOfIteration lp])
                                (\irec -> isEndIteration 
                                              ? Just [Iteration $ apppendRaw lp irec]
                                              $ Nothing
                                              )
                        )
  in 
    phaseChangeIsValid ? validPhaseStep $ invalidPhaseStep

$(deriveJSON defaultOptions ''IterationRecord)
$(deriveJSON defaultOptions ''ApStateInfo)
$(deriveJSON defaultOptions ''PrepStateInfo)
$(deriveJSON defaultOptions ''ItemInfo)
$(deriveJSON defaultOptions ''IterationWarning)
$(deriveJSON defaultOptions ''IterationSummary)
$(deriveJSON defaultOptions ''ExecutionStatus)
$(deriveJSON defaultOptions ''IterationError)
$(deriveJSON defaultOptions ''IterationPhase)
$(deriveJSON defaultOptions ''IterationLogElement)
$(deriveJSON defaultOptions ''IterationAuxEvent)
$(deriveJSON defaultOptions ''Issues)