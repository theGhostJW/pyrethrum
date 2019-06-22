module LogTransformation.PrintLogDisplayElement (
  emptyIterationAccum,
  printLogDisplayStep,
  prettyPrintDisplayElement,
  IterationAccum(..),
  PrintLogDisplayElement(..),
  IterationRecord(..),
  LogTransformError(..),
  IterationSummary(..),
  IterationError(..),
  IterationWarning(..),
  ApStateInfo(..),
  PrepStateInfo(..)
) where

import Common as C (AppError(..), DetailedInfo(..))
import LogTransformation.Common as LC
import Check as CK
import Pyrelude as P
import Data.DList as D
import qualified DSL.LogProtocol as LP
import DSL.LogProtocol hiding (StartRun)
import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import RunElementClasses
import LogTransformation.Stats
import PrettyPrintCommon
import Data.Yaml.Pretty as YP
import qualified Data.Map as M

-- TODO: creation relational records
-- relational records from Iteration records and use reporting service
-- to provide full report - use sql lite locally 
-- see https://www.oreilly.com/library/view/microservices-antipatterns-and/9781492042716/ch04.html


--------------------------------------------------------
----------------- Iteration Aggregation ----------------
--------------------------------------------------------

data PrintLogDisplayElement =
  FilterLog [FilterResult] |

  StartRun {  
        title :: RunTitle, 
        config :: A.Value, 
        runStatus :: ExecutionStatus,
        testStats :: StatusCount, 
        iterationStats :: StatusCount,
        outOfTest :: StatusCount
    } | 
  EndRun |

  StartGroup GroupTitle |
  EndGroup GroupTitle |

  StartTest TestDisplayInfo |
  EndTest TestModule |
  Iteration IterationRecord |
  LineError LogTransformError |
  NotImplemented -- delete later
  deriving (Show, Eq)

data IterationSummary = IterationSummary {
                          iid :: ItemId,
                          pre :: Text,
                          post:: Text,
                          status :: ExecutionStatus
                        } deriving (Eq, Show)

data IterationError = IterationError {
    phase :: IterationPhase,
    error :: LogProtocol
  } deriving (Eq, Show)
                          
data IterationWarning = IterationWarning {
    phase :: IterationPhase,
    warning :: LogProtocol
  } deriving (Eq, Show)

data ApStateInfo = SucceededInteractor ApStateDisplay | 
                   FailedInteractor AppError
                   deriving (Eq, Show)

data PrepStateInfo = SucceededPrepState DStateDisplay |
                     FailedPrepState AppError
                     deriving (Eq, Show)

data IterationRecord = IterationRecord {
  summary :: IterationSummary,
  validation :: [CheckReport],
  otherErrors :: [IterationError],
  otherWarnings :: [IterationWarning],
  item :: Maybe A.Value,
  apState :: Maybe ApStateInfo,
  domainState :: Maybe PrepStateInfo,
  reverseLog :: [LogProtocol]
} deriving (Eq, Show)

data IterationAccum = IterationAccum {
  phase :: IterationPhase,
  stageFailure :: Maybe IterationPhase,
  rec :: Maybe IterationRecord,
  filterLog :: Maybe [FilterResult]
} deriving (Eq, Show)

emptyIterationAccum :: IterationAccum
emptyIterationAccum = IterationAccum {
  phase = OutOfIteration,
  stageFailure = Nothing,
  rec = Nothing,
  filterLog = Nothing
}

printLogDisplayStep ::
              RunResults                                                            -- RunResults
              -> LineNo                                                             -- lineNo
              -> IterationAccum                                                     -- accum
              -> Either DeserialisationError LogProtocol                                                        -- parse error or apperror
              -> (IterationAccum, Maybe [PrintLogDisplayElement]) -- (newAccum, err / result)
printLogDisplayStep runResults lineNo accum@(IterationAccum lastPhase stageFailure mRec mFltrLg) eithLp = 
  
  eitherf eithLp
   (\err -> (
              accum {stageFailure = calcNextIterationFailStage stageFailure LC.Fail lastPhase}, 
              Just [LineError $ LogDeserialisationError err]
              )
    )

   (\lp ->
     let 
        noImp = (accum, Nothing)

        RunResults outOfTest iterationResults = runResults

        elOut  :: a -> Maybe [a]
        elOut a = Just [a]

        nxtWithoutPhaseErrorOrPhase :: (IterationAccum, Maybe [PrintLogDisplayElement]) 
        nxtWithoutPhaseErrorOrPhase@(nxtAccum, mbePrntElms) = 
          case lp of
              BoundaryLog bl-> 
                case bl of
                  LP.FilterLog flgs -> (accum {filterLog = Just flgs}, Nothing) 

                  LP.StartRun runTitle jsonCfg -> (accum, elOut $ StartRun {  
                    title = runTitle, 
                    config = jsonCfg, 
                    runStatus = worstStatus runResults,
                    testStats = testStatusCounts runResults, 
                    iterationStats = iterationStatusCounts runResults,
                    outOfTest = outOfTest
                  } )
                  LP.EndRun -> noImp
              
                  LP.StartGroup gt -> LogTransformation.PrintLogDisplayElement.StartGroup gt
                  LP.EndGroup (GroupTitle txt') -> noImp
              
                  LP.StartTest TestDisplayInfo{} -> noImp
                  LP.EndTest TestModule{} -> noImp
              
                  StartIteration (ItemId tstModule itmId) (WhenClause whn) (ThenClause thn) jsonVal -> noImp
                  EndIteration (ItemId tstModule itmId) -> noImp
              
              IterationLog subProtocol -> 
                case subProtocol of
                  Doc _ -> noImp
                  Run action -> 
                    case action of
                      IOAction txt' -> noImp
                      StartPrepState -> noImp
                      StartInteraction -> noImp
                      InteractorSuccess{} -> noImp
                      InteractorFailure{} -> noImp
                    
                      PrepStateSuccess{} -> noImp
                      PrepStateFailure{} -> noImp
                      StartChecks -> noImp 
                      CheckOutcome itmId (CheckReport rslt mInfo) -> noImp
      
                      Message txt' -> noImp
                      Message' (DetailedInfo msg txt') -> noImp
                    
                      LP.Warning txt' -> noImp
                      Warning' (DetailedInfo msg txt') -> noImp
      
                      LP.Error err -> noImp
      in 
        nxtWithoutPhaseErrorOrPhase
      )

prettyPrintDisplayElement :: PrintLogDisplayElement -> Text
prettyPrintDisplayElement pde = 
  let 
    noImp = txtPretty pde
    newLn2 = newLn <> newLn
  in 
    case pde of
      LogTransformation.PrintLogDisplayElement.FilterLog arrFr -> noImp

      LogTransformation.PrintLogDisplayElement.StartRun (RunTitle titl) config runStatus testStats iterationStats outOfTest -> 
            majorHeader (titl <> " - " <> toTitle (statusLabel False runStatus) <> " - 00:00:88") 
                <> newLn
                <> "toDo - start end duration raw file" 
                <> newLn2 
                <> "runConfig: " 
                <> newLn 
                <> prettyYamlKeyValues 2 LeftJustify config
                <> newLn
                <> "test stats:" 
                <> newLn
                <> alignKeyValues 2 RightJustify (statusCountTupleText False testStats)
                <> newLn
                <> "iteration stats:" 
                <> newLn
                <> alignKeyValues 2 RightJustify (statusCountTupleText False iterationStats)
                <> newLn
                <> "out of test issues:" 
                <> newLn
                <> alignKeyValues 2 RightJustify (statusCountTupleText True outOfTest)


      LogTransformation.PrintLogDisplayElement.EndRun -> noImp

      LogTransformation.PrintLogDisplayElement.StartGroup (GroupTitle titl)-> noImp
      LogTransformation.PrintLogDisplayElement.EndGroup (GroupTitle titl) -> noImp

      LogTransformation.PrintLogDisplayElement.StartTest TestDisplayInfo{} -> noImp
      LogTransformation.PrintLogDisplayElement.EndTest TestModule{} -> noImp
      LogTransformation.PrintLogDisplayElement.Iteration IterationRecord{} -> noImp
      LineError err -> noImp
      NotImplemented -> noImp

statusLabel :: Bool -> ExecutionStatus -> Text
statusLabel outOfTest = \case
                          LC.Pass -> "pass"
                          LC.Fail -> outOfTest ? "error" $ "fail"
                          LC.Regression -> "regression"
                          Type2Error -> "type 2 error"
                          LC.Warning -> "warning"
                          KnownError -> "known error"

statusCountTupleText :: Bool -> StatusCount -> [(Text, Text)]
statusCountTupleText outOfTest sc = 
  let
    chkOnlyStatuses :: [ExecutionStatus]
    chkOnlyStatuses = [KnownError, Type2Error, LC.Regression, LC.Pass]

    defList :: [ExecutionStatus]
    defList = outOfTest ? enumList \\ chkOnlyStatuses $ enumList
    
    defaults :: StatusCount
    defaults = M.fromList $ (\es -> (es, 0)) <$> defList

    displayOrder :: (ExecutionStatus, a) -> (ExecutionStatus, a) -> Ordering
    displayOrder (es1, _) (es2, _) =
      let 
        exOrd :: ExecutionStatus -> Int
        exOrd  = \case
                    LC.Pass -> 0
                    LC.Fail -> 1
                    LC.Regression -> 2
                    Type2Error -> 3
                    KnownError -> 4
                    LC.Warning -> 5
      in 
        compare (exOrd es1) (exOrd es2)

    statusTuples :: [(Text, Text)]
    statusTuples = bimap ((<> ":") . statusLabel outOfTest) txt <$> sortBy displayOrder (M.toList (M.union sc defaults))
  in
    outOfTest 
        ? statusTuples 
        $ ("total:", txt . sum $ M.elems sc) : statusTuples 

-- data ExecutionStatus = Inconclusive |
--                        Pass |
--                        Warning IterationPhase |
--                        Fail IterationPhase 
--                        deriving (Eq, Show)
                      
-- instance Ord ExecutionStatus where
--      (<=) :: ExecutionStatus -> ExecutionStatus -> Bool 
--      Inconclusive <= _ = True

--      LogTransformation.Iteration.Pass <= Inconclusive = False
--      LogTransformation.Iteration.Pass <= LogTransformation.Iteration.Pass = True
--      LogTransformation.Iteration.Pass <= LogTransformation.Iteration.Warning _ = True
--      LogTransformation.Iteration.Pass <= LogTransformation.Iteration.Fail _ = True

--      LogTransformation.Iteration.Warning _ <= Inconclusive = False
--      LogTransformation.Iteration.Warning _ <= LogTransformation.Iteration.Pass = True
--      LogTransformation.Iteration.Warning p0 <= LogTransformation.Iteration.Warning p1 = p0 > p1 -- if phase is greater then warning is smaller (favour earlier warnings)
--      LogTransformation.Iteration.Warning _ <= LogTransformation.Iteration.Fail _ = True

--      LogTransformation.Iteration.Fail _ <= Inconclusive = False
--      LogTransformation.Iteration.Fail _ <= LogTransformation.Iteration.Pass = False
--      LogTransformation.Iteration.Fail _ <= LogTransformation.Iteration.Warning _ = False
--      LogTransformation.Iteration.Fail p0 <= LogTransformation.Iteration.Fail p1 = p0 > p1 -- if phase is greater then warning is smaller (favour earlier failures)

-- isFailure :: ExecutionStatus -> Bool
-- isFailure = \case 
--               Inconclusive -> False
--               LogTransformation.Iteration.Pass -> False
--               LogTransformation.Iteration.Warning _ -> False
--               LogTransformation.Iteration.Fail _ -> True

-- isWarning :: ExecutionStatus -> Bool
-- isWarning = \case 
--               Inconclusive -> False
--               LogTransformation.Iteration.Pass -> False
--               LogTransformation.Iteration.Warning _ -> True
--               LogTransformation.Iteration.Fail _ -> False

-- calcStatus :: Issues -> (IterationPhase -> ExecutionStatus)
-- calcStatus issues  
--    | type2Failure issues > 0 = LogTransformation.Iteration.Fail 
--    | LogTransformation.Iteration.fail issues > 0 = LogTransformation.Iteration.Fail 
--    | regression issues > 0 = LogTransformation.Iteration.Fail 

--    | (LogTransformation.Iteration.warning :: Issues -> Int) issues > 0 = LogTransformation.Iteration.Warning
--    | expectedFailure issues > 0 = LogTransformation.Iteration.Warning

--    | otherwise = const LogTransformation.Iteration.Pass



-- data Issues = Issues {
--   expectedFailure :: Int,
--   warning :: Int,
--   type2Failure :: Int,
--   fail :: Int,
--   regression :: Int
-- }  deriving (Show, Eq)

-- instance Semigroup Issues where 
--   s0 <> s1 = 
--     let 
--       plus :: (Issues -> Int) -> Int
--       plus f = f s0 + f s1 
--     in 
--       Issues {
--                 warning = plus warning,
--                 expectedFailure = plus expectedFailure,
--                 type2Failure = plus type2Failure,
--                 fail = plus LogTransformation.Iteration.fail,
--                 regression = plus regression
--               } 

-- instance Monoid Issues where 
--   mempty = Issues 0 0 0 0 0

-- data IterationError = IterationError {
--     phase :: IterationPhase,
--     error :: LogProtocol
--   } deriving (Eq, Show)
                          
-- data IterationWarning = IterationWarning {
--     phase :: IterationPhase,
--     warning :: LogProtocol
--   } deriving (Eq, Show)
                                        
-- data ItemInfo = ItemInfo ItemId WhenClause ThenClause A.Value deriving (Eq, Show)

-- data ApStateInfo = SucceededInteractor ApStateDisplay | 
--                    FailedInteractor AppError
--                    deriving (Eq, Show)

-- data PrepStateInfo = SucceededPrepState DStateDisplay |
--                      FailedPrepState AppError
--                      deriving (Eq, Show)

-- data FailStage = InteractorFailed |
--                  PrepStateFailed |
--                  NoFailure
--                  deriving (Eq, Show)
     
-- updateIterationErrsWarnings:: IterationPhase -> LogProtocol -> IterationRecord -> IterationRecord
-- updateIterationErrsWarnings p lp iRec = 
--   let
--     updatedIssues :: Issues
--     updatedIssues = updateIssues . issues $ summary iRec

--     worstStatus :: ExecutionStatus
--     worstStatus = calcStatus updatedIssues p

--     stepStatus :: ExecutionStatus
--     stepStatus = calcStatus (updateIssues mempty) p

--     updateIssues :: Issues -> Issues
--     updateIssues oldIssues = 
--       let 
--         modifier :: Issues -> Issues
--         modifier = 
--           -- good motivating case for lens
--           let 
--             incFailure :: Issues -> Issues
--             incFailure s = s {LogTransformation.Iteration.fail = LogTransformation.Iteration.fail s + 1}
          
--             incExpectedFailure :: Issues -> Issues
--             incExpectedFailure s = s {LogTransformation.Iteration.expectedFailure = LogTransformation.Iteration.expectedFailure s + 1}
          
--             incRegresssion :: Issues -> Issues
--             incRegresssion s = s {LogTransformation.Iteration.regression = LogTransformation.Iteration.regression s + 1}

--             incWarning :: Issues -> Issues
--             incWarning s = s {LogTransformation.Iteration.warning = (LogTransformation.Iteration.warning :: Issues -> Int) s + 1 }
--           in
--             case lp of
--               BoundaryLog bl -> id  -- this should not happen and will cause a phase error to be logged
--               IterationLog (Doc dp) -> id -- this should not happen and will cause a phase error to be logged
--               IterationLog (Run rp) -> 
--                 case rp of
--                   StartPrepState -> id
--                   IOAction _ -> id

--                   StartInteraction -> id
--                   InteractorSuccess {} -> id 
--                   InteractorFailure {} -> incFailure

--                   LP,PrepStateSuccess {} -> id
--                   PrepStateFailure {} -> incFailure

--                   StartChecks{} -> id
--                   CheckOutcome _ (CheckReport reslt _) -> 
--                     case reslt of
--                       CK.Pass -> id
--                       CK.Fail -> incFailure
--                       GateFail -> incFailure
--                       FailExpected _ -> incExpectedFailure
--                       GateFailExpected _ -> incExpectedFailure
--                       PassWhenFailExpected _ -> 
--                         \s -> s {LogTransformation.Iteration.type2Failure = LogTransformation.Iteration.type2Failure s + 1}
--                       Regression _ -> incRegresssion
--                       GateRegression _ -> incRegresssion
--                       Skip -> id

--                   Message _ -> id
--                   Message' _ -> id
                                          
--                   LP,Warning _ -> incWarning
--                   Warning' _ -> incWarning
--                   LP,Error _ -> incFailure
--         in 
--           modifier oldIssues

--     notCheckPhase :: Bool
--     notCheckPhase = p /= Checks

--   in 
--     iRec {
--       summary = (summary iRec) {
--                                 status = worstStatus,
--                                 issues = updatedIssues
--                                }

--       , otherErrorsDesc = isFailure stepStatus && notCheckPhase 
--                               ? IterationError p lp : otherErrorsDesc iRec  
--                               $ otherErrorsDesc iRec

--       , otherWarningsDesc = isWarning stepStatus && notCheckPhase 
--                               ? IterationWarning p lp : otherWarningsDesc iRec 
--                               $ otherWarningsDesc iRec
--     }

-- apppendRaw :: LogProtocol -> IterationRecord -> IterationRecord
-- apppendRaw lp iRec = iRec {rawLog = D.snoc (rawLog iRec) lp} 

-- failStage :: LogProtocol -> FailStage
-- failStage = \case
--                 BoundaryLog bl -> case bl of 
--                                       LP,StartRun{} -> NoFailure
--                                       LP,EndRun -> NoFailure
                                      
--                                       LP,FilterLog _ -> NoFailure
--                                       LP,StartGroup _ -> NoFailure
--                                       LP,EndGroup _ -> NoFailure
--                                       LP,StartTest _ -> NoFailure
--                                       LP,EndTest _ -> NoFailure

--                                       LP,StartIteration{} -> NoFailure
--                                       LP,EndIteration _ -> NoFailure

--                 -- should never happen
--                 IterationLog (Doc _) -> NoFailure
--                 IterationLog (Run rp) -> case rp of
--                                       StartPrepState -> NoFailure
--                                       IOAction _ -> NoFailure
--                                       StartInteraction -> NoFailure
--                                       InteractorSuccess{} -> NoFailure
--                                       InteractorFailure{}  -> InteractorFailed
--                                       LP,PrepStateSuccess{}  -> NoFailure
--                                       PrepStateFailure iid err -> PrepStateFailed
--                                       StartChecks{} -> NoFailure
--                                       CheckOutcome{} -> NoFailure
--                                       Message _ -> NoFailure
--                                       Message' _ -> NoFailure
--                                       LP,Warning{} -> NoFailure
--                                       Warning' _ -> NoFailure
--                                       LP,Error _ -> NoFailure

-- expectedCurrentPhase :: IterationPhase -> FailStage -> LogProtocol -> IterationPhase
-- expectedCurrentPhase current fs lp = case lp of
--                                       BoundaryLog bl -> case bl of 
--                                                             LP,StartRun{} -> OutOfIteration
--                                                             LP,EndRun -> OutOfIteration
                                                            
--                                                             LP,FilterLog _ -> OutOfIteration
--                                                             LP,StartGroup _ -> OutOfIteration
--                                                             LP,EndGroup _ -> OutOfIteration
--                                                             LP,StartTest _ -> OutOfIteration
--                                                             LP,EndTest _ -> OutOfIteration

--                                                             LP,StartIteration{} -> OutOfIteration
--                                                             LP,EndIteration _ -> case fs of 
--                                                                                 NoFailure -> Checks  -- TODO: ensure raw file test with no checks
--                                                                                 InteractorFailed -> Interactor
--                                                                                 PrepStateFailed -> PrepState

--                                       -- should never happen
--                                       IterationLog (Doc _) -> current
--                                       IterationLog (Run rp) -> case rp of
--                                                             StartPrepState -> PrePrepState
--                                                             IOAction _ -> current

--                                                             StartInteraction -> PreInteractor
--                                                             InteractorSuccess{} -> Interactor
--                                                             InteractorFailure{}  -> Interactor

--                                                             LP,PrepStateSuccess{}  -> PrepState
--                                                             PrepStateFailure iid err -> PrepState

--                                                             StartChecks{} -> PreChecks
--                                                             CheckOutcome{} -> Checks
--                                                             Message _ -> current
--                                                             Message' _ -> current
--                                                             LP,Warning{} -> current
--                                                             Warning' _ -> current
--                                                             LP,Error _ -> current

-- nextPhase :: IterationPhase -> LogProtocol -> IterationPhase
-- nextPhase current lp = case lp of
--                           BoundaryLog bl -> case bl of 
--                                               LP,StartRun{} -> OutOfIteration
--                                               LP,EndRun -> OutOfIteration

--                                               LP,FilterLog _ -> OutOfIteration
--                                               LP,StartGroup _ -> OutOfIteration
--                                               LP,EndGroup _ -> OutOfIteration
--                                               LP,StartTest _ -> OutOfIteration
--                                               LP,EndTest _ -> OutOfIteration
--                                               LP,StartIteration{} -> PreInteractor
--                                               LP,EndIteration _ -> OutOfIteration

--                                               -- should never happen
--                           IterationLog (Doc _) -> current
                          
--                           IterationLog (Run rp) -> case rp of
--                                                 LP,Error _ -> current
--                                                 StartPrepState -> PrepState
--                                                 IOAction _ -> current
--                                                 StartInteraction -> Interactor
--                                                 InteractorSuccess{} -> PrePrepState 
--                                                 InteractorFailure{}  -> Interactor  -- leave in failed phase
--                                                 LP,PrepStateSuccess{}  -> PreChecks
--                                                 PrepStateFailure iid err -> PrepState -- leave in failed phase
--                                                 StartChecks{} -> Checks
--                                                 Message _ -> current
--                                                 Message' _ -> current
--                                                 LP,Warning s -> current
--                                                 Warning' detailedInfo -> current
--                                                 CheckOutcome{}  -> Checks

$(deriveJSON defaultOptions ''PrintLogDisplayElement)
$(deriveJSON defaultOptions ''IterationRecord)
$(deriveJSON defaultOptions ''ApStateInfo)
-- $(deriveJSON defaultOptions ''ItemInfo)
$(deriveJSON defaultOptions ''IterationWarning)
$(deriveJSON defaultOptions ''IterationSummary)

$(deriveJSON defaultOptions ''IterationError)

$(deriveJSON defaultOptions ''PrepStateInfo)
-- $(deriveJSON defaultOptions ''IterationAuxEvent)
-- $(deriveJSON defaultOptions ''Issues)