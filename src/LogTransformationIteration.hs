module LogTransformationIteration where

import Common as C (AppError(..))
import LogTransformationCommon
import Check as CK
import Pyrelude as P
import Pyrelude.IO
import Data.DList as D
import qualified Prelude as PO
import AuxFiles
import OrphanedInstances
import DSL.LogProtocol as LP
import Text.Show.Pretty as PP
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import System.IO as S
import Data.Functor
import DSL.Logger
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Identity

--------------------------------------------------------
----------------- Iteration Aggregation ----------------
--------------------------------------------------------

data IterationPhase = OutOfIteration | 
                      PreInteractor | 
                      Interactor | 
                      PrePrepState |
                      PrepState |
                      PreChecks |
                      Checks 
                      deriving (Eq, Ord, Show)

data IterationResult = Inconclusive |
                       Pass |
                       Warning IterationPhase |
                       Fail IterationPhase 
                       deriving (Eq, Ord, Show)

isFailure :: IterationResult -> Bool
isFailure = \case 
              Inconclusive -> False
              LogTransformationIteration.Pass -> False
              LogTransformationIteration.Warning _ -> False
              LogTransformationIteration.Fail _ -> True

isWarning :: IterationResult -> Bool
isWarning = \case 
              Inconclusive -> False
              LogTransformationIteration.Pass -> False
              LogTransformationIteration.Warning _ -> True
              LogTransformationIteration.Fail _ -> False

data IterationSummary = IterationSummary {
                        iid :: ItemId,
                        pre :: WhenClause,
                        post:: ThenClause,
                        result :: IterationResult
                        } deriving (Eq, Show)

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

emptyAccum :: IterationAccum
emptyAccum = IterationAccum {
  phase = OutOfIteration,
  stageFailure = NoFailure,
  rec = Nothing
}


data TestIteration = Iteration IterationRecord |
                    OutOfIterationLog LogProtocol |
                    LineError LogTransformError 
                    deriving (Show, Eq)

updateErrsWarnings:: IterationPhase -> LogProtocol -> IterationRecord -> IterationRecord
updateErrsWarnings p lp ir = 
  let
    lpResult :: IterationResult 
    lpResult = case lp of 
      Message _ -> Inconclusive
      Message' _ -> Inconclusive

      (LP.Warning _) -> LogTransformationIteration.Warning p
      (Warning' _) -> LogTransformationIteration.Warning p
      (LP.Error _) -> LogTransformationIteration.Fail p

      StartRun{} -> Inconclusive
      EndRun -> Inconclusive
      FilterLog _ -> Inconclusive
      StartGroup _ -> Inconclusive
      EndGroup _ -> Inconclusive
      StartTest _ -> Inconclusive
      EndTest _ -> Inconclusive
      
      StartIteration {} -> Inconclusive
      EndIteration _ -> Inconclusive
      
      SubLog (Doc dp) -> LogTransformationIteration.Fail p
      SubLog (Run rp) -> case rp of
                              StartPrepState -> Inconclusive
                              IOAction _ -> Inconclusive

                              StartInteraction -> Inconclusive
                              InteractorSuccess {} -> LogTransformationIteration.Pass
                              InteractorFailure {} -> LogTransformationIteration.Fail p

                              LP.PrepStateSuccess {} -> LogTransformationIteration.Pass
                              PrepStateFailure {} -> LogTransformationIteration.Fail p

                              StartChecks{} -> Inconclusive
                              CheckOutcome _ (CheckReport reslt _) -> case classifyResult reslt of
                                                                        OK -> LogTransformationIteration.Pass
                                                                        CK.Error -> LogTransformationIteration.Fail p
                                                                        CK.Warning -> LogTransformationIteration.Warning p
                                                                        Skipped -> Inconclusive

    notCheckPhase :: Bool
    notCheckPhase = p /= Checks

    worstResult :: IterationResult
    worstResult = max lpResult $ LogTransformationIteration.result (summary ir)

  in 
    ir {
      summary = (summary ir) {result = worstResult}
      , otherErrorsDesc = isFailure lpResult && notCheckPhase 
                              ? IterationError p lp : otherErrorsDesc ir  
                              $ otherErrorsDesc ir

      , otherWarningsDesc = isWarning lpResult && notCheckPhase 
                              ? IterationWarning p lp : otherWarningsDesc ir 
                              $ otherWarningsDesc ir
    }

apppendRaw :: LogProtocol -> IterationRecord -> IterationRecord
apppendRaw lp ir = ir {rawLog = D.snoc (rawLog ir) lp} 

expectedCurrentPhase :: IterationPhase -> FailStage -> LogProtocol -> IterationPhase
expectedCurrentPhase current fs lp = case lp of
                                      StartRun{} -> OutOfIteration
                                      EndRun -> OutOfIteration
                                      Message _ -> current
                                      Message' _ -> current
                                      LP.Warning{} -> current
                                      Warning' _ -> current
                                      LP.Error _ -> current
                                      FilterLog _ -> OutOfIteration
                                      StartGroup _ -> OutOfIteration
                                      EndGroup _ -> OutOfIteration
                                      StartTest _ -> OutOfIteration
                                      EndTest _ -> OutOfIteration

                                      StartIteration{} -> OutOfIteration
                                      EndIteration _ -> case fs of 
                                                          NoFailure -> Checks  -- TODO: ensure raw file test with no checks
                                                          InteractorFailed -> Interactor
                                                          PrepStateFailed -> PrepState

                                      -- should never happen
                                      SubLog (Doc _) -> current
                                      SubLog (Run rp) -> case rp of
                                                            StartPrepState -> PrePrepState
                                                            IOAction _ -> current

                                                            StartInteraction -> PreInteractor
                                                            InteractorSuccess{} -> Interactor
                                                            InteractorFailure{}  -> Interactor

                                                            LP.PrepStateSuccess{}  -> PrepState
                                                            PrepStateFailure iid err -> PrepState

                                                            StartChecks{} -> PreChecks
                                                            CheckOutcome{} -> Checks

failStage :: LogProtocol -> FailStage
failStage = \case
                StartRun{} -> NoFailure
                EndRun -> NoFailure
                Message _ -> NoFailure
                Message' _ -> NoFailure
                LP.Warning{} -> NoFailure
                Warning' _ -> NoFailure
                LP.Error _ -> NoFailure
                FilterLog _ -> NoFailure
                StartGroup _ -> NoFailure
                EndGroup _ -> NoFailure
                StartTest _ -> NoFailure
                EndTest _ -> NoFailure

                StartIteration{} -> NoFailure
                EndIteration _ -> NoFailure

                -- should never happen
                SubLog (Doc _) -> NoFailure
                SubLog (Run rp) -> case rp of
                                      StartPrepState -> NoFailure
                                      IOAction _ -> NoFailure
                                      StartInteraction -> NoFailure
                                      InteractorSuccess{} -> NoFailure
                                      InteractorFailure{}  -> InteractorFailed
                                      LP.PrepStateSuccess{}  -> NoFailure
                                      PrepStateFailure iid err -> PrepStateFailed
                                      StartChecks{} -> NoFailure
                                      CheckOutcome{} -> NoFailure

nextPhase :: IterationPhase -> LogProtocol -> IterationPhase
nextPhase current lp = case lp of
                          StartRun{} -> OutOfIteration
                          EndRun -> OutOfIteration
                          Message _ -> current
                          Message' _ -> current
                          LP.Warning s -> current
                          Warning' detailedInfo -> current
                          LP.Error _ -> current
                          FilterLog _ -> OutOfIteration
                          StartGroup _ -> OutOfIteration
                          EndGroup _ -> OutOfIteration
                          StartTest _ -> OutOfIteration
                          EndTest _ -> OutOfIteration
                          StartIteration{} -> PreInteractor
                          EndIteration _ -> OutOfIteration

                          -- should never happen
                          SubLog (Doc _) -> current
                          
                          SubLog (Run rp) -> case rp of
                                                StartPrepState -> PrepState
                                                IOAction _ -> current
                                                StartInteraction -> Interactor
                                                InteractorSuccess{} -> PrepState 
                                                InteractorFailure{}  -> PrepState
                                                LP.PrepStateSuccess{}  -> PreChecks
                                                PrepStateFailure iid err -> PreChecks
                                                StartChecks{} -> Checks
                                                CheckOutcome{}  -> Checks

serialiseIteration :: TestIteration -> ByteString
serialiseIteration = L.toStrict . A.encode

iterationStep ::
              LineNo                                                                -- lineNo
              -> IterationAccum                                                     -- accum
              -> LogProtocol                                                        -- parse error or apperror
              -> (IterationAccum, Either LogTransformError (Maybe [TestIteration])) -- (newAccum, err / result)
iterationStep lineNo accum@(IterationAccum thisPhase stageFailure mRec) lp = 
  let
    isStartIteration :: Bool 
    isStartIteration = case lp of
                          StartIteration{} -> True
                          _ -> False

    isEndIteration ::  Bool 
    isEndIteration = case lp of 
                        EndIteration _ -> True
                        _ -> False

    -- this is the wrong kind of sublog
    -- should never happen                                                 
    isDocLog ::  Bool
    isDocLog = case lp of
                  SubLog (Doc _) -> True
                  _ -> False

    phaseChangeIsValid :: Bool
    phaseChangeIsValid = expectedCurrentPhase thisPhase stageFailure lp == thisPhase 
                          &&  (thisPhase == OutOfIteration) == isNothing mRec
                          && not isDocLog

    invalidPhaseStep :: (IterationAccum, Either LogTransformError (Maybe [TestIteration])) 
    invalidPhaseStep =
      let 
        nextAccum :: IterationAccum
        nextAccum = IterationAccum {
          phase = OutOfIteration,         
          stageFailure = NoFailure,
          rec = Nothing
        } 

        err :: TestIteration
        err = LineError LogTransformError {
                            linNo = lineNo,
                            logItem = lp,
                            info = isDocLog 
                                      ? "A documentation log has been encountered during a run - this type of log should not be generated during a run"
                                      $ "Unexpected log message encounterred - Messages have either been lost or received out of order.\n"
                                          <> "Test and Interation summaries may not reflect true results of the test"
                          } 
      in
        (nextAccum, Right . Just $ maybef mRec 
                                    [err] 
                                    (\r -> [Iteration r , err]) -- if the record exists close it off add add a error record after
        )

    -- TODO: Add raw log - special processing for end iteration
    -- UpdateErrors
    -- phase change
    -- if nothing result is singleton - OutOfIterationLog
    -- subLog Doc log error
    validPhaseStep :: (IterationAccum, Either LogTransformError (Maybe [TestIteration])) 
    validPhaseStep = let 
                      nextRec:: LogProtocol -> IterationRecord -> Maybe IterationRecord
                      nextRec lgp thisRec =
                        updateErrsWarnings thisPhase lgp  
                        . apppendRaw lgp <$>  
                              case lgp of
                                Message _ -> pure thisRec
                                Message' _ -> pure thisRec
                                LP.Warning _ -> pure thisRec
                                Warning' detailedInfo -> pure thisRec
                                LP.Error _ -> pure thisRec

                                StartRun{} -> Nothing
                                EndRun -> Nothing
                                FilterLog _ -> Nothing
                                StartGroup _ -> Nothing
                                EndGroup _ -> Nothing
                                StartTest _ -> Nothing
                                EndTest _ -> Nothing
                                
                                StartIteration iid pre post val -> pure $ IterationRecord {
                                                                                            summary = IterationSummary {
                                                                                              iid = iid,
                                                                                              pre = pre,
                                                                                              post = post,
                                                                                              result = Inconclusive
                                                                                            },
                                                                                            validation = [],
                                                                                            otherErrorsDesc = [],
                                                                                            otherWarningsDesc = [],
                                                                                            item = Just $ ItemInfo iid pre post val,
                                                                                            apState = Nothing,
                                                                                            domainState = Nothing,
                                                                                            rawLog = D.empty
                                                                                          }
                                
                                EndIteration _ -> Nothing -- note special processing for end iteration

                                SubLog (Doc _) -> Nothing

                                SubLog (Run rp) -> case rp of
                                  StartInteraction -> pure thisRec
                                  StartChecks -> pure thisRec
                                  StartPrepState -> pure thisRec
                
                                  IOAction m -> pure thisRec
                                  
                                  InteractorSuccess iid displayInfo -> pure $ thisRec {apState = Just $ SucceededInteractor displayInfo}
                                    
                                  InteractorFailure iid err -> pure $ thisRec {apState = Just $ FailedInteractor err}

                                  LP.PrepStateSuccess iid dStateDisplayInfo -> pure $ thisRec {domainState = Just $ SucceededPrepState dStateDisplayInfo}
                                  PrepStateFailure iid err -> pure $ thisRec {domainState = Just $ FailedPrepState err}

                                  CheckOutcome iid cr@(CheckReport reslt (CheckInfo chkhdr mbInfo)) -> 
                                    pure $ thisRec {validation = P.snoc (validation thisRec) cr}
                      in 
                        (
                        IterationAccum {
                          phase = nextPhase thisPhase lp,
                          stageFailure = failStage lp,
                          rec = mRec >>= nextRec lp
                        }, Right $ 
                              maybef mRec
                                (Just [OutOfIterationLog lp])
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
$(deriveJSON defaultOptions ''IterationResult)
$(deriveJSON defaultOptions ''IterationError)
$(deriveJSON defaultOptions ''IterationPhase)
$(deriveJSON defaultOptions ''TestIteration)