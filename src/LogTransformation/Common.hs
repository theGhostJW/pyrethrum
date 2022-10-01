module LogTransformation.Common where

import qualified Check as CK
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import DSL.LogProtocol as LP
import DSL.LogProtocol.PrettyPrint
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.DList as D
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Yaml as Y
import OrphanedInstances ()
import Pyrelude as P hiding (phase)
import RunElementClasses as RC
import RunnerBase

newtype LineNo = LineNo {unLineNo :: Int} deriving (Show, Eq)

$(deriveJSON defaultOptions ''LineNo)

data DeserialisationError = DeserialisationError
  { linNo :: LineNo,
    errorTxt :: Text,
    line :: Either UnicodeException Text -- the type for decode UTF8
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''DeserialisationError)

data LogTransformError
  = LogDeserialisationError DeserialisationError
  | LogIOError
      { message :: Text,
        error :: IOError
      }
  | LogTransformError
      { linNo :: LineNo,
        logItem :: LogProtocolOut,
        info :: Text
      }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LogTransformError)



-- Raw Data Types - no totalling

data ExecutionStatus
  = Pass
  | KnownError
  | Warning
  | Type2Error
  | Fail
  | Regression
  deriving (Show, Eq, Ord, Enum)
$(deriveJSON defaultOptions ''ExecutionStatus)

instance A.ToJSONKey ExecutionStatus

-- default implementation

instance A.FromJSONKey ExecutionStatus

-- default implementation

isBoundaryLog :: LogProtocolBase e -> Bool
isBoundaryLog =
  \case
    FilterLog _ -> True
    StartRun {} -> True
    EndRun -> True
    StaXTGroup _ -> True
    EndGroup _ -> True
    StartTest _ -> True
    EndTest _ -> True
    StartHook {} -> True
    EndHook {} -> True
    StartIteration {} -> True
    EndIteration _ -> True
    IOAction _ -> False
    IOAction' _ -> False
    StartInteraction -> False
    InteractorSuccess {} -> False
    InteractorFailure {} -> False
    StartParser -> False
    ParserSuccess {} -> False
    ParserSkipped _ -> False
    ParserFailure {} -> False
    StartChecks -> False
    CheckOutcome {} -> False
    Message _ -> False
    Message' _ -> False
    LP.Warning _ -> False
    Warning' _ -> False
    Error _ -> False

calcNextIterationFailStage :: Maybe IterationPhase -> ExecutionStatus -> IterationPhase -> Maybe (LogProtocolBase e) -> Maybe IterationPhase
calcNextIterationFailStage mCurrentFailPhase lgStatus currPhase mLp =
  currPhase == OutOfIteration || maybe False isBoundaryLog mLp
    ? Nothing
    $ lgStatus > LogTransformation.Common.Warning
      ? maybef
        mCurrentFailPhase
        (Just currPhase)
        (\fs -> Just $ max fs currPhase)
      $ mCurrentFailPhase

logProtocolStatus :: Bool -> LogProtocolBase e -> ExecutionStatus
logProtocolStatus chkEncountered = \case
  FilterLog _ -> Pass
  StartRun {} -> Pass
  EndRun -> Pass
  StaXTGroup _ -> Pass
  EndGroup _ -> Pass
  StartHook {} -> Pass
  EndHook {} -> Pass
  StartTest _ -> Pass
  EndTest _ -> Pass
  StartIteration {} -> Pass
  -- a test with 0 checks is deemed a fail
  EndIteration {} -> chkEncountered ? Pass $ Fail
  StartParser -> Pass
  IOAction _ -> Pass
  IOAction' _ -> Pass
  StartInteraction -> Pass
  InteractorSuccess {} -> Pass
  InteractorFailure {} -> Fail
  ParserSuccess {} -> Pass
  ParserSkipped {} -> Pass
  ParserFailure {} -> Fail
  StartChecks {} -> Pass
  CheckOutcome _ (CK.CheckReport reslt _) ->
    case reslt of
      CK.Pass -> Pass
      CK.Fail -> Fail
      CK.GateFail -> Fail
      CK.FailExpected _ -> KnownError
      CK.GateFailExpected _ -> KnownError
      CK.PassWhenFailExpected _ -> Type2Error
      CK.Regression _ -> Regression
      CK.GateRegression _ -> Regression
      CK.Skip -> Pass
  Message _ -> Pass
  Message' _ -> Pass
  LP.Warning _ -> LogTransformation.Common.Warning
  Warning' _ -> LogTransformation.Common.Warning
  Error _ -> Fail

-- order is backward so ealier phase is deemed
-- larger than a later one
data IterationPhase
  = OutOfIteration
  | Checks
  | PreChecks
  | Parse
  | PreParse
  | Interactor
  | PreInteractor
  deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''IterationPhase)

data IterationOutcome = IterationOutcome
  { executionStatus :: ExecutionStatus,
    iterationPhase :: IterationPhase
  }
  deriving (Eq, Ord, Show)
  
$(deriveJSON defaultOptions ''IterationOutcome)

type IterationResults = M.Map ItemId IterationOutcome

type StatusCount = M.Map ExecutionStatus Int

data RunResults = RunResults
  { outOfTest :: StatusCount,
    iterationResults :: IterationResults
  }
  deriving (Show)

$(deriveJSON defaultOptions ''RunResults)

data PhaseSwitch = PhaseSwitch
  { legalFromPhases :: S.Set IterationPhase,
    to :: IterationPhase
  }

isEndIteration :: LogProtocolBase e -> Bool
isEndIteration = \case
  EndIteration _ -> True
  _ -> False

-- calculate expected from / to base on log message
phaseSwitch :: LogProtocolOut -> Maybe IterationPhase -> Maybe PhaseSwitch
phaseSwitch LogProtocolOut {logInfo = lp} mFailedPhase =
  let ps :: IterationPhase -> IterationPhase -> Maybe PhaseSwitch
      ps cur nxt = Just $ PhaseSwitch (S.singleton cur) nxt

      outToOut :: Maybe PhaseSwitch
      outToOut = ps OutOfIteration OutOfIteration
   in case lp of
        StartRun {} -> outToOut
        EndRun -> outToOut
        FilterLog _ -> outToOut
        StaXTGroup _ -> outToOut
        EndGroup _ -> outToOut
        StartHook {} -> outToOut
        EndHook {} -> outToOut
        StartTest _ -> outToOut
        -- if test is empty state will be OutOfIteration else Checks
        EndTest _ -> Just $ PhaseSwitch (S.fromList [Checks, OutOfIteration]) OutOfIteration
        -- first iteration will be OutOfIteration else Checks
        StartIteration {} -> Just $ PhaseSwitch (S.fromList [Checks, OutOfIteration]) PreInteractor
        -- checks -> checks checks is the last status of an iteration
        -- need to leave in checks until after EndIteration because need to be in iteration when
        -- processing final EndIteration
        EndIteration _ -> Nothing
        LP.Error _ -> Nothing
        StartParser -> ps PreParse Parse
        IOAction _ -> Nothing
        IOAction' _ -> Nothing
        StartInteraction -> ps PreInteractor Interactor
        InteractorSuccess {} -> ps Interactor PreParse
        InteractorFailure {} -> Nothing -- keep in failed stage
        ParserSuccess {} -> ps Parse PreChecks
        ParserSkipped {} -> ps Interactor PreChecks
        ParserFailure {} -> Nothing -- keep in failed phase
        StartChecks {} -> ps (fromMaybe PreChecks mFailedPhase) Checks
        Message _ -> Nothing
        Message' _ -> Nothing
        LP.Warning _ -> Nothing
        Warning' _ -> Nothing
        CheckOutcome {} -> Nothing -- stay in checks

data PhaseChangeValidation = PhaseChangeValidation
  { fromPhase :: IterationPhase,
    toPhase :: IterationPhase,
    valid :: Bool
  }
  deriving (Show)

phaseChange :: IterationPhase -> Maybe IterationPhase -> LogProtocolOut -> PhaseChangeValidation
phaseChange lastPhase stageFailure lp =
  maybef
    (phaseSwitch lp stageFailure)
    (PhaseChangeValidation lastPhase lastPhase True)
    (\(PhaseSwitch legalFromPhases to) -> PhaseChangeValidation lastPhase to $ lastPhase `S.member` legalFromPhases)

data DeltaAction a = Clear | Keep | New a

nxtValue :: Maybe a -> DeltaAction a -> Maybe a
nxtValue mCurrent = \case
  Clear -> Nothing
  Keep -> mCurrent
  New val -> Just val

testItrDelta :: LogProtocolBase e -> (DeltaAction Address, DeltaAction ItemId)
testItrDelta =
  let clear = (Clear, Clear)
      keep = (Keep, Keep)
   in \case
        StartTest (TestLogInfo ttl domain _) -> (New $ push ttl RC.Test domain, Clear)
        StartIteration iid _ _ -> (Keep, New iid)
        EndIteration _ -> keep
        StartRun {} -> clear
        EndRun -> clear
        FilterLog _ -> clear
        StaXTGroup _ -> clear
        EndGroup _ -> clear
        EndTest _ -> clear
        -- non-boundry logs
        _ -> keep

nxtIteration :: Maybe (ItemId, IterationOutcome) -> LogProtocolBase e -> Maybe (ItemId, IterationOutcome)
nxtIteration current lp =
  let ( modAction :: DeltaAction Address,
        idAction :: DeltaAction ItemId
        ) = testItrDelta lp
   in case modAction of
        Clear -> Nothing
        New _ -> Nothing
        Keep -> case idAction of
          Clear -> Nothing
          Keep -> current
          New itmId -> Just (itmId, IterationOutcome Pass OutOfIteration)

isCheck :: LogProtocolBase e -> Bool
isCheck =
  \case
    CheckOutcome {} -> True
    _ -> False

resetCheck :: LogProtocolBase e -> Bool
resetCheck =
  \case
    EndIteration _ -> False
    lp' -> not $ isCheck lp'

logProtocolStep :: LPStep -> LogProtocolOut -> LPStep
logProtocolStep (LPStep _phaseValid failStage phase _logItemStatus activeIteration checkEncountered) lpOut@LogProtocolOut {logInfo = lp} =
  let PhaseChangeValidation {toPhase = nxtPhase, valid = nxtPhaseValid} = phaseChange phase failStage lpOut

      nxtActiveItr :: Maybe (ItemId, IterationOutcome)
      nxtActiveItr = nxtIteration activeIteration lp

      nxtCheckEncountered :: Bool
      nxtCheckEncountered =
        isNothing nxtActiveItr || resetCheck lp
          ? False
          $ checkEncountered || isCheck lp

      lgStatus :: ExecutionStatus
      lgStatus = max (logProtocolStatus checkEncountered lp) (nxtPhaseValid ? Pass $ Fail)

      nxtFailStage :: Maybe IterationPhase
      nxtFailStage = calcNextIterationFailStage failStage lgStatus nxtPhase $ Just lp
   in LPStep
        { phaseValid = nxtPhaseValid,
          faileStage = nxtFailStage,
          phase = nxtPhase,
          logItemStatus = lgStatus,
          activeIteration = nxtActiveItr,
          checkEncountered = nxtCheckEncountered
        }

data LPStep = LPStep
  { phaseValid :: Bool,
    faileStage :: Maybe IterationPhase,
    phase :: IterationPhase,
    logItemStatus :: ExecutionStatus,
    activeIteration :: Maybe (ItemId, IterationOutcome),
    checkEncountered :: Bool
  }
  deriving (Show)

$(deriveJSON defaultOptions ''LPStep)

emptyLPStep = LPStep True Nothing OutOfIteration Pass Nothing False

------------------------------------------------------
----------------- Testing Using DList ----------------
------------------------------------------------------

type WriterState i o a = WriterT (DList o) (StateT (DList i) Identity) a

runToList :: DList input -> WriterState input outputItem accum -> (accum, DList outputItem)
runToList input m = fst . runIdentity $ runStateT (runWriterT m) input

testSource :: WriterState i o (Maybe i)
testSource = do
  dlst <- get
  case dlst of
    Nil -> pure Nothing
    Cons x xs -> do
      put $ D.fromList xs
      pure $ Just x
    _ -> P.error "DList pattern match error this should never happen"

testSink :: [o] -> WriterState i o ()
-- testSink = tell . P.fromList
testSink = tell . D.fromList

------------------------------------------------------------
-------------------- Shared Item Components ----------------
------------------------------------------------------------

logProtocolPrettyPrintReducer ::
  Show e =>
  LineNo ->
  () -> -- accumulator
  Either DeserialisationError (LogProtocolBase e) -> -- line item
  ((), Maybe [Text]) -- (accum, result item)
logProtocolPrettyPrintReducer _ _ ethLp =
  ( (),
    Just . pure $
      eitherf
        ethLp
        txtPretty
        (prettyPrintLogProtocol Run)
  )

jsonSerialiser :: A.ToJSON a => a -> ByteString
jsonSerialiser = L.toStrict . A.encode

jsonDeserialiser :: FromJSON a => LineNo -> ByteString -> Either DeserialisationError a
jsonDeserialiser ln bs = mapLeft (\erStr -> DeserialisationError ln (toS erStr) (decodeUtf8' bs)) $ A.eitherDecode $ L.fromStrict bs

yamlSerialiser :: A.ToJSON a => a -> ByteString
yamlSerialiser = Y.encode . Y.toJSON

textToByteString :: Text -> ByteString
textToByteString = toS

showToByteString :: Show a => a -> ByteString
showToByteString = textToByteString . txt