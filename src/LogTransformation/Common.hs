module LogTransformation.Common where

import Common as C (AppError(..))
import qualified Check as CK
import Pyrelude as P hiding (phase)
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
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import RunElementClasses
import Data.Yaml as Y
import DSL.LogProtocol.PrettyPrint 

newtype LineNo = LineNo { unLineNo :: Int } deriving (Show, Eq)

data LogTransformError =  LogDeserialisationError DeserialisationError |

                          LogIOError {
                                  message :: Text,
                                  error :: IOError
                                } |

                          LogTransformError {
                                  linNo :: LineNo,
                                  logItem :: LogProtocol,
                                  info :: Text
                                } deriving (Eq, Show)


data DeserialisationError  = DeserialisationError {
  linNo :: LineNo,
  errorTxt :: Text,
  line :: Either UnicodeException Text -- the type for decode UTF8
}  deriving (Eq, Show)


-- Raw Data Types - no totalling

data ExecutionStatus = Pass
                  | KnownError
                  | Warning 
                  | Type2Error 
                  | Fail 
                  | Regression
                  deriving (Show, Eq, Ord, Enum)

instance A.ToJSONKey ExecutionStatus where
  -- default implementation

instance A.FromJSONKey ExecutionStatus where
   -- default implementation

calcNextIterationFailStage :: Maybe IterationPhase -> ExecutionStatus -> IterationPhase -> Maybe IterationPhase
calcNextIterationFailStage mCurrentFailPhase nxtStatus nxtPhase = 
  nxtPhase == OutOfIteration 
      ? Nothing
      $ 
        nxtStatus == Fail 
          ?  maybef mCurrentFailPhase
                (Just nxtPhase)
                (\fs -> Just $ max fs nxtPhase)
          $ mCurrentFailPhase

logProtocolStatus :: LogProtocol -> ExecutionStatus
logProtocolStatus = \case
                        BoundaryLog bl -> Pass 
                        IterationLog (Doc dp) -> Pass -- this should not happen and will cause a phase error to be logged
                        IterationLog (Run rp) -> 
                          case rp of
                            StartPrepState -> Pass
                            IOAction _ -> Pass
          
                            StartInteraction -> Pass
                            InteractorSuccess {} -> Pass
                            InteractorFailure {} -> Fail
          
                            PrepStateSuccess {} -> Pass
                            PrepStateFailure {} -> Fail
          
                            StartChecks{} -> Pass
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
data IterationPhase = OutOfIteration
                      | Checks 
                      | PreChecks
                      | PrepState 
                      | PrePrepState 
                      | Interactor
                      | PreInteractor 
                       deriving (Eq, Ord, Show)

data IterationOutcome = IterationOutcome {
                          executionStatus :: ExecutionStatus, 
                          iterationPhase :: IterationPhase
                        } deriving (Eq, Ord, Show)

type IterationResults = M.Map ItemId IterationOutcome

type StatusCount = M.Map ExecutionStatus Int

data RunResults = RunResults {
                                outOfTest :: StatusCount,
                                iterationResults :: IterationResults
                             } 
                             deriving Show

data PhaseSwitch = PhaseSwitch {
                        from :: IterationPhase, 
                         to :: IterationPhase
                     }

-- calculate expected from / to base on log message
phaseSwitch :: LogProtocol -> Maybe IterationPhase -> Maybe PhaseSwitch
phaseSwitch lp mFailedPhase = 
  let
    ps :: IterationPhase -> IterationPhase -> Maybe PhaseSwitch
    ps cur = Just . PhaseSwitch cur

    outToOut :: Maybe PhaseSwitch
    outToOut = ps OutOfIteration OutOfIteration
  in
    case lp of
        BoundaryLog bl -> case bl of 
                            StartRun{} -> outToOut
                            EndRun -> outToOut
                            FilterLog _ -> outToOut
                            StartGroup _ -> outToOut
                            EndGroup _ -> outToOut
                            StartTest _ -> outToOut
                            EndTest _ -> outToOut
                            StartIteration{} -> ps OutOfIteration PreInteractor
                            EndIteration _ -> ps (fromMaybe Checks mFailedPhase) OutOfIteration

        IterationLog (Doc _) -> Nothing
        
        IterationLog (Run rp) -> case rp of
                                    LP.Error _ -> Nothing
                                    StartPrepState -> ps PrePrepState PrepState
                                    IOAction _ -> Nothing
                                    StartInteraction -> ps PreInteractor Interactor
                                    InteractorSuccess{} -> ps Interactor PrePrepState 
                                    InteractorFailure{}  -> Nothing -- leave in failed stage
                                    PrepStateSuccess{}  -> ps PrepState PreChecks
                                    PrepStateFailure iid err -> Nothing -- leave in failed phase
                                    StartChecks{} -> ps PreChecks Checks
                                    Message _ -> Nothing
                                    Message' _ -> Nothing
                                    LP.Warning s -> Nothing
                                    Warning' detailedInfo -> Nothing
                                    CheckOutcome{} -> Nothing -- stay in checks
                  
phaseChange :: IterationPhase -> Maybe IterationPhase -> LogProtocol -> (Bool, IterationPhase)
phaseChange lastPhase stageFailure lp =  
  maybef (phaseSwitch lp stageFailure)
    (True, lastPhase)
    (\(PhaseSwitch from to) -> (from == lastPhase, to))

data DeltaAction a = Clear | Leave | New a

nxtValue :: Maybe a -> DeltaAction a -> Maybe a
nxtValue  mCurrent = \case 
                        Clear -> Nothing 
                        Leave -> mCurrent 
                        New val -> Just val

testItrDelta :: LogProtocol -> (DeltaAction TestModule, DeltaAction ItemId)
testItrDelta = 
  let 
    clear = (Clear, Clear)
    leave = (Leave, Leave)
  in
    \case 
      BoundaryLog bl -> 
        case bl of 
            StartTest (TestDisplayInfo mdule _ _) -> (New mdule, Clear)
            StartIteration iid _ _ _ -> (Leave, New iid)
            EndIteration _ -> (Leave, Clear)
            StartRun{} -> clear
            EndRun -> clear
            FilterLog _ -> clear
            StartGroup _ -> clear
            EndGroup _ -> clear
            EndTest _ -> clear
                        
      IterationLog (Doc _) -> leave -- should not happen
      IterationLog (Run rp) -> leave

nxtIteration :: Maybe (ItemId, IterationOutcome) -> LogProtocol -> Maybe (ItemId, IterationOutcome) 
nxtIteration current lp = 
  let 
    (
      modAction :: DeltaAction TestModule, 
      idAction :: DeltaAction ItemId
     ) = testItrDelta lp

    newId :: ItemId -> Maybe (ItemId, IterationOutcome) -> Maybe (ItemId, IterationOutcome)
    newId itmId = const $ Just (itmId, IterationOutcome Pass OutOfIteration) 

  in 
    case modAction of 
                Clear -> Nothing
                New testMod -> Nothing
                Leave -> case idAction of 
                            Clear -> Nothing
                            Leave -> current
                            New itmId -> Just (itmId, IterationOutcome Pass OutOfIteration)


statsStepFromLogProtocol :: LPStep -> LogProtocol -> LPStep
statsStepFromLogProtocol (LPStep phaseValid failStage phase activeIteration) lp = 
  let 
    (
      nxtPhaseValid :: Bool, 
      nxtPhase :: IterationPhase
      ) = phaseChange phase failStage lp

    nxtActiveItr :: Maybe (ItemId, IterationOutcome)
    nxtActiveItr = nxtIteration activeIteration lp

    nxtStatus :: ExecutionStatus
    nxtStatus = max (logProtocolStatus lp) (nxtPhaseValid ? Pass $ Fail)

    nxtFailStage :: Maybe IterationPhase
    nxtFailStage = calcNextIterationFailStage failStage nxtStatus nxtPhase

  in 
    LPStep {
      phaseValid = nxtPhaseValid, 
      faileStage = nxtFailStage,
      phase = nxtPhase,
      activeIteration = nxtActiveItr
    }

data LPStep = LPStep {
  phaseValid :: Bool, 
  faileStage ::  Maybe IterationPhase,
  phase :: IterationPhase,
  activeIteration :: Maybe (ItemId, IterationOutcome)
}
------------------------------------------------------
----------------- Testing Using DList ----------------
------------------------------------------------------

type WriterState a i o = WriterT (DList o) (StateT (DList i) Identity) a

runToList :: DList input -> WriterState accum input outputItem -> (accum, DList outputItem)
runToList input m = fst . runIdentity $ runStateT (runWriterT m) input

testSource :: WriterState (Maybe i) i o 
testSource = do 
              dlst <- get 
              case dlst of
                Nil -> pure Nothing
                Cons x xs -> do
                              put $ fromList xs
                              pure $ Just x 
                _ -> P.error "DList pattern match error this should never happen"

testSink :: [o] -> WriterState () i o
testSink = tell . fromList
                             
------------------------------------------------------------
-------------------- Shared Item Components ----------------
------------------------------------------------------------

logProtocolPrettyPrintReducer :: 
                LineNo 
                -> ()                  -- accumulator
                -> Either DeserialisationError LogProtocol         -- line item
                -> ((), Maybe [Text])             -- (accum, result item)
logProtocolPrettyPrintReducer _ _ ethLp = (
                                              (), Just . pure $ eitherf ethLp  
                                                               txtPretty 
                                                               (prettyPrintLogProtocol False)
                                           )

jsonSerialiser :: A.ToJSON a => a -> ByteString
jsonSerialiser = L.toStrict . A.encode

jsonDeserialiser :: FromJSON a => LineNo -> ByteString -> Either DeserialisationError a
jsonDeserialiser ln bs = mapLeft (\erStr -> DeserialisationError ln (toS erStr) (decodeUtf8' bs)) $ A.eitherDecode $ L.fromStrict bs

yamlSerialiser :: A.ToJSON a => a -> ByteString
yamlSerialiser = Y.encode . Y.toJSON

textToByteString :: Text -> ByteString
textToByteString = toS

showToByteString :: Show a => a ->  ByteString
showToByteString = textToByteString . txt
                            

$(deriveJSON defaultOptions ''LogTransformError)
$(deriveJSON defaultOptions ''LineNo)
$(deriveJSON defaultOptions ''DeserialisationError)
$(deriveJSON defaultOptions ''IterationPhase)
$(deriveJSON defaultOptions ''ExecutionStatus)
$(deriveJSON defaultOptions ''IterationOutcome)
$(deriveJSON defaultOptions ''RunResults)