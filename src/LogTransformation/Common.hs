module LogTransformation.Common where

import qualified Check as CK
import Pyrelude as P hiding (phase)
import Data.DList as D
import OrphanedInstances()
import DSL.LogProtocol as LP
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import RunElementClasses
import Data.Yaml as Y
import DSL.LogProtocol.PrettyPrint 
import qualified Data.Set as S

newtype LineNo = LineNo { unLineNo :: Int } deriving (Show, Eq)

data LogTransformError e = LogDeserialisationError DeserialisationError |

                          LogIOError {
                                  message :: Text,
                                  error :: IOError
                                } |

                          LogTransformError {
                                  linNo :: LineNo,
                                  logItem :: LogProtocolBase e,
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

isBoundaryLog :: LogProtocolBase e -> Bool
isBoundaryLog = 
  \case
   BoundaryLog bl -> True
   _ -> False

calcNextIterationFailStage :: Maybe IterationPhase -> ExecutionStatus -> IterationPhase -> Maybe (LogProtocolBase e) -> Maybe IterationPhase
calcNextIterationFailStage mCurrentFailPhase lgStatus currPhase mLp = 
  currPhase == OutOfIteration || maybe False isBoundaryLog mLp
      ? Nothing
      $ 
        lgStatus > LogTransformation.Common.Warning
          ?  maybef mCurrentFailPhase
                (Just currPhase)
                (\fs -> Just $ max fs currPhase)
          $ mCurrentFailPhase

logProtocolStatus :: Bool -> LogProtocolBase e -> ExecutionStatus
logProtocolStatus chkEncountered = \case
                        BoundaryLog bl -> 
                          case bl of 
                            -- a test with o checks is deemed a fail
                            EndIteration{} -> chkEncountered ? Pass $ Fail 
                            _ -> Pass

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
                        from :: S.Set IterationPhase, 
                         to :: IterationPhase
                     }

isEndIteration :: LogProtocolBase e -> Bool
isEndIteration = \case
                    BoundaryLog bl -> case bl of 
                                        EndIteration _ -> True
                                        _ -> False
                    _ -> False

-- calculate expected from / to base on log message
phaseSwitch :: LogProtocolBase e -> Maybe IterationPhase -> Maybe PhaseSwitch
phaseSwitch lp mFailedPhase = 
  let
    ps :: IterationPhase -> IterationPhase -> Maybe PhaseSwitch
    ps cur nxt = Just $ PhaseSwitch (S.singleton cur) nxt

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
                            -- if test is empty state will be OutOfIteration else Checks
                            EndTest _ -> Just $ PhaseSwitch (S.fromList [Checks, OutOfIteration]) OutOfIteration
                            -- first iteration will be OutOfIteration else Checks 
                            StartIteration{} -> Just $ PhaseSwitch (S.fromList [Checks, OutOfIteration]) PreInteractor
                            -- checks -> checks checks is the last status of an iteration
                            -- need to leave in checks until after EndIteration because need to be in iteration when 
                            -- processing final EndIteration 
                            EndIteration _ -> Nothing

        IterationLog (Doc _) -> Nothing
        
        IterationLog (Run rp) -> case rp of
                                    LP.Error _ -> Nothing
                                    StartPrepState -> ps PrePrepState PrepState
                                    IOAction _ -> Nothing
                                    StartInteraction -> ps PreInteractor Interactor
                                    InteractorSuccess{} -> ps Interactor PrePrepState 
                                    InteractorFailure{}  -> Nothing -- keep in failed stage
                                    PrepStateSuccess{}  -> ps PrepState PreChecks
                                    PrepStateFailure iid err -> Nothing -- keep in failed phase
                                    StartChecks{} -> ps (fromMaybe PreChecks mFailedPhase) Checks
                                    Message _ -> Nothing
                                    Message' _ -> Nothing
                                    LP.Warning s -> Nothing
                                    Warning' detailedInfo -> Nothing
                                    CheckOutcome{} -> Nothing -- stay in checks
                  
phaseChange :: IterationPhase -> Maybe IterationPhase -> LogProtocolBase e -> (Bool, IterationPhase)
phaseChange lastPhase stageFailure lp =  
  maybef (phaseSwitch lp stageFailure)
    (True, lastPhase)
    (\(PhaseSwitch from to) -> (lastPhase `S.member` from, to))

data DeltaAction a = Clear | Keep | New a

nxtValue :: Maybe a -> DeltaAction a -> Maybe a
nxtValue  mCurrent = \case 
                        Clear -> Nothing 
                        Keep -> mCurrent 
                        New val -> Just val

testItrDelta :: LogProtocolBase e -> (DeltaAction TestModule, DeltaAction ItemId)
testItrDelta = 
  let 
    clear = (Clear, Clear)
    keep = (Keep, Keep)
  in
    \case 
      BoundaryLog bl -> 
        case bl of 
          StartTest (TestDisplayInfo mdule _ _) -> (New mdule, Clear)
          StartIteration iid _ _ _ -> (Keep, New iid)
          EndIteration _ -> keep
          StartRun{} -> clear
          EndRun -> clear
          FilterLog _ -> clear
          StartGroup _ -> clear
          EndGroup _ -> clear
          EndTest _ -> clear
                        
      IterationLog (Doc _) -> keep -- should not happen
      IterationLog (Run rp) -> keep

nxtIteration :: Maybe (ItemId, IterationOutcome) -> LogProtocolBase e -> Maybe (ItemId, IterationOutcome) 
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
      Keep -> case idAction of 
                Clear -> Nothing
                Keep -> current
                New itmId -> Just (itmId, IterationOutcome Pass OutOfIteration)


logProtocolStep :: LPStep -> LogProtocolBase e -> LPStep
logProtocolStep (LPStep phaseValid failStage phase logItemStatus activeIteration checkEncountered) lp = 
  let 
    (
      nxtPhaseValid :: Bool, 
      nxtPhase :: IterationPhase
      ) = phaseChange phase failStage lp

    nxtActiveItr :: Maybe (ItemId, IterationOutcome)
    nxtActiveItr = nxtIteration activeIteration lp

    isCheck :: LogProtocolBase e -> Bool 
    isCheck = 
      \case 
          BoundaryLog{} -> False
          IterationLog sp ->
            case sp of 
              Doc _ -> False
              Run rp -> 
                case rp of 
                  CheckOutcome{} -> True 
                  _ -> False 

    resetCheck :: LogProtocolBase e -> Bool 
    resetCheck = 
      \case 
          BoundaryLog bl -> 
            case bl of 
              EndIteration _ -> False
              _ -> True

          lp'@(IterationLog sp) ->
            case sp of 
              Doc _ -> False
              Run rp -> not $ isCheck lp'

    nxtCheckEncountered :: Bool
    nxtCheckEncountered = isNothing nxtActiveItr || resetCheck lp
                            ? False
                            $ checkEncountered || isCheck lp

    lgStatus :: ExecutionStatus
    lgStatus = max (logProtocolStatus checkEncountered lp) (nxtPhaseValid ? Pass $ Fail)

    nxtFailStage :: Maybe IterationPhase
    nxtFailStage = calcNextIterationFailStage failStage lgStatus nxtPhase $ Just lp
  in 
    LPStep {
      phaseValid = nxtPhaseValid, 
      faileStage = nxtFailStage,
      phase = nxtPhase,
      logItemStatus = lgStatus,
      activeIteration = nxtActiveItr,
      checkEncountered = nxtCheckEncountered
    }

data LPStep = LPStep {
  phaseValid :: Bool, 
  faileStage ::  Maybe IterationPhase,
  phase :: IterationPhase,
  logItemStatus :: ExecutionStatus,
  activeIteration :: Maybe (ItemId, IterationOutcome),
  checkEncountered :: Bool
} deriving Show

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
                              put $ fromList xs
                              pure $ Just x 
                _ -> P.error "DList pattern match error this should never happen"

testSink :: [o] -> WriterState i o () 
testSink = tell . fromList
                             
------------------------------------------------------------
-------------------- Shared Item Components ----------------
------------------------------------------------------------

logProtocolPrettyPrintReducer :: Show e =>
                LineNo 
                -> ()                  -- accumulator
                -> Either DeserialisationError (LogProtocolBase e)         -- line item
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
$(deriveJSON defaultOptions ''LPStep)