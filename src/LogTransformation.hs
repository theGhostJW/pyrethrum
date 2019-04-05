module LogTransformation where

import Common as C (AppError(..))
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

-- TODO: update to use streaming library such as streamly


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

-----------------------------------------------------------------
----------------- Generalised Log Transformation ----------------
-----------------------------------------------------------------

newtype LineNo = LineNo { unLineNo :: Int } deriving (Show, Eq)

logTransform :: forall a itm rsltItem m. Monad m =>
                                     m (Maybe ByteString)                                                             -- source
                                    -> ([ByteString] -> m ())                                                         -- sink
                                    -> (LineNo -> a -> itm -> (a, Either LogTransformError (Maybe [rsltItem])))       -- reducer step
                                    -> (LineNo -> ByteString -> Either DeserialisationError itm)                      -- item desrialiser
                                    -> (rsltItem -> ByteString)                                                       -- result serialiser
                                    -> (LogTransformError -> ByteString)                                              -- error serialiser
                                    -> LineNo                                                                         -- linNo
                                    -> a                                                                              -- accumulattor
                                    -> m ()
logTransform src snk step idser rsltSersr errSersr lineNo accum =
    let 
      localLoop :: LineNo -> a -> m ()
      localLoop = logTransform src snk step idser rsltSersr errSersr

      errorSnk :: LogTransformError -> m ()
      errorSnk = snk . pure . errSersr 

      rsltSink :: [rsltItem] -> m ()
      rsltSink = snk . fmap rsltSersr
    in
      do 
        lg <- src

        maybef lg
          (pure ()) -- EOF
          (\bs ->
            let 
              (nxtAccum, result) = either
                                     (\e -> (accum, Left e)) 
                                     (step lineNo accum)
                                     (mapLeft LogDeserialisationError $ idser lineNo bs)
            in
              do
                either errorSnk (maybe (pure ()) rsltSink) result
                localLoop (LineNo $ unLineNo lineNo + 1) nxtAccum
          )
          
-- -----------------------------------------------------
-- ----------------- FileTransformation ----------------
-- -----------------------------------------------------

transformToFile :: forall itm rslt accum.    
                  (LineNo -> accum -> itm -> (accum, Either LogTransformError (Maybe [rslt])))   -- line stepper
                  -> (LineNo -> ByteString -> Either DeserialisationError itm)                   -- a deserialiser for the item
                  -> (rslt -> ByteString)                                                        -- a serialiser for the result
                  -> (LogTransformError -> ByteString)                                           -- a serialiser for the error
                  -> accum                                                                       -- seed accumulator
                  -> AbsFile                                                                     -- source file
                  -> (forall m. MonadThrow m => AbsFile -> m AbsFile)                             -- dest file calculation                                                          -- seed accumulator
                  -> IO (Either LogTransformError AbsFile)
transformToFile step idser rser eser seed srcPth destPthFunc =
        let
          source :: Handle -> IO (Maybe ByteString) 
          source h = hIsEOF h >>= bool (Just <$> B.hGetLine h) (pure Nothing)
          
          sink :: Handle -> [ByteString] -> IO () 
          sink h bs = sequence_ $ B.hPutStrLn h <$> bs
          
          processLines :: Handle -> Handle -> IO ()
          processLines hIn hOut = logTransform (source hIn) (sink hOut) step idser rser eser (LineNo 1) seed 
        in
          do
            hSrc <- safeOpenFile srcPth ReadMode
            eitherf hSrc
                (pure . Left . LogIOError "Openning Source File")
                (\hIn -> 
                        (
                          do
                            rsltFile <- destPthFunc srcPth
                            outHndle <- safeOpenFile rsltFile S.WriteMode 
                            eitherf outHndle
                              (pure . Left . LogIOError "Openning Output File")
                              (\hOut -> finally (processLines hIn hOut) (S.hClose hOut) $> Right rsltFile)        
                        ) 
                        `finally`
                           hClose hIn
                )
                
testPrettyPrintFile :: AbsFile                                            -- source file
                    -> (forall m. MonadThrow m => AbsFile -> m AbsFile)   -- destFileFunc
                    -> IO (Either LogTransformError AbsFile)              -- dest file path or error 
testPrettyPrintFile = transformToFile prettyPrintItem lpDeserialiser textToByteString showToByteString ()




------------------------------------------------------
----------------- Testing Using DList ----------------
------------------------------------------------------

type WriterState a = WriterT (DList ByteString) (StateT (DList ByteString) Identity) a

runToList :: DList ByteString -> WriterState a -> DList ByteString
runToList input m = snd . fst . runIdentity $ runStateT (runWriterT m) input

testSource :: WriterState (Maybe ByteString)     
testSource = do 
              dlst <- get 
              case dlst of
                Nil -> pure Nothing
                Cons x xs -> do
                              put $ fromList xs
                              pure $ Just x 
                _ -> P.error "DList pattern match error this should never happen"

testSink :: [ByteString] -> WriterState ()
testSink = tell . fromList 

testPrettyPrint :: DList ByteString -> DList ByteString
testPrettyPrint input = runToList input $ logTransform testSource testSink prettyPrintItem lpDeserialiser textToByteString showToByteString (LineNo 1) ()

------------------------------------------------------------
-------------------- Shared Item Components ----------------
------------------------------------------------------------

prettyPrintItem :: 
                LineNo 
                -> ()                                             -- accumulator
                -> LogProtocol                    -- line item
                -> ((), Either LogTransformError (Maybe [Text]))             -- (accum, result item)
prettyPrintItem _ _ lp = ((), Right .  Just . pure $ logStrPP False lp)

lpDeserialiser :: LineNo -> ByteString -> Either DeserialisationError LogProtocol
lpDeserialiser ln bs = mapLeft (\erStr -> DeserialisationError ln (toS erStr) (decodeUtf8' bs)) $ A.eitherDecode $ L.fromStrict bs

textToByteString :: Text ->  ByteString
textToByteString = toS

showToByteString :: Show a => a ->  ByteString
showToByteString = textToByteString . txt


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
              LogTransformation.Pass -> False
              LogTransformation.Warning _ -> False
              LogTransformation.Fail _ -> True

isWarning :: IterationResult -> Bool
isWarning = \case 
              Inconclusive -> False
              LogTransformation.Pass -> False
              LogTransformation.Warning _ -> True
              LogTransformation.Fail _ -> False

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

data ApStateInfo = ApStateSuccess ApStateDisplay | 
                   FailedInteractor AppError   -- TODO: convert this to own error?
                   deriving (Eq, Show)

data PrepStateInfo = PrepStateSuccess DStateDisplay |
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

      (LP.Warning _) -> LogTransformation.Warning p
      (Warning' _) -> LogTransformation.Warning p
      (LP.Error _) -> LogTransformation.Fail p

      StartRun{} -> Inconclusive
      EndRun -> Inconclusive
      FilterLog _ -> Inconclusive
      StartGroup _ -> Inconclusive
      EndGroup _ -> Inconclusive
      StartTest _ -> Inconclusive
      EndTest _ -> Inconclusive
      
      StartIteration {} -> Inconclusive
      StartInteraction -> Inconclusive
      StartChecks{} -> Inconclusive
      EndIteration _ -> Inconclusive
      
      -- should never happen TODO:: add log for this
      SubLog (Doc dp) -> LogTransformation.Fail p
      SubLog (Run rp) -> case rp of
                              StartPrepState -> Inconclusive
                              IOAction _ -> Inconclusive
                              InteractorSuccess {} -> LogTransformation.Pass
                              InteractorFailure {} -> LogTransformation.Fail p

                              LP.PrepStateSuccess {} -> LogTransformation.Pass
                              PrepStateFailure {} -> LogTransformation.Fail p

                              CheckOutcome _ (CheckReport reslt _) -> case classifyResult reslt of
                                                                        OK -> LogTransformation.Pass
                                                                        CK.Error -> LogTransformation.Fail p
                                                                        CK.Warning -> LogTransformation.Warning p
                                                                        Skipped -> Inconclusive

    notCheckPhase :: Bool
    notCheckPhase = p /= Checks

    worstResult :: IterationResult
    worstResult = max lpResult $ LogTransformation.result (summary ir)

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
                                      StartInteraction -> PreInteractor
                                      StartChecks{} -> PreChecks
                                      EndIteration _ -> case fs of 
                                                          NoFailure -> Checks  -- TODO: ensure raw file test with no checks
                                                          InteractorFailed -> Interactor
                                                          PrepStateFailed -> PrepState

                                      -- should never happen
                                      SubLog (Doc _) -> current
                                      SubLog (Run rp) -> case rp of
                                                            StartPrepState -> PrePrepState
                                                            IOAction _ -> current
                                                            InteractorSuccess{} -> Interactor
                                                            InteractorFailure{}  -> Interactor

                                                            LP.PrepStateSuccess{}  -> PrepState
                                                            PrepStateFailure iid err -> PrepState
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
                StartInteraction -> NoFailure
                StartChecks{} -> NoFailure
                EndIteration _ -> NoFailure

                -- should never happen
                SubLog (Doc _) -> NoFailure
                SubLog (Run rp) -> case rp of
                                      StartPrepState -> NoFailure
                                      IOAction _ -> NoFailure
                                      InteractorSuccess{} -> NoFailure
                                      InteractorFailure{}  -> InteractorFailed

                                      LP.PrepStateSuccess{}  -> NoFailure
                                      PrepStateFailure iid err -> PrepStateFailed
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
                          StartInteraction -> Interactor
                          StartChecks{} -> Checks
                          EndIteration _ -> OutOfIteration

                          -- should never happen
                          SubLog (Doc _) -> current
                          SubLog (Run rp) -> case rp of
                                                StartPrepState -> PrepState
                                                IOAction _ -> current
                                                InteractorSuccess{} -> PrepState -- TODO: how to update when failures
                                                InteractorFailure{}  -> PrepState

                                                LP.PrepStateSuccess{}  -> PreChecks
                                                PrepStateFailure iid err -> PreChecks
                                                CheckOutcome{}  -> Checks

outOfIterationLog :: LogProtocol -> TestIteration
outOfIterationLog = OutOfIterationLog

iterationStep ::
              LineNo                                                                -- lineNo
              -> IterationAccum                                                     -- accum
              -> Either LogTransformError LogProtocol                               -- parse error or apperror
              -> (IterationAccum, Either LogTransformError (Maybe [TestIteration])) -- (newAccum, err / result)
iterationStep lineNo accum@(IterationAccum thisPhase stageFailure mRec) elp = 
    eitherf elp 
      (\e -> (accum, Left e)) -- seirialisation error
      (\lp ->
        let
          isStartIteration :: Bool 
          isStartIteration = case lp of
                                StartIteration{} -> True
                                _ -> False

          isEndIteration ::  Bool 
          isEndIteration = case lp of 
                              EndIteration _ -> True
                              _ -> False

          phaseChangeIsValid :: Bool
          phaseChangeIsValid = expectedCurrentPhase thisPhase stageFailure lp == thisPhase && 
                             (thisPhase == OutOfIteration) == isNothing mRec


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
                                  info = "Unexpected log message encounterred - Messages have either been lost or received out of order. Summary may not reflect true results of the test"
                                } 
            in
              (nextAccum, Right . Just $ maybef mRec 
                                    [err] 
                                    (\r -> [Iteration r , err])
              )

          validPhaseStep :: (IterationAccum, Either LogTransformError (Maybe [TestIteration])) 
          validPhaseStep = uu

        in 
          phaseChangeIsValid ? validPhaseStep $ invalidPhaseStep
      )
                       


                        -- nextRec:: LogProtocol -> Maybe IterationRecord
                        -- nextRec lp = apppendRaw lp <$> 
                        --             case lp of
                        --               Message _ -> rec
                        --               Message' _ -> rec
                        --               LP.Warning _ -> Nothing
                        --               Warning' detailedInfo -> Nothing
                        --               LP.Error _ -> Nothing

                        --               StartRun{} -> Nothing
                        --               EndRun -> Nothing
                        --               FilterLog _ -> Nothing
                        --               StartGroup _ -> Nothing
                        --               EndGroup _ -> Nothing
                        --               StartTest _ -> Nothing
                        --               EndTest _ -> Nothing
                                      
                        --               StartIteration iid pre post val -> Just $ IterationRecord {
                        --                                                                           summary = IterationSummary {
                        --                                                                             iid = iid,
                        --                                                                             pre = pre,
                        --                                                                             post = post,
                        --                                                                             result = Inconclusive
                        --                                                                           },
                        --                                                                           validation = [],
                        --                                                                           otherErrorsDesc = [],
                        --                                                                           otherWarningsDesc = [],
                        --                                                                           domainState = Nothing,
                        --                                                                           item = Nothing,
                        --                                                                           apState = Nothing,
                        --                                                                           rawLog = D.empty
                        --                                                                         }
                        --               StartInteraction -> Interactor
                        --               StartChecks{} -> Validation
                        --               EndIteration _ -> OutOfIteration
      
      
                    
                      -- eitherf elp 
                      --       (\err -> (ConversionError err,  ))
                      --       (\lp -> \
                               
                      --         case lp of
                               
                      --       )
--   -- \case
 
                  -- StartIteration iid  _ _ val -> newLn <> subHeader ("Start Iteration: " <> iterId iid) <> 
                  --                                 newLn <> "Item:" <> 
                  --                                 newLn <> ppAesonBlock val <>
                  --                                 (docMode ? "" $ newLn)

                  -- StartInteraction -> newLn <> "Interaction:"
                  -- StartChecks -> newLn <> "Checks:"

                  -- EndIteration iid -> newLn <> subHeader ("End Iteration: " <> iterId iid)
       

                  -- SubLog (Doc dp) -> case dp of 
                  --                       DocAction ai -> case ai of
                  --                         ActionInfo msg -> "  >> " <> msg
                  --                         ActionInfoM msg extended -> "  >> " <> 
                  --                                                         msg <> 
                  --                                                         newLn <> 
                  --                                                         indent2 extended

                  --                       DocCheck iid chkhdr resultExpectation gateStatus -> 
                  --                                   indent2 $ "% " <> chkhdr  <> 
                  --                                       (
                  --                                         gateStatus == GateCheck 
                  --                                           ? "(Gate: subsequent checks will not be executed if this check fails)" 
                  --                                           $ ""
                  --                                       ) <> 
                  --                                       (
                  --                                       case resultExpectation of 
                  --                                         ExpectPass -> ""
                  --                                         ExpectFailure Inactive  _  -> ""
                  --                                         ExpectFailure Active message -> newLn <> indent2 ("!! This check is expected to fail: " <> message)
                  --                                       )
  
                  --                       DocIOAction m -> logIO m

                  -- SubLog (Run rp) -> case rp of
                  --                       StartPrepState -> newLn <> "PrepState:"
                      
                  --                       IOAction m -> indent2 $ logIO m 
                                        
                  --                       InteractorSuccess iid (ApStateDisplay as) -> newLn <> prettyBlock '>' "Interactor Complete"  iid as
                                          
                  --                       InteractorFailure iid err -> prettyBlock '>' "Interactor Failure" iid $ showPretty err

                  --                       PrepStateSuccess iid (DStateDisplay ds) -> prettyBlock '>' "PrepState Complete" iid ds
                  --                       PrepStateFailure iid err -> prettyBlock '>' "PrepState Failure" iid $ showPretty err

                  --                       CheckOutcome iid (CheckReport reslt (CheckInfo chkhdr mbInfo)) -> prettyBlock 'x' ("Check: " <> showPretty (classifyResult reslt)) iid $ 
                  --                                                                                                                   chkhdr  <> " -> " <> showPretty reslt <> 
                  --                                                                                                                   ppMsgInfo mbInfo


jsnSerialise :: A.ToJSON v => v -> ByteString
jsnSerialise = toS . A.encode

-- itrSerialise = uu

summariseIterations :: AbsFile -> IO Text
summariseIterations inputLog = uu
                              -- let
                              --   seed :: IterationAccumulator 
                              --   seed = uu

                              --   processLines :: Handle -> IO ()
                              --   processLines = runLines iterationStep lpDeserialiser itrSerialise encodeUtf8 seed inputLog
                              -- in
                              --   do 
                              --     itrFile <- inputLog -<.> ".itr"
                              --     outHndle <- safeOpenFile itrFile S.WriteMode
                              --     eitherf outHndle
                              --       (pure . show)
                              --       (\h -> finally (processLines h) (S.hClose h) $> (toS . toFilePath $ itrFile))

$(deriveJSON defaultOptions ''IterationRecord)
$(deriveJSON defaultOptions ''ApStateInfo)
$(deriveJSON defaultOptions ''PrepStateInfo)
$(deriveJSON defaultOptions ''ItemInfo)
$(deriveJSON defaultOptions ''IterationWarning)
$(deriveJSON defaultOptions ''IterationSummary)
$(deriveJSON defaultOptions ''IterationResult)
$(deriveJSON defaultOptions ''IterationError)
$(deriveJSON defaultOptions ''IterationPhase)
$(deriveJSON defaultOptions ''LineNo)
$(deriveJSON defaultOptions ''DeserialisationError)
$(deriveJSON defaultOptions ''LogTransformError)


-- Just $ IterationRecord {
--                                                                                           summary = IterationSummary {
--                                                                                             iid = iid,
--                                                                                             pre = WhenClause pre,
--                                                                                             post =  ThenClause post,
--                                                                                             result = Inconclusive
--                                                                                           },
--                                                                                           validation = [],
--                                                                                           otherErrors = [],
--                                                                                           otherWarnings = [],
--                                                                                           domainState = Nothing,
--                                                                                           item = Nothing,
--                                                                                           apState = Nothing,
--                                                                                           rawLog = D.empty
--                                                                                         }
