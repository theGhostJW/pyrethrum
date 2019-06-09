module LogTransformation where

import Common as C (AppError(..))
import LogTransformation.Common
import LogTransformation.Iteration as I
import LogTransformation.Test
import Check as CK
import Pyrelude as P
import Pyrelude.IO
import Data.DList as D
import Data.Yaml as Y
import qualified Prelude as PO
import AuxFiles
import OrphanedInstances
import DSL.LogProtocol as LP
import DSL.LogProtocol.PrettyPrint
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
import PrettyPrintCommon as PC

-- TODO: update to use streaming library such as streamly

-----------------------------------------------------------------
----------------- Generalised Log Transformation ----------------
-----------------------------------------------------------------

data LogTransformParams a itm rsltItem m src snk = LogTransformParams {
  source :: m (Maybe src),
  sink :: [snk] -> m (),
  reducer:: LineNo -> a -> itm -> (a, Either LogTransformError (Maybe [rsltItem])),
  itemDesrialiser :: LineNo -> src -> Either DeserialisationError itm,
  resultSerialiser :: rsltItem -> snk,    
  errorSerialiser :: LogTransformError -> snk,
  linNo :: LineNo,
  accumulator :: a
}

logTransform :: forall a itm rsltItem m src snk. Monad m => LogTransformParams a itm rsltItem m src snk -> m ()
logTransform (LogTransformParams src snk step idser rsltSersr errSersr lineNo accum) =
    let 
      localLoop :: LineNo -> a -> m ()
      localLoop ln a = logTransform (LogTransformParams src snk step idser rsltSersr errSersr ln a)

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
          processLines hIn hOut = logTransform  LogTransformParams {
                                                  source = source hIn,
                                                  sink = sink hOut,
                                                  reducer = step,
                                                  itemDesrialiser = idser,
                                                  resultSerialiser = rser,    
                                                  errorSerialiser = eser,
                                                  linNo = LineNo 1,
                                                  accumulator = seed
                                                }
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
testPrettyPrintFile = transformToFile prettyPrintItem jsonDeserialiser textToByteString showToByteString ()

                
testIterationStepFile :: AbsFile                                            -- source file
                    -> (forall m. MonadThrow m => AbsFile -> m AbsFile)   -- destFileFunc
                    -> IO (Either LogTransformError AbsFile)              -- dest file path or error 
testIterationStepFile = transformToFile iterationStep jsonDeserialiser yamlSerialiser showToByteString emptyIterationAccum
                
------------------------------------------------------
----------------- Testing Using DList ----------------
------------------------------------------------------

type WriterState a i o = WriterT (DList o) (StateT (DList i) Identity) a

runToList :: DList i -> WriterState a i o -> DList o
runToList input m = snd . fst . runIdentity $ runStateT (runWriterT m) input

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

testPrettyPrint :: DList ByteString -> DList ByteString
testPrettyPrint input = runToList input $ logTransform LogTransformParams {
                                                                            source = testSource,
                                                                            sink = testSink,
                                                                            reducer = prettyPrintItem,
                                                                            itemDesrialiser = jsonDeserialiser,
                                                                            resultSerialiser = textToByteString,    
                                                                            errorSerialiser = showToByteString,
                                                                            linNo = LineNo 1,
                                                                            accumulator = ()
                                                                          }

iterationToJsonParams :: LogTransformParams I.IterationAccum LogProtocol IterationLogElement (WriterT (DList ByteString) (StateT (DList ByteString) Identity)) ByteString ByteString
iterationToJsonParams = LogTransformParams {
                          source = testSource,
                          sink = testSink,
                          reducer = iterationStep,
                          itemDesrialiser = jsonDeserialiser,
                          resultSerialiser = jsonSerialiser,    
                          errorSerialiser = jsonSerialiser,
                          linNo = LineNo 1,
                          accumulator = emptyIterationAccum
                        }

testIterationStep :: DList ByteString -> DList ByteString
testIterationStep input = runToList input $ logTransform iterationToJsonParams

testIterationPretyPrintStep :: DList ByteString -> DList ByteString
testIterationPretyPrintStep input = runToList input $ logTransform iterationToJsonParams {
                                                                            resultSerialiser = yamlSerialiser,    
                                                                            errorSerialiser = showToByteString
                                                                          } 

iterationToRawTestLogParams :: LogTransformParams TestAccum IterationLogElement TestLogElement (WriterT (DList TestLogElement) (StateT (DList ByteString) Identity)) ByteString TestLogElement
iterationToRawTestLogParams = LogTransformParams {
                                      source = testSource,
                                      sink = testSink,
                                      reducer = testStep,
                                      itemDesrialiser = jsonDeserialiser,
                                      resultSerialiser = id,    
                                      errorSerialiser = TransError . IterationTransError "Log Processing Error" . LineError,
                                      linNo = LineNo 1,
                                      accumulator = emptyTestAccum
                                    }

testTestLogStepRaw :: DList ByteString -> [TestLogElement]
testTestLogStepRaw input = P.toList . runToList input $ logTransform iterationToRawTestLogParams

iterationToTestLogParams :: LogTransformParams TestAccum IterationLogElement TestLogElement (WriterT (DList ByteString) (StateT (DList ByteString) Identity)) ByteString ByteString
iterationToTestLogParams = LogTransformParams {
                                      source = testSource,
                                      sink = testSink,
                                      reducer = testStep,
                                      itemDesrialiser = jsonDeserialiser,
                                      resultSerialiser = jsonSerialiser,    
                                      errorSerialiser = jsonSerialiser,
                                      linNo = LineNo 1,
                                      accumulator = emptyTestAccum
                                    }

testTestLogStep :: DList ByteString -> DList ByteString
testTestLogStep input = runToList input $ logTransform iterationToTestLogParams

testTestLogPrettyPrintStep :: DList ByteString -> DList ByteString
testTestLogPrettyPrintStep input = runToList input $ logTransform iterationToTestLogParams  {
                                                                                              resultSerialiser = toS . prettyPrintTestLogElement,    
                                                                                              errorSerialiser = toS . prettyPrintTestLogElement 
                                                                                                                . TransError . IterationTransError "Log Processing Error" . LineError
                                                                                            } 

------------------------------------------------------------
-------------------- Shared Item Components ----------------
------------------------------------------------------------

prettyPrintItem :: 
                LineNo 
                -> ()                  -- accumulator
                -> LogProtocol         -- line item
                -> ((), Either LogTransformError (Maybe [Text]))             -- (accum, result item)
prettyPrintItem _ _ lp = ((), Right .  Just . pure $ prettyPrintLogProtocol False lp)

jsonSerialiser :: A.ToJSON a => a -> ByteString
jsonSerialiser = L.toStrict . A.encode

jsonDeserialiser :: FromJSON a => LineNo -> ByteString -> Either DeserialisationError a
jsonDeserialiser ln bs = mapLeft (\erStr -> DeserialisationError ln (toS erStr) (decodeUtf8' bs)) $ A.eitherDecode $ L.fromStrict bs

yamlSerialiser :: A.ToJSON a => a -> ByteString
yamlSerialiser = Y.encode . Y.toJSON

textToByteString :: Text ->  ByteString
textToByteString = toS

showToByteString :: Show a => a ->  ByteString
showToByteString = textToByteString . txt

