module LogTransformation where

import Common as C (AppError(..))
import LogTransformation.Common
import LogTransformation.Iteration
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

-----------------------------------------------------------------
----------------- Generalised Log Transformation ----------------
-----------------------------------------------------------------

logTransform :: forall a itm rsltItem m src snk. Monad m =>
                                     m (Maybe src)                                                             -- source
                                    -> ([snk] -> m ())                                                         -- sink
                                    -> (LineNo -> a -> itm -> (a, Either LogTransformError (Maybe [rsltItem])))       -- reducer step
                                    -> (LineNo -> src -> Either DeserialisationError itm)                      -- item desrialiser
                                    -> (rsltItem -> snk)                                                       -- result serialiser
                                    -> (LogTransformError -> snk)                                              -- error serialiser
                                    -> LineNo                                                                  -- linNo
                                    -> a                                                                       -- accumulattor
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

                
testIterationStepFile :: AbsFile                                            -- source file
                    -> (forall m. MonadThrow m => AbsFile -> m AbsFile)   -- destFileFunc
                    -> IO (Either LogTransformError AbsFile)              -- dest file path or error 
testIterationStepFile = transformToFile iterationStep lpDeserialiser serialiseIteration showToByteString emptyIterationAccum


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

testIterationStep :: DList ByteString -> DList ByteString
testIterationStep input = runToList input $ logTransform testSource testSink iterationStep lpDeserialiser serialiseIteration showToByteString (LineNo 1) emptyIterationAccum

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