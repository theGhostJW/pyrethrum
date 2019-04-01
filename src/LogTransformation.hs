module LogTransformation where

import Common
import Check
import Pyrelude as E
import Pyrelude.IO
import Data.DList as D
import qualified Prelude as P
import AuxFiles
import DSL.LogProtocol
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

-----------------------------------------------------------------
----------------- Generalised Log Transformation ----------------
-----------------------------------------------------------------

logTransform :: forall a itm rsltItem m. Monad m =>
                                     m (Maybe ByteString)                                                             -- source
                                    -> (ByteString -> m ())                                                           -- sink
                                    -> (a -> Either AppError itm -> (a, Either AppError (Maybe rsltItem)))  -- reducer step
                                    -> (ByteString -> Either AppError itm)                                            -- item desrialiser
                                    -> (rsltItem -> ByteString)                                                       -- result serialiser
                                    -> (AppError -> ByteString)                                                       -- error serialiser
                                    -> LineNo                                                                         -- linNo
                                    -> a                                                                              -- accumulattor
                                    -> m ()
logTransform src snk step ipsr rsltSersr errSersr lineNo accum =
    let 
      localLoop :: LineNo -> a -> m ()
      localLoop = logTransform src snk step ipsr rsltSersr errSersr

      errorSnk :: AppError -> m ()
      errorSnk = snk . errSersr

      lineSink :: rsltItem -> m ()
      lineSink = snk . rsltSersr
    in
      do 
        lg <- src

        maybef lg
          (pure ()) -- EOF
          (\bs ->
            let 
             (nxtAccum, result) = step accum $ ipsr bs
            in
              do
                eitherf result
                  (errorSnk . AppLineError lineNo)
                  (maybe
                    (pure ())
                    lineSink                    
                  ) 
                localLoop (debug $ LineNo $ unLineNo lineNo + 1) nxtAccum
          )
          
-----------------------------------------------------
----------------- FileTransformation ----------------
-----------------------------------------------------

transformToFile :: forall itm rslt accum.    
                  (accum -> Either AppError itm -> (accum, Either AppError (Maybe rslt)))  -- line stepper
                  -> (ByteString -> Either AppError itm)                                             -- a parser for the item
                  -> (rslt -> ByteString)                                                            -- a serialiser for the result
                  -> (AppError -> ByteString)                                                        -- a serialiser for the error
                  -> accum                                                                           -- seed accumulator
                  -> AbsFile                                                                         -- source file
                  -> (forall m. MonadThrow m => AbsFile -> m AbsFile)                                -- dest file calculation                                                          -- seed accumulator
                  -> IO (Either AppError AbsFile)
transformToFile step ipsr rser eser seed srcPth destPthFunc =
        let
          source :: Handle -> IO (Maybe ByteString) 
          source h = hIsEOF h >>= bool (Just <$> B.hGetLine h) (pure Nothing)

          sink :: Handle -> ByteString -> IO () 
          sink = B.hPutStrLn

          processLines :: Handle -> Handle -> IO ()
          processLines hIn hOut = logTransform (source hIn) (sink hOut) step ipsr rser eser (LineNo 1) seed 
        in
          do
            hSrc <- safeOpenFile srcPth ReadMode
            eitherf hSrc
                (pure . Left . AppIOError' "Openning Source File")
                (\hIn -> 
                        (
                          do
                            rsltFile <- destPthFunc srcPth
                            outHndle <- safeOpenFile rsltFile S.WriteMode 
                            eitherf outHndle
                              (pure . Left . AppIOError' "Openning Output File")
                              (\hOut -> finally (processLines hIn hOut) (S.hClose hOut) $> Right rsltFile)        
                        ) 
                        `finally`
                           hClose hIn
                 )

testPrettyPrintFile :: AbsFile                                            -- source file
                    -> (forall m. MonadThrow m => AbsFile -> m AbsFile)   -- destFileFunc
                    -> IO (Either AppError AbsFile)                       -- dest file path or error 
testPrettyPrintFile = transformToFile prettyPrintItem lpParser textToByteString showableToByteString ()

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
                _ -> error "DList pattern match error this should never happen"

testSink :: ByteString -> WriterState ()
testSink = tell . D.singleton

testPrettyPrint :: DList ByteString -> DList ByteString
testPrettyPrint input = runToList input $ logTransform testSource testSink prettyPrintItem lpParser textToByteString showableToByteString (LineNo 1) ()

------------------------------------------------------------
-------------------- Shared Item Components ----------------
------------------------------------------------------------

prettyPrintItem :: ()                                             -- accumulator
                -> Either AppError LogProtocol                    -- line item
                -> ((), Either AppError (Maybe Text))             -- (accum, result item)
prettyPrintItem _ ethLn = ((), Just . logStrPP False <$> ethLn)

lpParser :: ByteString -> Either AppError LogProtocol
lpParser bs = mapLeft (AppGenericError . toS) $ A.eitherDecode $ L.fromStrict bs

textToByteString :: Text ->  ByteString
textToByteString = toS

showableToByteString :: Show a => a ->  ByteString
showableToByteString = textToByteString . txt


--------------------------------------------------------
----------------- Iteration Aggregation ----------------
--------------------------------------------------------
data IterationPhase = Unknown | 
                      Interactor | 
                      PrepState |
                      Validation
                      deriving (Eq, Show)

data IterationResult = Inconclusive |
                       Pass |
                       Fail IterationPhase |
                       Warning IterationPhase
                       deriving (Eq, Show)
                        
data IterationRecord = IterationRecord {
  summary :: IterationResult,
  validation :: [CheckReport],
  domainState :: LogProtocol,
  item :: LogProtocol,
  apState :: LogProtocol,
  rawLog :: DList LogProtocol
}

data TestIteraion = Iteration |
                    OutOfIterationError AppError |
                    ParseError AppError
                    deriving (Show, Eq)



-- iterationStep :: (IterationAccumulator -> Either AppError LogProtocol -> (IterationAccumulator, Either AppError (Maybe TestIteraion)))  -- reducer step
-- iterationStep linNo accum = uu
--   -- \case
--   --                               Message Text            -> uu
--   --                               Message' DetailedInfo |

--   --                               Warning Text |
--   --                               Warning' DetailedInfo |

--   --                               IOAction Text |
--   --                               DocIOAction Text |
--   --                               DocAction DocActionInfo |
--   --                               DocCheck ItemId Text ResultExpectation GateStatus | 
--   --                               DocStartInteraction | 
--   --                               DocStartChecks | 

--   --                               InteractorSuccess ItemId ApStateDisplay |
--   --                               InteractorFailure ItemId AppError |

--   --                               PrepStateSuccess ItemId DStateDisplay |
--   --                               PrepStateFailure ItemId AppError |

--   --                               Error AppError |
--   --                               FilterLog [FilterResult] |

--   --                               StartRun RunTitle Value | 
--   --                               EndRun |

--   --                               StartGroup GroupTitle |
--   --                               EndGroup GroupTitle |

--   --                               StartTest TestDisplayInfo |
--   --                               EndTest TestModule |

--   --                               CheckOutcome ItemId CheckReport |

--   --                               StartIteration ItemId WhenClause ThenClause Value | 
--   --                               EndIteration ItemId 


jsnSerialise :: A.ToJSON v => v -> ByteString
jsnSerialise = toS . A.encode

-- itrSerialise = uu

summariseIterations :: AbsFile -> IO Text
summariseIterations inputLog = uu
                              -- let
                              --   seed :: IterationAccumulator 
                              --   seed = uu

                              --   processLines :: Handle -> IO ()
                              --   processLines = runLines iterationStep lpParser itrSerialise encodeUtf8 seed inputLog
                              -- in
                              --   do 
                              --     itrFile <- inputLog -<.> ".itr"
                              --     outHndle <- safeOpenFile itrFile S.WriteMode
                              --     eitherf outHndle
                              --       (pure . show)
                              --       (\h -> finally (processLines h) (S.hClose h) $> (toS . toFilePath $ itrFile))

$(deriveJSON defaultOptions ''TestIteraion)

