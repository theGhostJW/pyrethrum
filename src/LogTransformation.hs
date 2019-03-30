module LogTransformation where

import Common
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


transformToFile :: forall itm rslt accum.                          
                (ByteString -> Either AppError itm)                                              -- a parser for the item
                -> (rslt -> ByteString)                                                     -- a serialiser for the result
                -> (AppError -> ByteString)                                                      -- a serialiser for the error
                -> (LineNo -> accum -> Either AppError itm -> (accum, Either AppError (Maybe rslt)))  -- line stepper
                -> accum                                                                    -- seed accumulator
                -> AbsFile                                                                  -- source file
                -> (forall m. MonadThrow m => AbsFile -> m AbsFile)                         -- dest file calculation                                                          -- seed accumulator
                -> IO (Either P.IOError AbsFile)
transformToFile ipsr rser eser step seed srcPth destPthFunc =
              let
                processLines :: Handle -> IO (Either P.IOError ())
                processLines = runLines step ipsr rser eser seed srcPth
              in
                do 
                  rsltFile <- destPthFunc srcPth
                  outHndle <- safeOpenFile rsltFile S.WriteMode
                  eitherf outHndle
                    (pure . Left)
                    (\h -> finally (processLines h) (S.hClose h) $> pure rsltFile)

testTransform :: forall accum itm rslt. (LineNo -> accum -> Either AppError itm -> (accum, Either AppError (Maybe rslt))) -- reducer step
                                    -> (ByteString -> Either AppError itm)                                               -- a parser for the item
                                    -> (rslt -> ByteString)                                                         -- a serialiser for the result
                                    -> (AppError -> ByteString)                                                    -- a serialiser for the error
                                    -> accum                                                                        -- accumulator
                                    -> DList ByteString                                                             -- input lines
                                    -> DList ByteString
testTransform step iPsr rsltSersr errSersr seed lstIn = 
  let
    testParseStep :: accum 
                  -> DList ByteString
                  -> DList ByteString
                  -> LineNo
                  -> DList ByteString
    testParseStep accum inLst outLst lineNo = 
      case inLst of
        Nil -> outLst
        Cons x xs -> 
          let 
            (newAccm, stepLine) = mapLeft (AppParseError lineNo) <$> step lineNo accum (iPsr x)
            newLOut = eitherf stepLine
                        (D.snoc outLst . errSersr) 
                        ( \sl ->
                          maybef sl 
                            outLst
                            (D.snoc outLst . rsltSersr) 
                        )
          in 
            testParseStep newAccm (fromList xs) newLOut (LineNo . succ $ unLineNo lineNo)
        _ -> error "DList pattern match error this should never happen"
    in 
      testParseStep seed lstIn (fromList []) $ LineNo 1

testPrettyPrint :: DList ByteString -> DList ByteString
testPrettyPrint = testTransform prettyPrintItem lpParser toS (toS . txt) ()

mainloop :: forall accum itm rslt. (LineNo -> accum -> Either AppError itm -> (accum, Either AppError (Maybe rslt))) -- reducer step
  -> (ByteString -> Either AppError itm)
  -> (rslt -> ByteString)   
  -> (AppError -> ByteString)
  -> Handle                       -- source
  -> Handle                       -- sink
  -> LineNo 
  -> accum 
  -> IO ()
mainloop step ipsr rsltSersr errSersr inh outh lineNo accum =
  let 
    localLoop :: LineNo -> accum -> IO () 
    localLoop = mainloop step ipsr rsltSersr errSersr inh outh 

    output :: B.ByteString -> IO ()
    output = B.hPutStrLn outh
  in
    hIsEOF inh >>=
              bool
                  ( -- not EOF -> has data
                    do 
                      byteLine <- B.hGetLine inh
                      let 
                        (nxtAccum, result) = step lineNo accum (ipsr byteLine)
                      
                      eitherf result
                        (output . errSersr)
                        (maybe
                          (pure ())
                          (output . rsltSersr)
                        )
                        
                      localLoop (LineNo $ unLineNo lineNo + 1) nxtAccum
                  )
                  (pure ())

-- TODO: re-implement with streams or logging lib such as co-log
runLines :: forall accum itm rslt. (LineNo -> accum -> Either AppError itm -> (accum, Either AppError (Maybe rslt)))  -- line processor / stepper
                                      -> (ByteString -> Either AppError itm)                               -- a parser for the item
                                      -> (rslt -> ByteString)                                              -- a serialiser for the result
                                      -> (AppError -> ByteString)                                          -- a serialiser for the error
                                      -> accum                                                             -- accumulator
                                      -> AbsFile                                                           -- input file
                                      -> Handle                                                            -- output handle
                                      -> IO (Either P.IOError ())
runLines step ipsr rsltSersr errSersr seed fileIn hOut = do 
                                                        eHIn <- safeOpenFile fileIn ReadMode
                                                        eitherf eHIn
                                                              (pure . Left)
                                                              (\hIn -> pure <$> finally (mainloop step ipsr rsltSersr errSersr hIn hOut (LineNo 1) seed) (hClose hIn))

{- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                            New Version                                                             %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -}

-- logTransform :: forall a itm rsltItem m. Monad m => 
logTransform :: forall a itm rsltItem m. Monad m =>
                                     m (Maybe ByteString)                 -- source
                                    -> (ByteString -> m ())                 -- sink
                                    -> (LineNo -> a -> Either AppError itm -> (a, Either AppError (Maybe rsltItem))) -- reducer step
                                    -> (ByteString -> Either AppError itm)  -- item desrialiser
                                    -> (rsltItem -> ByteString)             -- result serialiser
                                    -> (AppError -> ByteString)             -- error serialiser
                                    -> LineNo                               -- linNo
                                    -> a                                    -- accumulattor
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
             (nxtAccum, result) = step lineNo accum $ ipsr bs
            in
              do
                eitherf result
                  errorSnk
                  (maybe
                    (pure ())
                    lineSink                    
                  ) 
                localLoop (debug $ LineNo $ unLineNo lineNo + 1) nxtAccum
          )

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

testPrettyPrint2 :: DList ByteString -> DList ByteString
testPrettyPrint2 input = runToList input $ logTransform testSource testSink prettyPrintItem lpParser toS (toS . txt) (LineNo 1) ()

testTransform2 :: forall accum itm rslt. (LineNo -> accum -> Either AppError itm -> (accum, Either AppError (Maybe rslt))) -- reducer step
                                    -> (ByteString -> Either AppError itm)                                               -- a parser for the item
                                    -> (rslt -> ByteString)                                                         -- a serialiser for the result
                                    -> (AppError -> ByteString)                                                    -- a serialiser for the error
                                    -> accum                                                                        -- accumulator
                                    -> DList ByteString                                                             -- input lines
                                    -> DList ByteString                                                             -- reult lines
testTransform2 step iPsr rsltSersr errSersr seed lstIn = 
  let
    (source, remainder) = case lstIn of 
                            Nil -> (Nothing, D.empty)
                            Cons x xs -> (Just $ D.singleton x, fromList xs)
                            _ -> error "DList pattern match error this should never happen"

    -- sink :: ByteString -> m ()
    
    testParseStep :: accum 
                  -> DList ByteString
                  -> DList ByteString
                  -> LineNo
                  -> DList ByteString
    testParseStep accum inLst outLst lineNo = 
      case inLst of
        Nil -> outLst
        Cons x xs -> 
          let 
            (newAccm, stepLine) = mapLeft (AppParseError lineNo) <$> step lineNo accum (iPsr x)
            newLOut = eitherf stepLine
                        (D.snoc outLst . errSersr) 
                        ( \sl ->
                          maybef sl 
                            outLst
                            (D.snoc outLst . rsltSersr) 
                        )
          in 
            testParseStep newAccm (fromList xs) newLOut (LineNo . succ $ unLineNo lineNo)
        _ -> error "DList pattern match error this should never happen"
    in 
      testParseStep seed lstIn (fromList []) $ LineNo 1


--- type Step accum itm AppError rslt = (Int -> accum -> Either AppError itm -> (accum, Either AppError (Maybe rslt)))
type IParser itm = ByteString -> Either AppError itm

data IterationAccumulator = IterationAccumulator

data TestIteraion = Iteration |
                    OutOfIterationError AppError |
                    ParseError AppError
                    deriving (Show, Eq)

prettyPrintItem :: LineNo                                             -- lineNo
                    -> ()                                             -- accumulator
                    -> Either AppError LogProtocol                     -- line item
                    -> ((), Either AppError (Maybe Text))             -- (accum, result item)
prettyPrintItem lnNo _ ethLn = ((), Just . logStrPP False <$> ethLn)

iterationStep :: Int -> IterationAccumulator -> Either Text LogProtocol -> (IterationAccumulator, Either Text (Maybe TestIteraion))
iterationStep linNo accum = uu
  -- \case
  --                               Message Text            -> uu
  --                               Message' DetailedInfo |

  --                               Warning Text |
  --                               Warning' DetailedInfo |

  --                               IOAction Text |
  --                               DocIOAction Text |
  --                               DocAction DocActionInfo |
  --                               DocCheck ItemId Text ResultExpectation GateStatus | 
  --                               DocStartInteraction | 
  --                               DocStartChecks | 

  --                               InteractorSuccess ItemId ApStateDisplay |
  --                               InteractorFailure ItemId AppError |

  --                               PrepStateSuccess ItemId DStateDisplay |
  --                               PrepStateFailure ItemId AppError |

  --                               Error AppError |
  --                               FilterLog [FilterResult] |

  --                               StartRun RunTitle Value | 
  --                               EndRun |

  --                               StartGroup GroupTitle |
  --                               EndGroup GroupTitle |

  --                               StartTest TestDisplayInfo |
  --                               EndTest TestModule |

  --                               CheckOutcome ItemId CheckReport |

  --                               StartIteration ItemId WhenClause ThenClause Value | 
  --                               EndIteration ItemId 


jsnSerialise :: A.ToJSON v => v -> ByteString
jsnSerialise = toS . A.encode

lpParser :: ByteString -> Either AppError LogProtocol
lpParser bs = mapLeft (AppGenericError . toS) $ A.eitherDecode $ L.fromStrict bs

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

