module LogTransformation where

import Common
import Pyrelude as E
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

newtype LineNo = LineNo { unLineNo :: Int }


transformToFile :: forall itm rslt err accum.                          
                (ByteString -> Either err itm)                                              -- a parser for the item
                -> (rslt -> ByteString)                                                     -- a serialiser for the result
                -> (err -> ByteString)                                                      -- a serialiser for the error
                -> (LineNo -> accum -> Either err itm -> (accum, Either err (Maybe rslt)))  -- line stepper
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

testTransform :: forall accum err itm rslt. (LineNo -> accum -> Either err itm -> (accum, Either err (Maybe rslt))) -- reducer step
                                    -> (ByteString -> Either err itm)                                               -- a parser for the item
                                    -> (rslt -> ByteString)                                                         -- a serialiser for the result
                                    -> (err -> ByteString)                                                          -- a serialiser for the error
                                    -> accum                                                                        -- accumulator
                                    -> DList ByteString                                                             -- input lines
                                    -> Either err (DList ByteString)
testTransform step iPsr rsltSersr errSersr seed lstIn = 
  let
    testParseStep :: accum 
                  -> Either err (DList ByteString)
                  -> Either err (DList ByteString)
                  -> LineNo
                  -> Either err (DList ByteString)
    testParseStep accum inLst outLst lineNo = 
      do 
        inL <- inLst
        outL <- outLst
        case inL of
          Nil -> pure outL
          Cons x xs -> 
            let 
              (newAccm, stepLine) = step lineNo accum (iPsr x)
            in 
              do
                sl <- stepLine
                let 
                  newLOut = maybef sl 
                              outL
                              (D.snoc outL . rsltSersr) 
                testParseStep newAccm (Right $ fromList xs) (Right newLOut) (LineNo . succ $ unLineNo lineNo)
          _ -> error "DList pattern match error this should never happen"
    in 
      testParseStep seed (Right lstIn) (Right $ fromList []) $ LineNo 1


-- TODO: re-implement with streams or logging lib such as co-log
runLines :: forall accum err itm rslt. (LineNo -> accum -> Either err itm -> (accum, Either err (Maybe rslt)))  -- line processor / stepper
                                      -> (ByteString -> Either err itm)                               -- a parser for the item
                                      -> (rslt -> ByteString)                                                   -- a serialiser for the result
                                      -> (err -> ByteString)                                                    -- a serialiser for the error
                                      -> accum                                                                  -- accumulator
                                      -> AbsFile                                                                -- input file
                                      -> Handle                                                                 -- output handle
                                      -> IO (Either P.IOError ())
runLines step ipsr rsltSersr errSersr seed fileIn hOut = do 
                                                        eHIn <- safeOpenFile fileIn ReadMode
                                                        eitherf eHIn
                                                              (pure . Left)
                                                              (\hIn -> pure <$> finally (mainloop step ipsr rsltSersr errSersr hIn hOut (LineNo 1) seed) (hClose hIn))

mainloop :: forall accum err itm rslt. (LineNo -> accum -> Either err itm -> (accum, Either err (Maybe rslt))) -- reducer step
                                    -> (ByteString -> Either err itm)
                                    -> (rslt -> ByteString)   
                                    -> (err -> ByteString)
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
            
--- type Step accum itm err rslt = (Int -> accum -> Either err itm -> (accum, Either err (Maybe rslt)))
type IParser err itm = ByteString -> Either err itm

data IterationAccumulator = IterationAccumulator

data TestIteraion = Iteration |
                    OutOfIterationError AppError |
                    ParseError AppError
                    deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestIteraion)

-- prettyPrintItem :: LineNo -> () -> Either AppError ByteString -> ((), Either AppError (Maybe Text))
-- prettyPrintItem lnNo _ ethLn = 
--   let
--     connvertedLine :: Either AppError (Maybe Text)
--     connvertedLine = eitherf ethLn 
--                             Left 
--                             (\bs ->
--                               let 
--                                 valMaybe :: Maybe LogProtocol
--                                 valMaybe = A.decode $ L.fromStrict bs

--                                 lineAsString :: Text
--                                 lineAsString = fst . fromBytesLenient $ fromByteString bs
--                               in 
--                                 maybef valMaybe
--                                  (Left $ AppGenericError $ "Failed to decode JSON line: " <> show (unLineNo lnNo) <> "(check file full line might not be displayed) : " <> lineAsString)
--                                  (Right . Just . logStrPP False)
--                             )
--   in 
--     ((), connvertedLine)

prettyPrintItem :: LineNo -> () -> Either AppError LogProtocol -> ((), Either AppError (Maybe Text))
prettyPrintItem _ _ ethLn = 
  let
    connvertedLine :: Either AppError (Maybe Text)
    connvertedLine = pure <$> (logStrPP False <$> ethLn)
  in 
    ((), connvertedLine)

testPrettyPrint :: DList ByteString -> Either AppError (DList ByteString)
testPrettyPrint =  testTransform
                            prettyPrintItem    -- reducer step
                            lpParser           -- a parser for the item
                            encodeUtf8         -- a serialiser for the result
                            (encodeUtf8 . txt) -- a serialiser for the error
                            ()                 -- accumulator
  
-- testTransform prettyPrintItem lpParser encodeUtf8 _ ()
-- testPrettyPrint = testTransform prettyPrintItem lpParser (toByteString . toBytes UTF8) (toByteString . toBytes UTF8 . show) ()

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