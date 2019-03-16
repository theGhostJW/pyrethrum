module LogTransformation where

import Common
import Foundation.Extended as E
import Foundation.List.DList
import Foundation.Compat.ByteString
import qualified Prelude as P
import Basement.String
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
                                    -> (ByteString -> Either err itm)                                           -- a parser for the item
                                    -> (rslt -> ByteString)                                                     -- a serialiser for the result
                                    -> (err -> ByteString)                                                      -- a serialiser for the error
                                    -> accum                                                                    -- accumulator
                                    -> DList ByteString                                                         -- input lines
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
        case E.uncons inL of
          Nothing -> pure outL
          Just (x, xs) -> 
            let 
              (newAccm, stepLine) = step lineNo accum (iPsr x)
            in 
              do
                sl <- stepLine
                let 
                  newLOut = maybef sl 
                              outL
                              (E.snoc outL . rsltSersr) 
                testParseStep newAccm (Right xs) (Right newLOut) (LineNo . succ $ unLineNo lineNo) 
    in 
      testParseStep seed (Right lstIn) (Right $ fromList []) $ LineNo 1


-- TODO: re-implement with streams or logging lib such as co-log
runLines :: forall accum err itm rslt. (LineNo -> accum -> Either err itm -> (accum, Either err (Maybe rslt)))  -- line processor / stepper
                                      -> (ByteString -> Either err itm)                                         -- a parser for the item
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
                                                              (\hIn -> pure <$> finally (mainloop step ipsr rsltSersr errSersr hIn hOut 1 seed) (hClose hIn))

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
                      (do 
                        fSize <- hFileSize inh
                        eof <- hIsEOF inh
                        pure ()
                      )
            
--- type Step accum itm err rslt = (Int -> accum -> Either err itm -> (accum, Either err (Maybe rslt)))
type IParser err itm = ByteString -> Either err itm

data IterationAccumulator = IterationAccumulator

data TestIteraion = Iteration |
                    OutOfIterationError AppError |
                    ParseError AppError
                    deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestIteraion)

prettyPrintItem :: LineNo -> () -> Either AppError ByteString -> ((), Either AppError (Maybe String))
prettyPrintItem lnNo _ ethLn = 
  let
    connvertedLine :: Either AppError (Maybe String)
    connvertedLine = eitherf ethLn 
                            Left 
                            (\bs ->
                              let 
                                valMaybe :: Maybe LogProtocol
                                valMaybe = A.decode $ L.fromStrict bs

                                lineAsString :: String
                                lineAsString = fst . fromBytesLenient $ fromByteString bs
                              in 
                                maybef valMaybe
                                 (Left $ AppGenericError $ "Failed to decode JSON line: " <> show (unLineNo lnNo) <> "(check file full line might not be displayed) : " <> lineAsString)
                                 (Right . Just . logStrPP False)
                            )
  in 
    ((), connvertedLine)

iterationStep :: Int -> IterationAccumulator -> Either String LogProtocol -> (IterationAccumulator, Either String (Maybe TestIteraion))
iterationStep linNo accum = uu
  -- \case
  --                               Message String            -> uu
  --                               Message' DetailedInfo |

  --                               Warning String |
  --                               Warning' DetailedInfo |

  --                               IOAction String |
  --                               DocIOAction String |
  --                               DocAction DocActionInfo |
  --                               DocCheck ItemId String ResultExpectation GateStatus | 
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

strSerialise :: String -> ByteString
strSerialise = toByteString . toBytes UTF8 

lpParser :: ByteString -> Either String LogProtocol
lpParser bs = mapLeft toS $ A.eitherDecode $ toS bs

summariseIterations :: AbsFile -> IO String
summariseIterations inputLog = 
                              let
                                seed :: IterationAccumulator 
                                seed = uu

                                processLines :: Handle -> IO ()
                                processLines = runLines iterationStep lpParser itrSerialise strSerialise seed inputLog
                              in
                                do 
                                  itrFile <- inputLog -<.> ".itr"
                                  outHndle <- safeOpenFile itrFile S.WriteMode
                                  eitherf outHndle
                                    (pure . show)
                                    (\h -> finally (processLines h) (S.hClose h) $> (toS . toFilePath $ itrFile))