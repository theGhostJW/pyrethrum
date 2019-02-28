module LogTransformation where

import Common
import Foundation.Extended as E
import Foundation.Compat.ByteString
import qualified Prelude as P
import Basement.String
import AuxFiles
import DSL.LogProtocol
import Text.Show.Pretty as PP
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import System.IO as S
import Data.Functor

-- TODO: re-implement with streams
runLines :: forall accum err itm rslt. Step accum itm err rslt 
                                      -> IParser err itm 
                                      -> ISerialiser rslt 
                                      -> ISerialiser err 
                                      -> accum 
                                      -> AbsFile 
                                      -> Handle
                                      -> IO ()
runLines step ipsr rsltSersr errSersr seed file hOut = do 
                                                        eHIn <- safeOpenFile file ReadMode
                                                        eitherf eHIn
                                                              P.print
                                                              (\hIn -> P.print "Hello from RunLines" *> finally (mainloop step ipsr rsltSersr errSersr hIn hOut 1 seed) (hClose hIn))

mainloop :: forall accum err itm rslt. Step accum itm err rslt 
                                    -> IParser err itm 
                                    -> ISerialiser rslt 
                                    -> ISerialiser err 
                                    -> Handle 
                                    -> Handle 
                                    -> Int 
                                    -> accum 
                                    -> IO ()
mainloop step ipsr rsltSersr errSersr inh outh lineNo accum =
    let 
      localLoop :: Int -> accum -> IO () 
      localLoop = mainloop step ipsr rsltSersr errSersr inh outh 

      output :: B.ByteString -> IO ()
      output bs = debug' "putlnstr" <$> B.hPutStrLn outh bs
    in
      P.print ("Hello from MainLoop " <> show lineNo) *>
      hIsEOF inh >>=
                  bool
                      ( -- not EOF -> has data
                        do 
                          P.print "PROCESSING LINE"
                          byteLine <- B.hGetLine inh
                          let 
                            (nxtAccum, result) = step lineNo accum (ipsr byteLine)
                          
                          eitherf result
                            (output . errSersr)
                            (maybe
                              (debug' "NOTHING" <$> pure ())
                              (output . debug' "HAS LINE" . rsltSersr)
                            )
                            
                          localLoop (lineNo + 1) nxtAccum
                      )
                      (do 
                        fSize <- hFileSize inh
                        eof <- hIsEOF inh
                        P.print $ "Done hIsEOF: " <> show inh
                        P.print $ "Size: " <> show fSize
                        pure ()
                      )
            
type Step accum itm err rslt = Int -> accum -> Either err itm -> (accum, Either err (Maybe rslt))
type ISerialiser src = src -> ByteString
type IParser err itm = ByteString -> Either err itm

data IterationAccumulator = IterationAccumulator

data TestIteraion = Iteration |
                    OutOfIterationError AppError |
                    ParseError AppError
                    deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestIteraion)

iterationStep :: Step IterationAccumulator LogProtocol String TestIteraion
iterationStep linNo accum lp = (accum, Right $ Just Iteration)

itrSerialise :: ISerialiser TestIteraion
itrSerialise = toS . A.encode

strSerialise :: ISerialiser String
strSerialise = toByteString . toBytes UTF8 

lpParser :: ByteString -> Either String LogProtocol
lpParser bs = mapLeft toS $ A.eitherDecode $ toS bs

summariseIterations :: AbsFile -> IO String
summariseIterations inputLog = 
                              let
                                seed :: IterationAccumulator 
                                seed = uu

                                processLines :: Handle -> IO ()
                                processLines h = debug' "RUN LINES" <$> runLines iterationStep lpParser itrSerialise strSerialise seed (debug' "INPUT LOG" inputLog) h
                              in
                                do 
                                  itrFile <- inputLog -<.> ".itr"
                                  outHndle <- safeOpenFile itrFile S.WriteMode
                                  eitherf outHndle
                                    (pure . show . debug' "FAILED OPEN")
                                    (\h -> finally (processLines h) (S.hClose h) $> (toS . toFilePath $ itrFile))