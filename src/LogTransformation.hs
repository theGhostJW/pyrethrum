module LogTransformation where

import Common
import Foundation.Extended
import Foundation.Compat.ByteString
import System.IO
import qualified Prelude as P
import Basement.String
import AuxFiles
import DSL.LogProtocol
import Text.Show.Pretty as PP
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Char8 as B

-- TODO: re-implement with streams
runLines :: forall accum err itm rslt. Step accum itm err rslt 
                                      -> IParser err itm 
                                      -> ISerialiser rslt 
                                      -> ISerialiser err 
                                      -> accum 
                                      -> AbsFile 
                                      -> Handle
                                      -> IO ()
runLines step ipsr rsltSersr errSersr seed file hOut = safeOpenFile file ReadMode
                    >>= either 
                          P.print
                          (\hIn -> finally (hClose hIn) $ mainloop step ipsr rsltSersr errSersr hIn hOut 1 seed)

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
      output = B.hPutStrLn outh
    in
      hIsEOF inh >>=
                  bool
                      (pure ())
                      (
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
                          
                        localLoop (lineNo + 1) nxtAccum
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