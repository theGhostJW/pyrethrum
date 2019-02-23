module LogTransformation where

import Common
import Foundation.Extended
import System.IO
import qualified Prelude as P
import Basement.String
import AuxFiles
import DSL.LogProtocol
import Text.Show.Pretty as PP
import qualified Data.Aeson as A

-- TODO: re-implement with streams
runLines :: Step accum itm rslt -> IParser itm -> Printer rslt -> accum -> AbsFile -> IO ()
runLines step ipsr prnter seed file = safeOpenFile file ReadMode
                    >>= either 
                          P.print
                          (\hIn -> mainloop step ipsr prnter hIn stdout 1 seed)

mainloop :: forall accum itm rslt. Step accum itm rslt -> IParser itm -> Printer rslt -> Handle -> Handle -> Int -> accum -> IO ()
mainloop step ipsr prnter inh outh lineNo accum =
    let 
      localLoop :: Int -> accum -> IO () 
      localLoop = mainloop step ipsr prnter inh outh 
    in
      hIsEOF inh >>=
                  bool
                      (pure ())
                      (
                      do 
                        inpStr <- hGetLine inh
                        let 
                          (nxtAccum, result) = step lineNo accum $ ipsr inpStr
                        
                        maybef result
                          (pure ())
                          (hPutStrLn outh . prnter)
                          
                        localLoop (lineNo + 1) nxtAccum
                      )
            

type Step accum itm rslt = Int -> accum -> itm -> (accum, Maybe rslt)
type Printer rslt = rslt -> P.String
type IParser itm = P.String -> itm

data TestIteraion = Iteration |
                    OutOfIterationError AppError |
                    ParseError AppError
                    deriving (Show, Eq)

iterationStep :: Int -> String -> LogProtocol -> (String, Maybe TestIteraion)
iterationStep linNo accum lp = ("Test", Just Iteration)

itrPrint :: TestIteraion -> P.String
itrPrint = ppShow

-- lpParser :: P.String -> LogProtocol
-- lpParser = _ --A.decode . toList