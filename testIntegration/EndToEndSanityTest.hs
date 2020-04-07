module EndToEndSanityTest where 

import           Pyrelude as E
import           Pyrelude.IO
import Pyrelude.Test      as T
import AuxFiles
import LogTransformation.Common
import DSL.LogProtocol
import Common
import DemoProject.TestCaseList

-- Helpers --

fullLog :: IO ([LogProtocol], Either AppError ())
fullLog = runToLPList

chkFullLog :: (([LogProtocol], Either AppError ()) -> Bool) -> (([LogProtocol], Either AppError ()) -> Text) -> IO ()
chkFullLog prd msgFunc = 
  do 
    lg <- fullLog
    let 
      success' = prd lg 
    unless success' (
      do 
        putStrLn "----- test failure ------"
        toTempFromList $ fst lg 
        putStrLn "-----------"
     )
    chk' (msgFunc lg) success'

chkRunRslt :: (Either AppError () -> Bool) -> Text -> IO ()
chkRunRslt prd msg =
  let 
    prd' = prd . snd
    msgF flg = msg <> "\n" <> txt (snd flg)
  in 
    chkFullLog prd' msgF

-- Tests --

unit_runs_without_error = chkRunRslt isRight "Error in run result should be Right"