module EndToEndSanityTest where 

import           Pyrelude as E
import           Pyrelude.IO
import Pyrelude.Test      as T
import AuxFiles
import LogTransformation.Common
import DSL.LogProtocol
import Common
import DemoProject.TestCaseList
import DemoProject.Config
import DSL.LogProtocol.PrettyPrint

-- Helpers --

fullLog :: IO ([LogProtocol], Either AppError ())
fullLog = runToLPList

logAllways = True

chkFullLog :: (([LogProtocol], Either AppError ()) -> Bool) -> (([LogProtocol], Either AppError ()) -> Text) -> IO ()
chkFullLog prd msgFunc = 
  do 
    lg <- fullLog
    let 
      success' = prd lg 
    unless (success' && not logAllways) (
      do 
        putStrLn (success' ? "----- test log ------" $ "----- test failure ------")
        toTemp . unlines $ prettyPrintLogProtocol False <$> fst lg 
        putStrLn "-----------"
     )
    chk' (msgFunc lg) success'

chkRunRslt :: (Either AppError () -> Bool) -> Text -> IO ()
chkRunRslt prd msg =
    chkFullLog  (prd . snd) (\flg -> msg <> "\n" <> txt (snd flg))

chkLog :: ([LogProtocol] -> Bool) -> Text -> IO ()
chkLog prd msg =
    chkFullLog (prd . fst) (const (msg <> " - see linked file for log details"))

-- Tests --

unit_runs_without_error = chkRunRslt isRight "Error in run result should be Right"

unit_records_expected_prepstate_failures = 
  let 
    isPrepFailure :: LogProtocol -> Bool 
    isPrepFailure = \case 
                        IterationLog (Run (PrepStateFailure _ _)) -> True
                        _ -> False
  in
    chkLog (\lg -> 2 == count isPrepFailure lg) "expect 2 prep failures 1 for each Rough test iteration 110"
    
      