module RunnerRestartGoHomeTest where

import           DemoProject.TestCaseList
import           Pyrelude      as F
import           Pyrelude.Test
import           LogListCheck
import           RunnerBase
import           Runner
import           DemoProject.Config
import           DSL.Interpreter 

goHomeCheckMessage :: Text
goHomeCheckMessage = "GoHome action ran without exception but completion check returned False. Looks like GoHome did not run as expected"

rolloverCheckMessage :: Text
rolloverCheckMessage = "No tests run in group. Rollover action ran without exception but completion check returned False. Looks like Rollover did not run as expected"

unit_all_prerun_success_no_go_home_check_error = chkMessageInstances rolloverCheckMessage 0 runDocument
unit_all_prerun_success_no_rollover_check_error = chkMessageInstances goHomeCheckMessage 0 runDocument

-- go home on all second group will fail 16 iterations
unit_go_home_iteration_check_fail = chkMessageInstances goHomeCheckMessage 16 runFailHomeG2Document

unit_rollover_check_fail = chkMessageInstances rolloverCheckMessage 1 runFailRolloverG1Document

-----------------------------------------------------------------
---------------------- Duplicate Group Names --------------------
-----------------------------------------------------------------

emptyGroup :: Text -> TestGroup m1 m a effs
emptyGroup hdr = TestGroup hdr doNothing doNothing []

planWithDuplicates :: forall m m1 effs a. TestPlan m1 m a effs
planWithDuplicates f = [
    emptyGroup "Group 1",
    emptyGroup "Group 2",
    emptyGroup "Group 1"
  ]

planWithNoDuplicates :: forall m m1 effs a. TestPlan m1 m a effs
planWithNoDuplicates f = [
    emptyGroup "Group 1",
    emptyGroup "Group 2",
    emptyGroup "Group 3"
 ]

docRunDuplicates :: [Text]
docRunDuplicates = toList $ docRunRaw planWithDuplicates

unit_duplicate_group_name_config_error = 1 ... length docRunDuplicates
unit_duplicate_group_name_config_error_txt = chk  . isInfixOf "Test Run Configuration Error. Duplicate Group Names: Group 1" . txt $ unsafeHead docRunDuplicates

docRunNoDuplicates :: [Text]
docRunNoDuplicates = toList $ docRunRaw planWithNoDuplicates

unit_no_duplicate_group_name_config_error = chkFalse . isInfixOf "Test Run Configuration Error" $ unlines docRunNoDuplicates