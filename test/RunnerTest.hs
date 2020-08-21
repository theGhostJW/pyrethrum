module RunnerTest where

import           DemoProject.TestCaseList
import           Pyrelude      as F
import           Pyrelude.Test
import           LogListCheck
import           RunnerBase
import           Runner
import           DSL.Interpreter
import           DemoProject.Config as CFG
import           Data.DList as D

import           DemoProject.Test.Rough as RT
import           DemoProject.Test.Rough2 as RT2
import           DemoProject.Test.Simple as ST
import DemoProject.Test.RoughIntState
import DemoProject.Test.RoughDisabled as DT
import  DemoProject.Test.Simple2 as ST2

failG2GoHomePlan :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
failG2GoHomePlan f =
  [
    Tests {
          header = "Group 2",
          rollover = doNothing,
          goHome = alwaysFailCheck,
          tests = [
              f RT2.test,
              f ST2.test
            ]
     }
    ]

runFailHomeG2IO :: IO ()
runFailHomeG2IO = runIO failG2GoHomePlan

failGroup2GoHomeLog :: DList Text
failGroup2GoHomeLog = listRaw failG2GoHomePlan

goHomeCheckMessage :: Text
goHomeCheckMessage = "GoHome action ran without exception but completion check returned False. Looks like GoHome did not run as expected"

rolloverCheckMessage :: Text
rolloverCheckMessage = "No tests run in group. Rollover action ran without exception but completion check returned False. Looks like Rollover did not run as expected"

-- the normal test run has not been configured with any failing checks
unit_all_prerun_success_no_go_home_check_error = chkMessageInstances rolloverCheckMessage 0 runDocument
unit_all_prerun_success_no_rollover_check_error = chkMessageInstances goHomeCheckMessage 0 runDocument

-- go home on all second group will fail 16 iterations
unit_go_home_iteration_check_fail = chkMessageInstances goHomeCheckMessage 16 failGroup2GoHomeLog

testRunFailRolloverG1 :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunFailRolloverG1 = validPlan alwaysFailCheck doNothing doNothing doNothing

runFailRolloverG1Document :: DList Text
runFailRolloverG1Document = listRaw testRunFailRolloverG1

unit_rollover_check_fail = chkMessageInstances rolloverCheckMessage 1 runFailRolloverG1Document

-----------------------------------------------------------------
---------------------- Duplicate Group Names --------------------
-----------------------------------------------------------------

emptyGroup :: Text -> RunElement m1 m a effs
emptyGroup hdr = Tests hdr doNothing doNothing []

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
docRunDuplicates = F.toList $ listRaw planWithDuplicates

unit_duplicate_group_name_config_error = 1 ... length docRunDuplicates
unit_duplicate_group_name_config_error_txt = chk  . isInfixOf "Test Run Configuration Error. Duplicate Group Names: Group 1" . txt $ unsafeHead docRunDuplicates

docRunNoDuplicates :: [Text]
docRunNoDuplicates = F.toList $ listRaw planWithNoDuplicates

unit_no_duplicate_group_name_config_error = chkFalse . isInfixOf "Test Run Configuration Error" $ unlines docRunNoDuplicates