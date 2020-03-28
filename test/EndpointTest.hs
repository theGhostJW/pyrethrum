module EndpointTest where

import           DemoProject.Config as C
import           DemoProject.Test.Rough   as T
import           DemoProject.TestCaseList
import           LogListCheck
import           Runner                 as R
import           Pyrelude
import           Pyrelude.IO
import           DSL.Interpreter
import           Common
import           DSL.Logger
import           DemoProject.Test.Rough as RT
import           DemoProject.Test.Rough2 as RT2
import           DemoProject.Test.Simple as ST
import DemoProject.Test.RoughIntState
import DemoProject.Test.RoughDisabled as DT
import  DemoProject.Test.Simple2 as ST2

justLogPreRun :: EFFLogger effs => PreTestStage -> PreRun effs
justLogPreRun stage = PreRun {
  runAction = log $ "Run Action: " <> txt stage,
  checkHasRun = log ("Check Action Run: " <> txt stage) $> True
}

justLogPreRunFailCheck :: EFFLogger effs => PreTestStage -> PreRun effs
justLogPreRunFailCheck stage = PreRun {
  runAction = log $ "Run Action: " <> txt stage,
  checkHasRun = log ("Check Action Run: " <> txt stage) $> False
}

testPlan :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testPlan f =
  [

    TestGroup {
           header = "Group 1",
           rollover = justLogPreRun Rollover,
           goHome = justLogPreRun GoHome, -- checkHasRun returns true so thinks its already home
           tests = [
               f RT.test,
               f DT.test,
               f ST.test,
               f DemoProject.Test.RoughIntState.test
             ]
      },

    TestGroup {
          header = "Group 2",
          rollover = justLogPreRun Rollover,
          goHome = justLogPreRunFailCheck GoHome,
          tests = [
              f RT2.test,
              f ST2.test
            ]
     }
    ]

endPointLog = 
  testEndpointDoc "DemoProject.Test.Rough" 
                  C.runConfig 
                  (filterredItemIds (IID 110) (T.items C.runConfig)) 
                  testPlan

endPointTextLog :: [Text]
endPointTextLog = toList endPointLog

runActionMsg = "Run Action"
checkActionMsg = "Check Action Run"
startIterationStr = "StartIteration"

-- once before group deciding not to run rollover ~ a feature of endpoints
-- once before iteration - deciding not to run goHome
unit_endpoint_already_home_check_home_run_twice = chkMessageInstances checkActionMsg 2 endPointLog

-- goHome should never be run because it is already home
unit_endpoint_already_home_go_home_action_not_run = chkMessageInstances runActionMsg 0 endPointLog

-- only one iteration matches the filter IID 110
unit_endpoint_single_iteration_only = chkMessageInstances startIterationStr 1 endPointLog



endPointLogFailHomeCheck = testEndpointDoc 
                                "DemoProject.Test.Rough2" 
                                runConfig 
                                (filterredItemIds (IID 110) (RT.items runConfig)) 
                                testPlan

-- go home check - once before group deciding to run rollover          | False
-- rollove check - once to check rollover success                      | True
-- go home check - once before iteration to decide if to run go home   | False
-- go home check - once after go home                                  | False
-- -> will fail due to go home failure
unit_endpoint_already_home_check_home_run_four_times = chkMessageInstances checkActionMsg 4 endPointLogFailHomeCheck

unit_endpoint_not_home_rollover_and_goHome_run = chkMessageInstances runActionMsg 2 endPointLogFailHomeCheck

unit_endpoint_no_iteration_run_when_gohome_fails = chkMessageInstances startIterationStr 0 endPointLogFailHomeCheck
