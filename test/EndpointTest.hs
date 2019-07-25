module EndpointTest where

import           DemoProject.Config
import           DemoProject.Test.Rough   as T
import           DemoProject.TestCaseList
import           Pyrelude      as F
import           LogListCheck
import           Runner                   as R

endPointLog = testEndpointDoc "DemoProject.Test.Rough" runConfig (filterredItemIds (IID 110) (T.items runConfig)) testG1GoHomeLogging

runActionMsg = "Run Action"
checkActionMsg = "Check Action Run"
startIterationStr = "StartIteration"

-- once before group deciding not to run rollover
-- once before iteration - deciding not to run goHome
unit_endpoint_already_home_check_home_run_twice = chkMessageInstances checkActionMsg 2 endPointLog

unit_endpoint_already_home_go_home_action_not_run = chkMessageInstances runActionMsg 0 endPointLog

unit_endpoint_single_iteration_only = chkMessageInstances startIterationStr 1 endPointLog

endPointLogFailHomeCheck = testEndpointDoc 
                                "DemoProject.Test.Rough" 
                                runConfig 
                                (filterredItemIds (IID 110) (T.items runConfig)) 
                                testG1GoHomeLoggingFailCheck

-- go home check - once before group deciding to run rollover          | False
-- rollove check - once to check rollover success                      | True
-- go home check - once before iteration to decide if to run go home   | False
-- go home check - once after go home                                  | False
-- -> will fail due to go home failure
unit_endpoint_already_home_check_home_run_four_times = chkMessageInstances checkActionMsg 4 endPointLogFailHomeCheck

unit_endpoint_not_home_rollover_and_goHome_run = chkMessageInstances runActionMsg 2 endPointLogFailHomeCheck

unit_endpoint_no_iteration_run_when_gohome_fails = chkMessageInstances startIterationStr 0 endPointLogFailHomeCheck
