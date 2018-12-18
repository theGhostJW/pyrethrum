module EndpointTest where

import qualified Check                    as C
import           DemoProject.Config
import           DemoProject.Test.Rough   as T
import           DemoProject.TestCaseList
import           DSL.Interpreter
import           Foundation.Collection
import           Foundation.Extended      as F
import           Foundation.List.DList
import           Foundation.String
import           ItemClass
import           LogListCheck
import qualified Prelude                  as P
import           Runner                   as R
import           Test.Extended            as U

endPointLog = testEndpointDoc "DemoProject.Test.Rough" runConfig (filterredItemIds (IID 110) T.items) testG1GoHomeLogging

runActionMsg = "Run Action"
checkActionMsg = "Check Action Run"

-- once before group deciding not to run rollover
-- once before iteration - deciding not to run goHome
unit_endpoint_already_home_check_home_run_once = chkMessageInstances checkActionMsg 2 endPointLog

unit_endpoint_already_home_go_home_action_not_run = chkMessageInstances runActionMsg 0 endPointLog
