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

endPointLog = testEndpointDoc "DemoProject.Test.Rough" runConfig (filterredItemIds (IID 120) T.items) testG1GoHomeLogging

runActionMsg = "Run Action"
checkActionMsg = "Check Action Run"

unit_check_home_only_on_home_restart_check_run_once = chkMessageInstances checkActionMsg 1 endPointLog
unit_no_restart_when_home_already = chkMessageInstances runActionMsg 0 endPointLog
