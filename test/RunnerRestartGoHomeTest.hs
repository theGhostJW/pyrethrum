module RunnerRestartGoHomeTest where

import qualified Check                    as C
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
import           TestAndRunConfig


goHomeCheckMessage :: String
goHomeCheckMessage = "GoHome action ran without exception but completion check returned False. Looks like GoHome did not run as expected"

rolloverCheckMessage :: String
rolloverCheckMessage = "No tests run in group. Rollover action ran without exception but completion check returned False. Looks like Rollover did not run as expected"

unit_all_prerun_success_no_go_home_check_error = chkMessageInstances rolloverCheckMessage 0 runDocument
unit_all_prerun_success_no_rollover_check_error = chkMessageInstances goHomeCheckMessage 0 runDocument

unit_go_home_iteration_check_fail = chkMessageInstances goHomeCheckMessage 12 runFailHomeG2Document
unit_rollover_check_fail = chkMessageInstances rolloverCheckMessage 1 runFailRolloverG1Document
