module RunnerRestartGoHomeTest where

import qualified Check                    as C
import           DemoProject.TestCaseList
import           DSL.Interpreter
import           Foundation.Collection
import           Foundation.Extended      as F
import           Foundation.List.DList
import           ItemClass
import qualified Prelude                  as P
import           Runner                   as R
import           Test.Extended            as U
import           TestAndRunConfig

type Log = DList String

chkLog :: (Log -> v) -> (v -> Assertion) -> Log -> Assertion
chkLog intprt assrt = assrt . intprt

goHomeCheckMessage :: String
goHomeCheckMessage = "GoHome action ran without exception but completion check returned False. Looks like GoHome did not run as expected"

rolloverCheckMessage :: String
rolloverCheckMessage = "No tests run in group. Rollover action ran without exception but completion check returned False. Looks like Rollover did not run as expected"

chkMessageInstances :: String -> Int -> DList String -> Assertion
chkMessageInstances msg exCount  = chkLog (count (isInfixOf msg)) (chkEq exCount)

unit_all_prerun_success_no_go_home_check_error = chkMessageInstances rolloverCheckMessage 0 runDocument
unit_all_prerun_success_no_rollover_check_error = chkMessageInstances goHomeCheckMessage 0 runDocument

unit_go_home_iteration_check_fail = chkMessageInstances goHomeCheckMessage 12 runFailHomeG2Document
unit_rollover_check_fail = chkMessageInstances rolloverCheckMessage 1 runFailRolloverG1Document
