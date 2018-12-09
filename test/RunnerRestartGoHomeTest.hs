module RunnerRestartGoHomeTest where

import qualified Check                        as C
import           DemoProject.DemoTestCaseList
import           DSL.Interpreter
import           Foundation.Collection
import           Foundation.Extended          as F
import           Foundation.List.DList
import           ItemClass
import qualified Prelude                      as P
import           Runner                       as R
import           Runner.Internal
import           RunnerShared
import           Test.Extended                as U
import           TestAndRunConfig

type Log = DList String

chkLog :: (Log -> v) -> (v -> Assertion) -> Log -> Assertion
chkLog intprt assrt = assrt . intprt

goHomeMessage :: String
goHomeMessage = "GoHome action ran without exception but completion check returned False. Looks like GoHome did not run as expected"

isGoHomeMessage :: String -> Bool
isGoHomeMessage = isInfixOf goHomeMessage

unit_go_home_iteration_fail = chkLog (count isGoHomeMessage) (chkEq 14) runFailHomeG2Document
