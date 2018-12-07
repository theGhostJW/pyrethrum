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

chkLog :: (Log -> Bool) -> Log -> Assertion
chkLog p = U.chk . p


count :: (Foldable collection, Truthy b, Additive a, P.Num a) => (Element collection -> b) -> collection -> a
count p = foldl' (\n x -> p x ? n + 1 $ n) 0

goHomeMessage :: String
goHomeMessage = "GoHome action ran without exception but completion check returned False. Looks like GoHome did not run as expected"



-- unit_check_expected_go_home_failures = chkLog
