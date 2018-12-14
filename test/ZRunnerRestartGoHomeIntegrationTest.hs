module ZRunnerRestartGoHomeIntegrationTest where

import qualified Check                    as C
import           DemoProject.TestCaseList
import           DSL.Interpreter
import           Foundation.Collection
import           Foundation.Extended      as F
import           Foundation.List.DList
import           Foundation.String
import qualified Prelude                  as P
import           Runner                   as R
import           RunnerRestartCommon
import           Test.Extended            as U


goHomeExceptionMessage :: String
goHomeExceptionMessage = "PreTestCheckExecutionError GoHome \"No items run for test. Execution of GoHome check\" (IOError user error (Pretend IO Error))"

-- unit_go_home_iteration_fail = do
--                                 (log, _) <- capture runExceptG2GoHomeCheckIO
--                                 chkMessageInstances goHomeExceptionMessage 12 $ {-debug $ -}fromList . lines $ toStr log
--
-- unit_rollover_group_fail = do
--                              (log, _) <- capture runExceptG1Rollover
--                              chkMessageInstances
--                                   "PreTestError Rollover \"Execution of Rollover\" (IOError user error (Pretend IO Error))"
--                                   1
--                                   $ fromList . lines $ toStr log
