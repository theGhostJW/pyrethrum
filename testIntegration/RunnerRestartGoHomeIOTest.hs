module RunnerRestartGoHomeIOTest where

import           Foundation.Extended as F

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
 