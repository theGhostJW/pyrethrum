module ArbitraryIOPTest where

import           DemoProject.TestCaseList
import           LogListCheck


fullLogRun = runDocument

-- RoughTest which is copied runs once on each iteration 10 iterations x 2 copies of test
unit_arbituary_io_is_logged_when_run_in_doc = chkMessageInstances "This is an arbitrary Put Line" 20 fullLogRun

-- this is the string that is put to line it should not appear in log
unit_arbituary_no_io_action_when_run_doc = chkMessageInstances "Hello from random action" 0 fullLogRun
