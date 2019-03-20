module RunnerRestartGoHomeTest where

import           DemoProject.TestCaseList
import           Pyrelude      as F
import           LogListCheck

goHomeCheckMessage :: Text
goHomeCheckMessage = "GoHome action ran without exception but completion check returned False. Looks like GoHome did not run as expected"

rolloverCheckMessage :: Text
rolloverCheckMessage = "No tests run in group. Rollover action ran without exception but completion check returned False. Looks like Rollover did not run as expected"

unit_all_prerun_success_no_go_home_check_error = chkMessageInstances rolloverCheckMessage 0 runDocument
unit_all_prerun_success_no_rollover_check_error = chkMessageInstances goHomeCheckMessage 0 runDocument

unit_go_home_iteration_check_fail = chkMessageInstances goHomeCheckMessage 12 runFailHomeG2Document
unit_rollover_check_fail = chkMessageInstances rolloverCheckMessage 1 runFailRolloverG1Document
