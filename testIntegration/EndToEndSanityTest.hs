module EndToEndSanityTest where 

import           Pyrelude as E
import           Pyrelude.IO
import Pyrelude.Test      as T
import AuxFiles
import LogTransformation.Common
import DSL.LogProtocol
import Common


-- fullLog :: IO ([LogProtocol], Either AppError ())
-- fullLog = runToLPList


-- unit_all_prerun_success_no_go_home_check_error = chkMessageInstances rolloverCheckMessage 0 runDocument
