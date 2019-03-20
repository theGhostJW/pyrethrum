module LogTransformationIntegrationTest where 

import           Pyrelude as E
import qualified Prelude             as P
import Pyrelude.Test      as T
import AuxFiles
import Control.Monad
import LogTransformation

-- TODO: reinstate
-- unit_demo :: IO ()
-- unit_demo = do 
--                 eFile <- jsoniFile 
--                 eItems <- P.traverse summariseIterations eFile
--                 chk $ isRight eItems  
--                 chk ("" /= fromRight' eItems)

jsoniFile :: IO (Either P.IOError AbsFile)
jsoniFile = dataFile [relfile|demo_raw_log.ijson|]