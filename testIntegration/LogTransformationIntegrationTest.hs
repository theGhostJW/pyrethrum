module LogTransformationIntegrationTest where 

import           Pyrelude as E
import           Pyrelude.IO
import Pyrelude.Test      as T
import AuxFiles
import LogTransformation


-- unit_demo_integration :: IO ()
-- unit_demo_integration = do 
--                           eFile <- jsoniFile 
--                           eitherf eFile
--                             (chkFail . txt)
--                             (\inputFile ->
--                               do
--                                 pth <- testPrettyPrintFile inputFile (-<.> ".log")                                          -- source file
--                                 putStrLn $ txt pth
--                                 chk $ isRight pth   
--                             )

jsoniFile :: IO (Either IOError AbsFile)
jsoniFile = dataFile [relfile|demo_raw_log.ijson|]