module LogTransformationIntegrationTest where 

import           Pyrelude as E
import           Pyrelude.IO
import Pyrelude.Test      as T
import AuxFiles
import LogTransformation
import LogTransformation.Common


type FileAggregator = AbsFile                                            -- source file
                    -> (forall m. MonadThrow m => AbsFile -> m AbsFile)   -- destFileFunc
                    -> IO (Either LogTransformError AbsFile)              -- dest file path or error 

runAgregator :: FileAggregator ->  IO ()
runAgregator fa = do 
                  eFile <- jsoniFile 
                  eitherf eFile
                    (chkFail . txt)
                    (\inputFile ->
                      do
                        pth <- fa inputFile (-<.> ".log")                                          -- source file
                        putStrLn $ txt pth
                        chk $ isRight pth   
                    )

unit_demo_prettyPrint_integration :: IO ()
unit_demo_prettyPrint_integration = runAgregator testPrettyPrintFile

unit_demo_itemAggregator_integration :: IO ()
unit_demo_itemAggregator_integration = runAgregator testIterationStepFile

jsoniFile :: IO (Either IOError AbsFile)
jsoniFile = dataFile [relfile|demo_raw_log.ijson|]