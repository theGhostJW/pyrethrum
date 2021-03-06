module LogTransformationIntegrationTest where 

import           Pyrelude as E
import           Pyrelude.IO
import Pyrelude.Test      as T
import AuxFiles
import LogTransformation.Common


type FileAggregator e = AbsFile                                            -- source file
                    -> (forall m. MonadThrow m => AbsFile -> m AbsFile)   -- destFileFunc
                    -> IO (Either LogTransformError AbsFile)              -- dest file path or error 
runAggregator :: IO (Either IOError AbsFile) -> FileAggregator Text ->  IO ()
runAggregator file fa = do 
                  eFile <- file 
                  eitherf eFile
                    (chkFail . txt)
                    (\inputFile ->
                      do
                        pth <- fa inputFile (replaceExtension ".log") -- later version of Path
                        putStrLn $ txt pth
                        chk $ isRight pth   
                    )

-- unit_demo_prettyPrint_integration :: IO ()
-- unit_demo_prettyPrint_integration = runAggregator jsoniFile testPrettyPrintFile

-- unit_demo_itemAggregator_integration :: IO ()
-- unit_demo_itemAggregator_integration = runAggregator jsoniFile testIterationStepFile

-- unit_demo_itemAggregator_integration_i140 :: IO ()
-- unit_demo_itemAggregator_integration_i140 = runAggregator jsoniFileIteration140 testIterationStepFile

jsoniFile :: IO AbsDir -> IO (Either IOError AbsFile)
jsoniFile projRoot = dataFile projRoot [relfile|demo_raw_log.ijson|]

jsoniFileIteration140 :: IO AbsDir -> IO (Either IOError AbsFile)
jsoniFileIteration140 projRoot = dataFile projRoot [relfile|demo_raw_log_140.ijson|]