
{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

module DemoProject.TestCaseList where

import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           DemoProject.Config
import           DemoProject.Test.Rough as RT
import           DemoProject.Test.Rough2 as RT2
import           DemoProject.Test.Simple as ST
import  DemoProject.Test.RoughIntState
import  DemoProject.Test.RoughDisabled as DT
import  DemoProject.Test.Simple2 as ST2
import  Control.Monad
import  Data.Functor (($>))
import           Common
import           DSL.Interpreter
import           DSL.Logger
import           DSL.Ensure
import           DSL.CurrentTime
import DSL.LogProtocol
import DSL.LogProtocol.PrettyPrint
import           Data.DList
import           Pyrelude as P
import           Pyrelude.IO as PIO (putStrLn)
import           Runner as R
import qualified Control.Exception as E
import AuxFiles as A
import LogTransformation (prepareFinalLogs)
import Data.Aeson (ToJSON(..))
import Data.Map as M
import TestFilter
import RunnerConsoleAndFile

-- parametrised so can inject different preruns for testing
validPlan :: forall m m1 effs a. EFFAllEffects effs =>
  PreRun effs      -- rollOver0
  -> PreRun effs   -- goHome0
  -> PreRun effs   -- rollOver1
  -> PreRun effs   -- goHome1
  ->  TestPlan m1 m a effs
validPlan ro0 gh0 ro1 gh1 f =
  [

    TestGroup {
           header = "Group 1",
           rollover = ro0,
           goHome = gh0,
           tests = [
               f RT.test,
               f DT.test,
               f ST.test,
               f DemoProject.Test.RoughIntState.test
             ]
      },

    TestGroup {
          header = "Group 2",
          rollover = ro1,
          goHome = gh1,
          tests = [
              f RT2.test,
              f ST2.test
            ]
     }
    ]

simplePlan :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
simplePlan = validPlan doNothing doNothing doNothing doNothing

runParams :: forall effs. EFFAllEffects effs => RunParams RunConfig TestConfig effs
runParams = RunParams {
      plan = simplePlan,
      filters = filterList,
      itemRunner = normalExecution,
      rc = runConfig
    }

------------------------------------------
--------- Apply Default Configs ----------
------------------------------------------

runSem :: forall effs. EFFAllEffects effs => Sem effs ()
runSem = mkRunSem runParams
     
listingSem :: forall effs. EFFAllEffects effs => Sem effs ()
listingSem = mkRunSem runParams {itemRunner = docExecution}

-----------------------------------------
---------- Listings - to DList ----------
-----------------------------------------

rawListing :: (DList Text, Either AppError ())
rawListing = documentRaw listingSem

prettyListing :: (DList Text, Either AppError ())
prettyListing = documentPretty listingSem

-- docRun :: (forall m1 m a. TestPlan m1 m a FullDocEffects) -> DList Text
-- docRun pln = extractDocLog $ mkRunSem pln filters docExecution documentPretty runConfig

-- a testing helper
runDocument :: DList Text
runDocument = fst rawListing

------------------------------
------------ Runs ------------
------------------------------

runToLPList :: IO ([LogProtocol], Either AppError ())
runToLPList = executeForTest runSem

consoleRunResults :: Either AppError [AbsFile] -> IO ()
consoleRunResults = 
  either 
    (\err -> putStrLn $ "Error Encountered\n" <> txt err)
    (\pths ->
      let 
        -- later version of Path changes type of fileExtension
        -- jsnItmsPth = find ((== jsonItemLogExt) . toS . fromMaybe "" . fileExtension) pths
        jsnItmsPth = find ((== jsonItemLogExt) . toS . fileExtension) pths
      in
        maybef jsnItmsPth
          (putStrLn $ "Unable to generate report no: " <> jsonItemLogExt <> " file found in log files\n")
          prepareFinalLogs
    )

runLogToFile :: WantConsole -> IO ()
runLogToFile wc = 
    ioRunToFile wc False executeWithLogger runSem >>= consoleRunResults
    
runToFile :: IO ()
runToFile = runLogToFile NoConsole

runToFileAndConsole :: IO ()
runToFileAndConsole = runLogToFile Console

docLogToFile :: WantConsole -> IO ()
docLogToFile wc = 
    ioRunToFile wc True documentWithLogger listingSem >>= consoleRunResults
    
docToFile :: IO ()
docToFile = docLogToFile NoConsole

docToFileAndConsole :: IO ()
docToFileAndConsole = docLogToFile Console

runConsoleRaw :: Sem FullIOEffects () -> IO ()
runConsoleRaw sem = executeInIOConsoleRaw sem >>= 
                        \eth ->  putStrLn $ either 
                                        (\err ->  "Error - Problem Encountered\n" <> txt err)
                                        (const "run complete")
                                        eth

runInIORaw :: IO ()
runInIORaw = runConsoleRaw runSem
                
runNZInIO :: IO ()
runNZInIO = runConsoleRaw $ mkRunSem runParams {rc = runConfig {country = NZ}}

runDocumentToConsole :: IO ()
runDocumentToConsole = sequence_ . P.toList $ putStrLn <$> runDocument

alwaysFailCheck :: PreRun effs
alwaysFailCheck = PreRun {
  runAction = pure (),
  checkHasRun = pure False
}

-- broken plans for testing

runIO :: (forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs) -> IO ()
runIO plan' = ioRunToFile Console False executeWithLogger 
                    (mkRunSem RunParams {
                        plan = plan',
                        filters = filterList,
                        itemRunner = normalExecution,
                        rc = runConfig
                      }) 
                    >>= consoleRunResults

listRaw :: (forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs) -> DList Text
listRaw plan' = fst . documentRaw $ mkRunSem RunParams {
                                                            plan = plan',
                                                            filters = filterList,
                                                            itemRunner = docExecution,
                                                            rc = runConfig
                                                         }

dummyIOException :: Sem effs Bool
dummyIOException = (E.throw $ P.userError "Pretend IO Error") :: Sem effs Bool

exceptionInCheck :: PreRun effs
exceptionInCheck = PreRun {
  runAction = pure (),
  checkHasRun = dummyIOException
}

testRunFailExceptG2GoHomeCheck :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunFailExceptG2GoHomeCheck = validPlan doNothing doNothing doNothing exceptionInCheck

runExceptG2GoHomeCheckIO :: IO ()
runExceptG2GoHomeCheckIO = runIO testRunFailExceptG2GoHomeCheck

exceptionInRollover :: PreRun effs
exceptionInRollover = PreRun {
  runAction = void dummyIOException,
  checkHasRun = pure True
}

testRunExceptG1Rollover:: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunExceptG1Rollover = validPlan exceptionInRollover doNothing doNothing doNothing

runExceptG1Rollover :: IO ()
runExceptG1Rollover = runIO testRunExceptG1Rollover