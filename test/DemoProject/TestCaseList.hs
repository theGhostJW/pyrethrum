
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
import DemoProject.Test.RoughIntState
import DemoProject.Test.RoughDisabled as DT
import  DemoProject.Test.Simple2 as ST2
import Control.Monad
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

plan :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
plan = validPlan doNothing doNothing doNothing doNothing

runPlan :: forall effs. (EFFAllEffects effs) =>
            (forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => (ItemRunParams as ds i TestConfig RunConfig effs -> Sem effs ()))  -- item runner                                                -- runConfig
            -> Sem effs ()
runPlan = testRun plan filters runConfig

-----------------------------
--------- Run Types ---------
-----------------------------

planRun :: Sem FullIOEffects ()
planRun = runPlan normalExecution
     
planListing :: Sem FullDocEffects ()
planListing = runPlan docExecution

------------------------------
---------- Listings ----------
------------------------------

rawListing :: (DList Text, Either AppError ())
rawListing = executeDocumentRaw planListing

prettyListing :: (DList Text, Either AppError ())
prettyListing = executeDocumentPretty planListing

------------------------------
------------ Runs ------------
------------------------------

-- runInIO :: IO ()
-- runInIO = executeInIOConsolePretty planRun

runLogToFile :: IO ()
runLogToFile = do 
                ePths <- ioRunToFile NoConsole False executeWithLogger planRun 
                eitherf ePths 
                  (\err -> putStrLn $ "Error Encountered\n" <> txt err)
                  (\pths ->
                      let 
                        jsnItmsPth = find ((== jsonItemLogExt) . toS . fileExtension) pths
                      in
                        maybef jsnItmsPth
                          (putStrLn $ "Unable to generate report no: " <> jsonItemLogExt <> " file found in log files\n")
                          prepareFinalLogs
                  )

-- runConsoleAndFile :: IO ()
-- runConsoleAndFile = void $ ioRunToFile Console False plan executeWithLogger normalExecution 

-- docConsoleAndFile :: IO ()
-- docConsoleAndFile = void $ ioRunToFile Console True plan documentWithLogger docExecution

-- runInIORaw :: IO ()
-- runInIORaw = ioRunRaw plan

-- runNZInIO :: IO ()
-- runNZInIO = testRun plan filters normalExecution executeInIOConsoleRaw runConfig {country = NZ}

-- runDocument :: DList Text
-- runDocument = docRun plan

-- runDocumentToConsole :: IO ()
-- runDocumentToConsole = sequence_ . P.toList $ putStrLn <$> runDocument

alwaysFailCheck :: PreRun effs
alwaysFailCheck = PreRun {
  runAction = pure (),
  checkHasRun = pure False
}

testRunFailHomeG2 :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunFailHomeG2 = validPlan doNothing -- rollOver0
                              doNothing -- goHome0
                              doNothing -- rollOver1
                              alwaysFailCheck -- goHome1

-- runFailHomeG2IO :: IO ()
-- runFailHomeG2IO = ioRun testRunFailHomeG2

-- runFailHomeG2Document :: DList Text
-- runFailHomeG2Document = docRun testRunFailHomeG2

-- testRunFailRolloverG1 :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
-- testRunFailRolloverG1 = validPlan alwaysFailCheck doNothing doNothing doNothing

-- runFailRolloverG1Document :: DList Text
-- runFailRolloverG1Document = docRun testRunFailRolloverG1

-- runFailRolloverG1IO :: IO ()
-- runFailRolloverG1IO = ioRun testRunFailRolloverG1

dummyIOException :: Sem effs Bool
dummyIOException = (E.throw $ P.userError "Pretend IO Error") :: Sem effs Bool

exceptionInCheck :: PreRun effs
exceptionInCheck = PreRun {
  runAction = pure (),
  checkHasRun = dummyIOException
}

testRunFailExceptG2GoHomeCheck :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunFailExceptG2GoHomeCheck = validPlan doNothing doNothing doNothing exceptionInCheck

-- runExceptG2GoHomeCheckIO :: IO ()
-- runExceptG2GoHomeCheckIO = ioRun testRunFailExceptG2GoHomeCheck

exceptionInRollover :: PreRun effs
exceptionInRollover = PreRun {
  runAction = void dummyIOException,
  checkHasRun = pure True
}

testRunExceptG1Rollover:: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunExceptG1Rollover = validPlan exceptionInRollover doNothing doNothing doNothing

-- runExceptG1Rollover :: IO ()
-- runExceptG1Rollover = ioRun testRunExceptG1Rollover

justLogPreRun :: EFFLogger effs => PreTestStage -> PreRun effs
justLogPreRun stage = PreRun {
  runAction = log $ "Run Action: " <> txt stage,
  checkHasRun = log ("Check Action Run: " <> txt stage) $> True
}

testG1GoHomeLogging:: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testG1GoHomeLogging = validPlan (justLogPreRun Rollover) (justLogPreRun GoHome) doNothing doNothing

justLogPreRunFailCheck :: EFFLogger effs => PreTestStage -> PreRun effs
justLogPreRunFailCheck stage = PreRun {
  runAction = log $ "Run Action: " <> txt stage,
  checkHasRun = log ("Check Action Run: " <> txt stage) $> False
}

testG1GoHomeLoggingFailCheck :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testG1GoHomeLoggingFailCheck = validPlan (justLogPreRun Rollover) (justLogPreRunFailCheck GoHome) doNothing doNothing
