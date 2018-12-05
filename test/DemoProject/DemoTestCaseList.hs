
module DemoProject.DemoTestCaseList where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import Data.Functor.Identity
import           DemoProject.DemoConfig
import           DemoProject.DemoRoughTest as RT
import           DemoProject.DemoRoughTest2 as RT2
import           DemoProject.DemoRoughTestSimple as ST
import           DemoProject.DemoRoughTestSimple2 as ST2
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.Logger
import           Foundation.Extended
import qualified Prelude                    as P
import           Runner as R

runInIO = runGrouped runSuccess [] testInfoFull runConfig executeInIO
runNZInIO = runGrouped runSuccess filters testInfoFull runConfig {country = NZ} executeInIO

runDocument = extractDocLog $ runGrouped runSuccess [] testInfoFull runConfig executeDocument

validPlan :: forall m m1 effs a. EFFFileSystem effs =>
  PreRun effs
  -> PreRun effs
  -> PreRun effs
  -> PreRun effs
  -> (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i effs as vs -> m1 (m a)) -> [TestGroup m1 m a effs]
validPlan ro0 gh0 ro1 gh1 f =
  [

   TestGroup {
          rollover = ro0,
          goHome = gh0,
          tests = [
              f RT.test,
              f ST.test
            ]
     },

    TestGroup {
          rollover = ro1,
          goHome = gh1,
          tests = [
              f RT2.test,
              f ST2.test
            ]
     }

    ]

runSuccess :: forall m m1 effs a. EFFFileSystem effs => (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i effs as vs -> m1 (m a)) -> [TestGroup m1 m a effs]
runSuccess = validPlan doNothing doNothing doNothing doNothing


alwaysFailCheck :: PreRun effs
alwaysFailCheck = PreRun {
  runAction = pure (),
  checkHasRun = pure False
}

runInIOFailCheck = runGrouped testRunFailHomeG2 [] testInfoFull runConfig executeInIO
runDocumentFailCheck = extractDocLog $ runGrouped testRunFailHomeG2 [] testInfoFull runConfig executeDocument

testRunFailHomeG2 :: forall m m1 effs a. EFFFileSystem effs => (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i effs as vs -> m1 (m a)) -> [TestGroup m1 m a effs]
testRunFailHomeG2 = validPlan doNothing doNothing doNothing alwaysFailCheck

--- Monad Play ---

fwtf :: IO Bool
fwtf = do
        putStrLn "Calculating"
        pure True

fwtfRun :: IO Bool -> IO Bool
fwtfRun f = do
             a <- f
             b <- f
             f

demoWtf = fwtfRun fwtf

fwtfRun'' :: IO Bool -> IO Bool
fwtfRun'' f = f >> f >> f

demoWtf'' = fwtfRun fwtf

fwtfRun' :: IO Bool -> IO Bool
fwtfRun' f = do
                f
                f
                f

demoWtf' = fwtfRun' fwtf
