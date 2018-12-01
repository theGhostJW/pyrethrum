
module DemoProject.DemoTestCaseList where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import Data.Functor.Identity
import           DemoProject.DemoConfig
import           DemoProject.DemoRoughTest as RT
import           DemoProject.DemoRoughTestSimple as ST
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.Logger
import           Foundation.Extended
import qualified Prelude                    as P
import           Runner as R

runInIO = executeTestRun [] testInfoFull runConfig executeInIO
runNZInIO = executeTestRun filters testInfoFull runConfig {country = NZ} executeInIO
runDocument  = executeTestRun [] testInfoFull runConfig executeDocument

executeTestRun :: forall effs m. (EFFFileSystem effs, Monad m) =>
                  TestFilters RunConfig TestConfig                                                                      -- filters
                  -> (forall i as vs. (ItemClass i vs, Show i, Show vs, Show as) => i -> as -> vs -> TestInfo i as vs)  -- test aggregator i.e. rslt constructor
                  -> RunConfig                                                                                          -- runConfig
                  -> (forall a. Eff effs a -> m (Either AppError a))                                                    -- interpreter
                  -> m ()
executeTestRun = runGrouped testRun

testRun :: forall m m1 effs a. EFFFileSystem effs => (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i effs as vs -> m1 (m a)) -> [TestGroup m1 m a effs]
testRun f =
  [

   TestGroup {
          rollover = doNothing,
          goHome = doNothing,
          tests = [
              f RT.test,
              f ST.test
            ]
     },

    TestGroup {
          rollover = doNothing,
          goHome = doNothing,
          tests = [
              f RT.test,
              f ST.test
            ]
     }

    ]




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
