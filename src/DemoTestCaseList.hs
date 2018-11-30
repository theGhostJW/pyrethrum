
module DemoTestCaseList where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import Data.Functor.Identity
import           DemoConfig
import           DemoRoughTest as RT
import           DemoRoughTestSimple as ST
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.Logger
import           Foundation.Extended
import qualified Prelude                    as P
import           Runner as R

runInIO = testRun [] runConfig testInfoFull executeInIO
runNZInIO = testRun filters runConfig {country = NZ} testInfoFull executeInIO
runDocument  = testRun [] runConfig testInfoFull executeDocument

runInIOg = testRunGrouped [] testInfoFull runConfig executeInIO
runNZInIOg = testRunGrouped filters testInfoFull runConfig {country = NZ} executeInIO
runDocumentg  = testRunGrouped [] testInfoFull runConfig executeDocument

testRun :: forall effs m. (EFFFileSystem effs, Monad m) =>
                  TestFilters RunConfig TestConfig                                          -- test filters
                  -> RunConfig                                                              -- runConfig
                  -> (forall i as vs. ItemClass i vs => i -> as -> vs -> TestInfo i as vs)  -- aggregator (result constructor)
                  -> (forall a. Eff effs a -> m (Either AppError a))                        -- interpreter
                  -> m ()
testRun = genericTestRun runRunner

testRunGrouped :: forall effs m. (EFFFileSystem effs, Monad m) =>
                                                TestFilters RunConfig TestConfig                               -- filters
                                                -> (forall i as vs. (ItemClass i vs, Show i, Show vs, Show as) => i -> as -> vs -> TestInfo i as vs)             -- test aggregator i.e. rslt constructor
                                                -> RunConfig                                            -- runConfig
                                                -> (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                                                -> m ()
testRunGrouped = runGrouped runGroups

runRunner :: forall m m1 effs a. EFFFileSystem effs => (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i effs as vs -> m1 (m a)) -> [m1 (m a)]
runRunner f =
    [
      f RT.test,
      f ST.test
    ]

runGroups :: forall m m1 effs a. EFFFileSystem effs => (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i effs as vs -> m1 (m a)) -> [TestGroup m1 m a effs]
runGroups f =
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
