
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

runInIO = testRun [] consoleLogger runConfig testInfoFull executeInIO
runDocument  = testRun [] consoleLogger runConfig testInfoFull executeDocument

filterTests :: forall effs. EFFFileSystem effs =>
      (forall i as vs. TestFilters RunConfig TestConfig -> RunConfig -> GenericTest TestConfig RunConfig i effs as vs -> Identity (TestFilterResult TestConfig))
      -> TestFilters RunConfig TestConfig
      -> RunConfig
      -> [Identity (TestFilterResult TestConfig)]
filterTests fltrFunc fltrs rc = runRunner (fltrFunc fltrs rc)

testRun :: forall effs m. (EFFFileSystem effs, Monad m) =>
                  TestFilters RunConfig TestConfig
                  -> (forall s. Show s => s -> m ())                                           -- logger
                  -> RunConfig                                                              -- runConfig
                  -> (forall i as vs. ItemClass i vs => i -> as -> vs -> TestInfo i as vs)  -- aggregator (result constructor)
                  -> (forall a. Eff effs a -> m (Either AppError a))                        -- interpreter
                  -> m ()
testRun fltrs l r agg itpr =
                      let
                        testFilter :: forall i as vs. GenericTest TestConfig RunConfig i effs as vs -> Identity (TestFilterResult TestConfig)
                        testFilter = filterTest fltrs r

                        fltrLog ::  [TestFilterResult TestConfig]
                        fltrLog = runIdentity <$> runRunner testFilter
                      in
                        l fltrLog >> foldl' (>>) (pure ()) (P.concat $ runRunner $ R.runLogAll agg l r itpr)

runRunner :: forall m m1 effs a. EFFFileSystem effs => (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i effs as vs -> m1 (m a)) -> [m1 (m a)]
runRunner f =
    [
      f RT.test,
      f ST.test
    ]
