module DemoConfig (
    module DemoConfig
  , module RC
) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           Data.Set                   as S
import           DemoConfigPrimatives
import           DemoRunConfig              as RC
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.Logger
import           Foundation.Extended
import           Runner
import           TestAndRunConfig

-- runAllDoc :: forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => Test i  (Eff '[FileSystem, Logger, Ensure, Error EnsureError, Writer [String], IO] as) as vs -> IO ()
-- runAllDoc = testRunnerFull executeFileSystemDocument All
--
-- runAllFull :: forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => Test i (Eff '[FileSystem, Logger, Ensure, Error FileSystemError, Error EnsureError, IO] as) as vs -> IO ()
-- runAllFull = testRunnerFull executeFileSystemInIO All

-- runAll :: forall i as vs effs rslt. (ItemClass i vs, Show rslt) => Test i (Eff effs as) as vs -> ((as -> TestInfo i as vs) -> Eff effs as -> IO rslt) -> IO ()
-- runAll = testRunnerFull All

allEnvironments :: Set Environment
allEnvironments = S.fromList [TST, UAT, PreProd, Prod]

allNonProdEnvironments :: Set Environment
allNonProdEnvironments = S.fromList [TST, UAT, PreProd]

allCountries = S.fromList [AU, NZ]
auOnly = S.singleton AU
nzOnly = S.singleton NZ

data TestConfig = TestConfig {
  header       :: String,
  environments :: Set Environment,
  countries    :: Set Country,
  minDepth     :: Depth,
  active       :: Bool
}  deriving Show

type Test = GenericTest TestConfig RC.RunConfig
type TestResult = GenericResult TestConfig


testRunner :: forall i vs rslt as tc effs. (ItemClass i vs, Show tc, Show rslt) =>
                               (i -> as -> vs -> rslt)                            -- aggreagator
                               -> Filter i                                        -- item filter
                               -> GenericTest tc RunConfig i (Eff effs as) as vs  -- TestCase
                               -> (Eff effs as -> IO as)                          -- interpreter
                               -> IO ()
testRunner = runTest runConfig consoleLogger


instance Titled TestConfig where
  title = header

testConfig :: TestConfig
testConfig = TestConfig {
  header    = "No Title Assigned",
  environments = allNonProdEnvironments,
  countries    = auOnly,
  minDepth     = DeepRegression,
  active       = True
}

runConfig :: RunConfig
runConfig = RunConfig {
  runTitle = "Sample RunConfig",
  environment = TST,
  depth = DeepRegression
}
