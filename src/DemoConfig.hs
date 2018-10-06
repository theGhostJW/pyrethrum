module DemoConfig (
    module DemoConfig
  , module RC
) where

import DSL.Interpreter
import           Data.Set             as S
import           DemoConfigPrimatives
import           DemoRunConfig        as RC
import           Foundation.Extended
import           Runner
import           Control.Monad.Freer
import           TestAndRunConfig

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

testRunner :: (ItemClass i vs, EFFLogger effs) =>  (i -> as -> vs -> ag)        -- aggreagator
                               -> ((as -> ag) -> Eff effs as -> rslt)  -- interpreter
                               -> Filter i                      -- item filter
                               -> GenericTest tc RunConfig i (Eff effs as) as vs
                               -> GenericResult tc rslt
testRunner = runTest runConfig

testRunnerFull :: (ItemClass i vs, EFFLogger effs, Show i, Show as, Show vs) => ((as -> String) -> Eff effs as -> rslt)  -- interpreter
                                   -> Filter i                                                    -- item filter
                                   -> GenericTest tc RunConfig i (Eff effs as) as vs
                                   -> GenericResult tc rslt
testRunnerFull = testRunner (testInfoFullShow show)

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
