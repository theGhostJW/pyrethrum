module DemoConfig (
    module DemoConfig
  , module RC
) where

import           Data.Set             as S
import           DemoConfigPrimatives
import           DemoRunConfig        as RC
import           Foundation.Extended
import           Runner
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

testRunner :: (ItemClass i vs) =>  (i -> as -> vs -> ag)        -- aggreagator
                               -> ((as -> ag) -> effs -> rslt)  -- interpreter
                               -> Filter i                      -- item filter
                               -> GenericTest tc RunConfig i effs as vs
                               -> GenericResult tc rslt
testRunner = runTest runConfig

testRunnerFull :: (ItemClass i vs) => ((as -> TestInfo i as vs) -> effs -> rslt)  -- interpreter
                                   -> Filter i                                    -- item filter
                                   -> GenericTest tc RunConfig i effs as vs
                                   -> GenericResult tc rslt
testRunnerFull = runTest runConfig testInfoFull

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
