module DemoConfig (
    module DemoConfig
  , module RC
) where

import           Data.Set             as S
import           DemoConfigPrimatives
import           DemoRunConfig        as RC
import           Foundation.Extended
import qualified Runner               as R
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

type Test = R.GenericTest TestConfig RC.RunConfig

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

runConfig = RunConfig {
  runTitle = "Sample RunConfig",
  environment = TST,
  depth = DeepRegression
}
