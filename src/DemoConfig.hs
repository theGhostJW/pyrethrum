module DemoConfig (
 module DemoConfigPrimatives,
 Environment(..),
 Country(..),
 Depth(..),
 TestConfig(..),
 RC.RunConfig(..),
 allEnvironments,
 allNonProdEnvironments,
 allCountries,
 auOnly,
 nzOnly,
 sampleRunConfig
) where

import           Data.Set             as S
import           DemoConfigPrimatives
import qualified DemoRunConfig        as RC
import           Foundation.Extended
import           TestAndRunConfig


allEnvironments :: Set Environment
allEnvironments = S.fromList [Test, UAT, PreProd,  Prod]
allNonProdEnvironments = S.fromList [Test, UAT, PreProd,  Prod]

allCountries = S.fromList [AU, NZ]
auOnly = S.singleton AU
nzOnly = S.singleton NZ

data TestConfig = TestConfig {
  title        :: String,
  address      :: String,
  environments :: Set Environment,
  countries    :: Set Country,
  minDepth     :: Depth,
  active       :: Bool
}  deriving Show

instance Titled TestConfig where
  title = DemoConfig.title

defaultTestConfig = TestConfig {
  title    = "No Title Assigned",
  address = "No address assigned use moduleOf TestItem",
  environments = allNonProdEnvironments,
  countries    = auOnly,
  minDepth     = DeepRegression,
  active       = True
}

sampleRunConfig = RC.defaultRunConfig
