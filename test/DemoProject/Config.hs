module DemoProject.Config where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           Data.Set                   as S
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.Logger
import           Foundation.Extended
import qualified Prelude                    as P
import           Runner
import           TestAndRunConfig

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord)
data Country = AU | NZ deriving (Show, Eq, Ord)
data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord)


data RunConfig = RunConfig {
  runTitle    :: String,
  environment :: Environment,
  country     :: Country,
  depth       :: Depth
} deriving (Eq, Show)

instance Titled RunConfig where
  title = runTitle

allEnvironments :: Set Environment
allEnvironments = S.fromList [TST, UAT, PreProd, Prod]

allNonProdEnvironments :: Set Environment
allNonProdEnvironments = S.fromList [TST, UAT, PreProd]

allCountries = S.fromList [AU, NZ]
auOnly = S.singleton AU
nzOnly = S.singleton NZ

data TestConfig = TestConfig {
  header       :: String,
  address      :: String,
  environments :: Set Environment,
  countries    :: Set Country,
  minDepth     :: Depth,
  active       :: Bool
}  deriving (Eq, Show)

type Test = GenericTest TestConfig RunConfig
type TestResult = GenericResult TestConfig

instance Titled TestConfig where
  title = header

instance TestConfigClass TestConfig where
  moduleAddress = address

testConfig :: TestConfig
testConfig = TestConfig {
  header    = "Configuration Error ~ No Title Assigned",
  address = "Configuration Error ~ No Address Assigned",
  environments = allNonProdEnvironments,
  countries    = auOnly,
  minDepth     = DeepRegression,
  active       = True
}

runConfig :: RunConfig
runConfig = RunConfig {
  runTitle = "Sample RunConfig",
  environment = TST,
  country = AU,
  depth = DeepRegression
}

countryFilter :: TestFilter RunConfig TestConfig
countryFilter = TestFilter {
     title = "country must match test run",
     predicate = \rc tc -> P.elem (country rc) $ countries tc
   }

levelFilter :: TestFilter RunConfig TestConfig
levelFilter = TestFilter {
     title = "minDepth must be at least depth of test run (e.g. regression test will not be run in connectivity run)",
     predicate = \rc tc -> minDepth tc <= depth rc
   }

activeFilter :: TestFilter RunConfig TestConfig
activeFilter = TestFilter {
    title = "test must be is active",
    predicate = \rc tc -> active tc
  }

filters :: [TestFilter RunConfig TestConfig]
filters = [activeFilter, countryFilter, levelFilter]
