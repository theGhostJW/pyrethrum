module DemoProject.Config where

import           Data.Set                   as S
import           DSL.Interpreter
import           DSL.Common
import           Foundation.Extended
import qualified Prelude                    as P
import           Runner
import           TestAndRunConfig
import           Foundation.List.DList

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
     title = "minDepth must be at least depth of test run (e.g. regression test will not be executed in connectivity run)",
     predicate = \rc tc -> minDepth tc <= depth rc
   }

isActiveFilter :: TestFilter RunConfig TestConfig
isActiveFilter = TestFilter {
    title = "test must be is active",
    predicate = \_ tc -> active tc
  }

filters :: [TestFilter RunConfig TestConfig]
filters = [isActiveFilter, countryFilter, levelFilter]

type TestPlan m1 m a effs = TestPlanBase TestConfig RunConfig m1 m a effs

testEndpoint ::
     String
     -> RunConfig
     -> Either FilterError (Set Int)
     -> (forall m1 m a. TestPlan m1 m a FullIOEffects)
     -> IO ()
testEndpoint = testEndpointBase filters testInfoFull executeInIOConsolePretty

testEndpointDoc ::
     String
     -> RunConfig
     -> Either FilterError (Set Int)
     -> (forall a m m1. TestPlan m1 m a FullDocEffects)
     -> DList String
testEndpointDoc tstAdd rc iids pln = extractDocLog $ testEndpointBase filters testInfoFull executeDocumentRaw tstAdd rc iids pln
