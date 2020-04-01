module DemoProject.Config where

import           Data.Set                   as S
import           DSL.Interpreter
import           Common
import           Pyrelude
import qualified Prelude                    as P
import           Runner 
import           TestFilter
import           Data.DList
import Data.Aeson
import Data.Aeson.TH
import Polysemy
import Polysemy.Error as PE

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum)
data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)

data RunConfig = RunConfig {
  runTitle    :: Text,
  environment :: Environment,
  country     :: Country,
  depth       :: Depth
} deriving (Eq, Show)

instance Titled RunConfig where
  title = runTitle

instance RunConfigClass RunConfig

allEnvironments :: Set Environment
allEnvironments = S.fromList enumList

allNonProdEnvironments :: Set Environment
allNonProdEnvironments = S.fromList [TST, UAT, PreProd]

allCountries = S.fromList [AU, NZ]
auOnly = S.singleton AU
nzOnly = S.singleton NZ

data TestConfig = TestConfig {
  header       :: Text,
  address      :: TestModule,
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
  address = TestModule "Configuration Error ~ No Address Assigned",
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

filterList :: [TestFilter RunConfig TestConfig]
filterList = [isActiveFilter, countryFilter, levelFilter]

applyTestFiltersToItems :: RunConfig -> (i -> TestConfig) -> [i] -> [i]
applyTestFiltersToItems = applyTestFilters filterList

type TestPlan m1 m a effs = TestPlanBase TestConfig RunConfig m1 m a effs


testEndpointPriv :: forall effs1. ApEffs effs1 =>
      (forall rc tc i as ds effs. (ItemClass i ds, ToJSON as, ToJSON ds, TestConfigClass tc, ApEffs effs) 
                  => ItemParams as ds i tc rc effs -> Sem effs ())  
     -> TestModule
     -> RunConfig
     -> Either FilterError (Set Int)
     -> (forall m1 m a. TestPlan m1 m a effs1)
     -> Sem effs1 ()
testEndpointPriv itmRunner testMod rc itrSet plan = 
  let 
    runParams :: RunParams RunConfig TestConfig effs1 
    runParams = RunParams {
      plan = plan,
      filters = filterList,
      itemRunner = itmRunner,
      rc = rc
    }
  in
    testEndpointBase runParams testMod itrSet

testEndpoint ::
     TestModule
     -> RunConfig
     -> Either FilterError (Set Int)
     -> (forall m1 m a. TestPlan m1 m a FullIOEffects)
     -> Sem FullIOEffects ()
testEndpoint = testEndpointPriv normalExecution

testEndpointDoc ::
     TestModule
     -> RunConfig
     -> Either FilterError (Set Int)
     -> (forall a m m1. TestPlan m1 m a FullDocEffects)
     -> DList Text
testEndpointDoc testMod rc itrSet plan = fst . documentRaw $ testEndpointPriv docExecution testMod rc itrSet plan


$(deriveJSON defaultOptions ''TestConfig)
$(deriveJSON defaultOptions ''Environment)
$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''Depth)
$(deriveJSON defaultOptions ''RunConfig)