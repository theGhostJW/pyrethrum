{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

module Config where
  
  {- 
  A temp module to get types worked out.
  This module will ultimately end up in the end user test project
  -}

import           Data.Set                   as S
import           DSL.Interpreter
import           Common
import           Pyrelude
import qualified Prelude                    as P
import Runner as R hiding (Test)
import qualified Runner as R
import FileLogging as L
import TestFilters as F
import           TestFilter
import           Data.DList
import Data.Aeson
import Data.Aeson.TH
import Polysemy
import Polysemy.Error as PE
import DSL.Logger
import DSL.LogProtocol
import ItemRunners
import Data.Either.Extra (eitherToMaybe)
-- import Paths_pyrethrumDemo

exeDir :: IO AbsDir
exeDir = debug . parent <$> (parseAbsFile =<< uu)

showAndLogItems :: Show a => [a] -> IO ()
showAndLogItems = L.showAndLogItems exeDir


type SuiteLogger = Logger SuiteError

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum)
data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)

data RunConfig = RunConfig {
  runTitle    :: Text,
  environment :: Environment,
  country     :: Country,
  depth       :: Depth
} deriving (Eq, Show)

data SuiteError = MyError Text | 
                  MyOtherError 
                  deriving (Show, Typeable)

type AppError = FrameworkError SuiteError
type EFFAllEffects effs = EFFAllEffectsBase SuiteError effs
type FullIOEffects = FullIOMembersBase SuiteError
type FullIOMembers = FullIOMembersBase SuiteError
type LogProtocol = LogProtocolBase SuiteError

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
  address      :: TestAddress,
  environments :: Set Environment,
  countries    :: Set Country,
  minDepth     :: Depth,
  active       :: Bool
}  deriving (Eq, Show)

type Test = R.Test SuiteError TestConfig RunConfig
type TestResult = GenericResult TestConfig

instance Titled TestConfig where
  title = header

instance TestConfigClass TestConfig where
  moduleAddress = address

testConfig :: TestConfig
testConfig = TestConfig {
  header    = "Configuration Error ~ No Title Assigned",
  address = TestAddress "Configuration Error ~ No Address Assigned",
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
applyTestFiltersToItems = F.applyTestFilters filterList

type TestPlan effs a = R.Suite SuiteError TestConfig RunConfig effs a

testEndpointPriv :: forall effs. MinEffs SuiteError effs =>
      (forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => 
        ItemRunner SuiteError as ds i TestConfig RunConfig effs)  
     -> TestAddress
     -> RunConfig
     -> Either FilterErrorType (Set Int)
     -> (forall a. TestPlan effs a)
     -> Sem effs ()
testEndpointPriv itmRunner testAddress rc itemIds suite = 
  let 
    runParams = RunParams {
      suite = suite,
      filters = filterList,
      itemRunner = itmRunner,
      itemIds = itemIds,
      rc = rc
    }
  in
    mkEndpointSem runParams testAddress itemIds

testEndpoint ::
     TestAddress
     -> RunConfig
     -> Either FilterErrorType (Set Int)
     -> (forall a. TestPlan FullIOMembers a)
     -> Sem FullIOMembers ()
testEndpoint = testEndpointPriv runItem

testEndpointDoc ::
     TestAddress
     -> RunConfig
     -> Either FilterErrorType (Set Int)
     -> (forall a. TestPlan (FullDocEffects SuiteError) a)
     -> DList Text
testEndpointDoc testMod rc itrSet suite = fst . documentRaw $ testEndpointPriv documentItem testMod rc itrSet suite

$(deriveJSON defaultOptions ''TestConfig)
$(deriveJSON defaultOptions ''SuiteError)
$(deriveJSON defaultOptions ''Environment)
$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''Depth)
$(deriveJSON defaultOptions ''RunConfig)