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
    predicate = \rc tc -> active tc
  }

filters :: [TestFilter RunConfig TestConfig]
filters = [isActiveFilter, countryFilter, levelFilter]

testEndpoint ::
     String
     -> RunConfig
     -> Either FilterError (Set Int)
     -> (forall a mo mi.
         (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i FullIOEffects as vs -> mo (mi a)) -> [TestGroup mo mi a FullIOEffects]
        )
     -> IO ()
testEndpoint = testEndpointBase filters testInfoFull executeInIO

testEndpointDoc ::
     String
     -> RunConfig
     -> Either FilterError (Set Int)
     -> (forall a mo mi.
         (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i FullDocEffects as vs -> mo (mi a)) -> [TestGroup mo mi a FullDocEffects]
        )
     -> DList String
testEndpointDoc tstAdd rc iids pln = extractDocLog $ testEndpointBase filters testInfoFull executeDocument tstAdd rc iids pln
