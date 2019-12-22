{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module MemberReflection where

import Pyrelude
import Polysemy
import Polysemy.Internal.CustomErrors
import           DSL.Ensure
import           DSL.ArbitraryIO
import           DSL.Logger
import           Runner
import           Data.Aeson.TH
import           Data.Set as S
import Type.Reflection as R
import Check

type EnsureType = DefiningModule Ensure


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Example Config %%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Example Test Case %%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


type Effects = '[Logger, Ensure, ArbitraryIO]

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  exePath :: Text,
  fileText :: Text
} deriving Show

newtype DState = V {
                    iidx10 :: Int
                  } deriving Show

interactor :: forall effs. Members Effects effs => RunConfig -> Item -> Sem effs ApState
interactor RunConfig{..} Item{..} = uu

prepState :: Ensurable effs => Item -> ApState -> Sem effs DState
prepState _i ApState{..} = uu

data Item = Item {
  iid    :: Int,
  pre    :: Text,
  post   :: Text,
  path   :: Path Abs File,
  checks :: CheckDList DState
} deriving (Show, Generic)

items :: RunConfig -> [Item]
items rc = []
  
nameOfModule :: TestModule
nameOfModule = mkTestModule ''ApState

test :: forall effs. Members Effects effs => Test Item ApState DState effs
test = GenericTest {
              configuration = MemberReflection.testConfig {address = nameOfModule},
              components = TestComponents {
                                testItems = items,
                                testInteractor = interactor,
                                testPrepState = prepState
                            }
            }

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Reflection %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- https://stackoverflow.com/a/53272316/5589037

-- Logger (* -> *) -> Logger
removeKindSuffix :: Text -> Text
removeKindSuffix = fst . breakOn " "

newtype MembersFuncWrapper memberEffs allEffs a = WrappedTest (Members memberEffs allEffs => a)

class ShowTypes es where
  showTypes :: [Text]

instance ShowTypes '[] where
  showTypes = []

instance (Typeable e, ShowTypes es) => ShowTypes (e ': es) where
   showTypes = txt (R.typeRep @e) : showTypes @es

showEffs :: forall es0 es1 a. ShowTypes es0 => MembersFuncWrapper es0 es1 a -> [Text]
showEffs _ = removeKindSuffix <$> showTypes @es0

demo :: [Text]
demo = showEffs (WrappedTest test :: MembersFuncWrapper Effects effs (Test Item ApState DState effs))

newtype InteractorFuncWrapper memberEffs allEffs a = WrappedInteractor (Members memberEffs allEffs => RunConfig -> Item -> Sem allEffs ApState)

showInteractorEffs :: forall es0 es1 a. ShowTypes es0 => InteractorFuncWrapper es0 es1 a -> [Text]
showInteractorEffs _ = showTypes @es0

demo2 :: [Text]
demo2 = removeKindSuffix <$> showInteractorEffs (WrappedInteractor interactor :: InteractorFuncWrapper Effects effs (RunConfig -> Item -> Sem effs ApState))


$(deriveJSON defaultOptions ''TestConfig)
$(deriveJSON defaultOptions ''Environment)
$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''Depth)
$(deriveJSON defaultOptions ''RunConfig)