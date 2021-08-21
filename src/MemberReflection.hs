{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module MemberReflection where

import Pyrelude hiding (Item)
import Polysemy
import Polysemy.Internal.CustomErrors
import           DSL.ArbitraryIO
import           DSL.Logger
import           Runner  as R hiding (interactor, parse)
import           Data.Aeson.TH
import           Data.Set as S
import Type.Reflection as R
import Check
import DSL.Interpreter (Failure)



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Example Config %%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum)
data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)

data RunConfig = RunConfig {
  title    :: Text,
  environment :: Environment,
  country     :: Country,
  depth       :: Depth
} deriving (Eq, Show)


instance Config RunConfig

allEnvironments :: Set Environment
allEnvironments = S.fromList enumList

allNonProdEnvironments :: Set Environment
allNonProdEnvironments = S.fromList [TST, UAT, PreProd]

allCountries = S.fromList [AU, NZ]
auOnly = S.singleton AU
nzOnly = S.singleton NZ

data TestConfig = TestConfig {
  title       :: Text,
  environments :: Set Environment,
  countries    :: Set Country,
  minDepth     :: Depth,
  active       :: Bool
}  deriving (Eq, Show)

data SuiteError = MyError1 | 
                  MyError2 
                  deriving (Show, Typeable)

{-
data Test e tc rc hi i as ds effs = Test {
  config :: tc,
  items :: rc -> [i],
  interactor :: rc -> hi -> i -> Sem effs as,
  parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
}
-}

type Test hi = R.Test SuiteError TestConfig RunConfig hi
type TestResult = GenericResult TestConfig


instance Config TestConfig 

testConfig :: TestConfig
testConfig = TestConfig {
  title    = "Configuration Error ~ No Title Assigned",
  environments = allNonProdEnvironments,
  countries    = auOnly,
  minDepth     = DeepRegression,
  active       = True
}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Example Test Case %%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


type Effects e = '[Logger e , ArbitraryIO]

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  exePath :: Text,
  fileText :: Text
} deriving Show

newtype DState = V {
                    iidx10 :: Int
                  } deriving Show

interactor :: forall e effs. Members (Effects e) effs => RunConfig -> () -> Item -> Sem effs ApState
interactor RunConfig{} () Item{} = uu

parse :: forall e effs. Member (Failure e) effs => ApState -> Sem effs DState
parse = uu

data Item = Item {
  iid    :: Int,
  pre    :: Text,
  post   :: Text,
  path   :: Path Abs File,
  checks :: Checks DState
} deriving (Show, Generic)

items :: RunConfig -> [Item]
items _ = []
  


test :: forall effs. Members (Effects SuiteError) effs => MemberReflection.Test () Item ApState DState effs
test = Test MemberReflection.testConfig 
            MemberReflection.items
            MemberReflection.interactor
            MemberReflection.parse

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
demo = showEffs (WrappedTest test :: MembersFuncWrapper (Effects SuiteError) effs (MemberReflection.Test () Item ApState DState effs))

newtype InteractorFuncWrapper memberEffs allEffs a = WrappedInteractor (Members memberEffs allEffs => RunConfig -> () -> Item -> Sem allEffs ApState)

showInteractorEffs :: forall es0 es1 a. ShowTypes es0 => InteractorFuncWrapper es0 es1 a -> [Text]
showInteractorEffs _ = showTypes @es0

demo2 :: [Text]
demo2 = removeKindSuffix <$> showInteractorEffs (WrappedInteractor MemberReflection.interactor :: InteractorFuncWrapper (Effects SuiteError) effs (RunConfig -> () -> Item -> Sem effs ApState))


$(deriveJSON defaultOptions ''TestConfig)
$(deriveJSON defaultOptions ''Environment)
$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''Depth)
$(deriveJSON defaultOptions ''RunConfig)