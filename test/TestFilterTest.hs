module TestFilterTest where

import Pyrelude         as P
import Polysemy
import           Runner as R
import           Pyrelude.Test hiding (Group)
import Data.Yaml
import Data.Aeson.TH
import Data.Aeson.Types
import TestFilter
import RunnerBase ( Test )

data TestDepth = Connectivity | Regression | DeepRegression deriving (Eq, Ord, Show)
data Country = Au | NZ deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''TestDepth)

data RunConfig = RunConfig {
  country :: Country,
  level :: TestDepth
}

data TestConfig = TestConfig {
  header :: Text,
  address :: TestAddress,
  countries :: [Country],
  level :: TestDepth,
  enabled :: Bool
}  deriving (Show ,Eq)

instance TestConfigClass TestConfig where
  moduleAddress = address

instance Titled TestConfig where
  title = header

$(deriveJSON defaultOptions ''TestConfig)

type MockTest i as ds effs = RunnerBase.Test Int TestConfig RunConfig i as ds effs

newtype MyInt = MyInt Int deriving (Show, Generic)

newtype MyText = MyText Text deriving (Show, Generic, ToJSON)

instance ItemClass MyInt MyInt where
  identifier _ =  -999
  whenClause _ =  "pre"
  thenClause _ =  "post"
  checkList = mempty

instance ItemClass MyText MyText  where
  identifier _ =  -999
  whenClause _ =  "pre"
  thenClause _ =  "post"
  checkList = mempty


instance ToJSON MyInt where
  toEncoding = genericToEncoding defaultOptions

au :: [Country]
au = [Au]

nz :: [Country]
nz = [NZ]

empti :: a -> [b]
empti = const ([] :: [b])

emptiInteractor :: b -> RunConfig -> a -> Sem effs b
emptiInteractor b _ _ = pure b

emptiPrepState:: a -> i -> as -> Sem effs a
emptiPrepState a _ _ = pure a

test1 :: MockTest MyInt Text MyInt effs
test1 = Test {
              config = TestConfig {
                header = "test1",
                address = TestAddress "test1",
                countries = au <> nz,
                level = Regression,
                enabled = True
              },
              items = empti,
              interactor = emptiInteractor "Hello",
              prepState = emptiPrepState (MyInt 1)
            }

test2 :: MockTest MyInt MyInt MyInt effs
test2 = Test {
              config = TestConfig {
                header = "test2",
                address = TestAddress "test2",
                countries = nz,
                level = Regression,
                enabled = True
              },
              items = empti,
              interactor = emptiInteractor (MyInt 1),
              prepState = \i as -> pure as
            }

test3 :: MockTest MyInt MyInt MyInt effs
test3 = Test {
                config = TestConfig {
                  header = "test3",
                  address = TestAddress "test3",
                  countries = au,
                  level = Connectivity,
                  enabled = True
                },
              items = empti,
              interactor = emptiInteractor (MyInt 3),
              prepState = \i as -> pure as
            }

test4 :: MockTest Text Text Text effs 
test4 = Test {
              config = TestConfig {
                  header = "test4",
                  address = TestAddress "test4",
                  countries = au,
                  level = DeepRegression,
                  enabled = True
                },
              items = empti,
              interactor = emptiInteractor "Hello",
              prepState = \i as -> pure as
            }

test5 :: MockTest MyInt MyInt MyInt effs
test5 = Test {
              config = TestConfig {
                  header = "test5",
                  address = TestAddress "test5",
                  countries = au,
                  level = DeepRegression,
                  enabled = False
                },
              items = empti,
              interactor = emptiInteractor (MyInt 1),
              prepState = \i as -> pure as 
            }


mockSuite :: forall effs a. (forall i as ds. MockTest i as ds effs -> a) -> SuiteItem effs [a]
mockSuite r = 
  R.Group "Filter SUite" [
    Hook BeforeAll (pure ()) [
      Tests [
        r test1,
        r test2,
        r test3
      ]
    ]
  ]