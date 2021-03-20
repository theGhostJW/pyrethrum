module SemTest where

import Pyrelude         as P
import Polysemy
import           Runner as R
import           Pyrelude.Test hiding (Group)
import Data.Yaml
import Data.Aeson.TH
import Data.Aeson.Types
import TestFilter
import RunnerBase ( Test )

data Include = In | Out deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''Include)

newtype RunConfig = RunConfig Include


data TestConfig = TestConfig {
  header :: Text,
  address :: TestAddress,
  include :: Include
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


empti :: a -> [b]
empti = const ([] :: [b])

emptiInteractor :: b -> RunConfig -> a -> Sem effs b
emptiInteractor b _ _ = pure b

emptiParser:: a -> i -> as -> Sem effs a
emptiParser a _ _ = pure a

test1 :: MockTest MyInt Text MyInt effs
test1 = Test {
              config = TestConfig {
                header = "test1",
                address = TestAddress "test1",
                include = In
              },
              items = empti,
              interactor = emptiInteractor "Hello",
              parse = emptiParser (MyInt 1)
            }

test2 :: MockTest MyInt MyInt MyInt effs
test2 = Test {
              config = TestConfig {
                header = "test2",
                address = TestAddress "test2 address",
                include = In
              },
              items = empti,
              interactor = emptiInteractor (MyInt 1),
              parse = \i as -> pure as
            }

test3 :: MockTest MyInt MyInt MyInt effs
test3 = Test {
                config = TestConfig {
                header = "test3",
                address = TestAddress "test3 address",
                include = Out
            },
              items = empti,
              interactor = emptiInteractor (MyInt 3),
              parse = \i as -> pure as
            }

test4 :: MockTest Text Text Text effs 
test4 = Test {
              config = TestConfig {
                  header = "test4",
                  address = TestAddress "test4 address",
                  include = In
              },
              items = empti,
              interactor = emptiInteractor "Hello",
              parse = \i as -> pure as
            }

test5 :: MockTest MyInt MyInt MyInt effs
test5 = Test {
              config = TestConfig {
                  header = "test5",
                  address = TestAddress "test5 address",
                  include = Out
                },
              items = empti,
              interactor = emptiInteractor (MyInt 1),
              parse = \i as -> pure as 
            }


includeFilter :: TestFilter RunConfig TestConfig
includeFilter = TestFilter {
     title = "test include must match run",
     predicate = \(RunConfig inc) tc -> include tc == inc
   }

filters' :: [TestFilter RunConfig TestConfig]
filters' = [includeFilter]


mockSuite :: forall effs a. (forall i as ds. (Show i, Show as, Show ds) => MockTest i as ds effs -> a) -> SuiteItem effs [a]
mockSuite r = 
  R.Group "Filter Suite" [
    Hook BeforeAll (pure ()) [
      Tests [
        r test1,
        r test2,
        r test3
      ]
    ],

    R.Group "Sub Group" [
      Tests [
        r test4,
        r test5
      ]
    ],

    R.Group "Empty Group" [
      Tests []
    ]
      
  ]


-- unit_test_filter_expect_empty