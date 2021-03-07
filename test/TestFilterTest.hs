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
                address = TestAddress "test2 address",
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
                  address = TestAddress "test3 address",
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
                  address = TestAddress "test4 address",
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
                  address = TestAddress "test5 address",
                  countries = au,
                  level = DeepRegression,
                  enabled = False
                },
              items = empti,
              interactor = emptiInteractor (MyInt 1),
              prepState = \i as -> pure as 
            }


enabledFilter :: TestFilter RunConfig TestConfig
enabledFilter = TestFilter {
     title = "test must be is enabled",
     predicate = \_ tc -> enabled tc
   }

countryFilter :: TestFilter RunConfig TestConfig
countryFilter = TestFilter {
     title = "country must match test run",
     predicate = \rc tc -> P.elem (country rc) $ countries tc
   }

levelFilter :: TestFilter RunConfig TestConfig
levelFilter = TestFilter {
     title = "depth must be within run parameters (e.g. regression test will not be run in connectiviity run)",
     predicate = \rc tc -> (level :: TestConfig -> TestDepth) tc <= (level :: RunConfig -> TestDepth) rc
   }

filters' :: [TestFilter RunConfig TestConfig]
filters' = [enabledFilter, countryFilter, levelFilter]


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



filterResults :: RunConfig -> [TestFilterResult]
filterResults = filterSuite mockSuite filters'

acceptedTests :: RunConfig -> [TestFilterResult]
acceptedTests rc = P.filter acceptFilter $ filterResults rc

chkFilters :: [Text] -> RunConfig -> Assertion
chkFilters expted rc = chkEq expted $ testTitle . testInfo <$> acceptedTests rc

unit_test_filter_expect_empty :: Assertion
unit_test_filter_expect_empty = chkFilters [] $ RunConfig NZ Connectivity

unit_test_filter_country :: Assertion
unit_test_filter_country = chkFilters ["test1", "test3"] $ RunConfig Au Regression

unit_test_filter_country_nz :: Assertion
unit_test_filter_country_nz = chkFilters ["test1", "test2"] $ RunConfig NZ Regression

unit_test_filter_country_au_deep_regression :: Assertion
unit_test_filter_country_au_deep_regression = chkFilters ["test1", "test3", "test4"] $ RunConfig Au DeepRegression


filtersExcludeReasons :: RunConfig -> [Text]
filtersExcludeReasons rc = catMaybes $ reasonForRejection <$> P.filter rejectFilter (filterResults rc)

unit_test_filter_exclude_reasons :: Assertion
unit_test_filter_exclude_reasons = chkEq [
                                          "depth must be within run parameters (e.g. regression test will not be run in connectiviity run)",
                                          "depth must be within run parameters (e.g. regression test will not be run in connectiviity run)",
                                          "country must match test run",
                                          "country must match test run",
                                          "test must be is enabled"
                                          ]
                                          $ filtersExcludeReasons $ RunConfig NZ Connectivity
