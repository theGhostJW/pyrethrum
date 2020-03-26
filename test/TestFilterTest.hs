module TestFilterTest where

import Pyrelude         as P
import           Runner as R
import           Pyrelude.Test
import Data.Yaml
import Data.Aeson.TH
import Data.Aeson.Types
import TestFilter

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
  address :: TestModule,
  countries :: [Country],
  level :: TestDepth,
  enabled :: Bool
}  deriving (Show ,Eq)

instance TestConfigClass TestConfig where
  moduleAddress = address

instance Titled TestConfig where
  title = header

$(deriveJSON defaultOptions ''TestConfig)

type TST = GenericTest TestConfig RunConfig

newtype MyInt = MyInt Int deriving (Show, Generic)

instance ItemClass MyInt MyInt where
  identifier _ =  -999
  whenClause _ =  "pre"
  thenClause _ =  "post"
  checkList = mempty


instance ToJSON MyInt where
  toEncoding = genericToEncoding defaultOptions

au = [Au]
nz = [NZ]

test1 :: TST MyInt MyInt MyInt effs
test1 = GenericTest {
              configuration = TestConfig {
                header = "test1",
                address = TestModule "test1",
                countries = au <> nz,
                level = Regression,
                enabled = True
              },
              components = undefined
            }

test2 :: TST MyInt MyInt MyInt effs
test2 = GenericTest {
              configuration = TestConfig {
                header = "test2",
                address = TestModule "test2",
                countries = nz,
                level = Regression,
                enabled = True
              },
              components = undefined
            }

test3 :: TST MyInt MyInt MyInt effs
test3 = GenericTest {
                configuration = TestConfig {
                  header = "test3",
                  address = TestModule "test3",
                  countries = au,
                  level = Connectivity,
                  enabled = True
                },
                components = undefined
            }

test4 :: TST MyInt MyInt MyInt effs 
test4 = GenericTest {
              configuration = TestConfig {
                  header = "test4",
                  address = TestModule "test4",
                  countries = au,
                  level = DeepRegression,
                  enabled = True
                },
              components = undefined
            }

test5 :: TST MyInt MyInt MyInt effs
test5 = GenericTest {
              configuration = TestConfig {
                  header = "test5",
                  address = TestModule "test5",
                  countries = au,
                  level = DeepRegression,
                  enabled = False
                },
              components = undefined
            }

runRunner :: forall m m1 effs a.
                (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds) => GenericTest TestConfig RunConfig i as ds effs -> m1 (m a))
                -> [TestGroup m1 m a effs]
runRunner f =
  [

   TestGroup {
          header = "Group 1",
          rollover = doNothing,
          goHome = doNothing,
          tests = [
              f test1,
              f test2,
              f test3
            ]
     },

    TestGroup {
          header = "Group 2",
          rollover = doNothing,
          goHome = doNothing,
          tests = [
              f test4,
              f test5
            ]
     }

    ]


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

filterList :: RunConfig -> [FilterResult]
filterList rc = filterLog $ filterGroups runRunner filters' rc

runFilters :: RunConfig -> [Text]
runFilters rc = testTitle . testInfo <$> P.filter acceptFilter (filterList rc)

chkFilters :: [Text] -> RunConfig -> Assertion
chkFilters expted rc = chkEq expted $ runFilters rc

unit_test_filter_expect_empty = chkFilters [] $ RunConfig NZ Connectivity
unit_test_filter_country = chkFilters ["test1", "test3"] $ RunConfig Au Regression
unit_test_filter_country_nz = chkFilters ["test1", "test2"] $ RunConfig NZ Regression
unit_test_filter_country2 = chkFilters ["test1", "test3", "test4"] $ RunConfig Au DeepRegression


filtersExcludeReasons :: RunConfig -> [Text]
filtersExcludeReasons rc = catMaybes $ reasonForRejection <$> P.filter rejectFilter (filterList rc)

unit_test_filter_exclude_reasons = chkEq [
                                          "depth must be within run parameters (e.g. regression test will not be run in connectiviity run)",
                                          "depth must be within run parameters (e.g. regression test will not be run in connectiviity run)",
                                          "country must match test run",
                                          "country must match test run",
                                          "test must be is enabled"
                                          ]
                                          $ filtersExcludeReasons $ RunConfig NZ Connectivity
