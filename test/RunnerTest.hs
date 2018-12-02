
{-# LANGUAGE AllowAmbiguousTypes #-}

module RunnerTest where

import qualified Check           as C
import           Data.List.Safe  as SafeList
import           Foundation      as F
import qualified Prelude         as P
import           Runner.Internal
import           Runner as R
import           Test.Extended
import           ItemClass
import DSL.Interpreter
import TestAndRunConfig


data TestItem = TestItem {
  iid    :: Int,
  pre    :: String,
  post   :: String,
  checks :: C.CheckList ValState
} deriving (Show)

type ValState = Int

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test Filters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data RunConfig = RunConfig {
  country :: Country,
  level :: TestDepth
}

data TestConfig = TestConfig {
  header :: String,
  address :: String,
  countries :: [Country],
  level :: TestDepth,
  enabled :: Bool
}  deriving Show

instance TestConfigClass TestConfig where
  moduleAddress = address

instance Titled TestConfig where
  title = header

type TST = GenericTest TestConfig RunConfig

newtype MyInt = MyInt Int deriving Show

instance ItemClass MyInt MyInt where
  identifier _ =  -999
  whenClause _ =  "pre"
  thenClause _ =  "post"
  checkList = mempty

data TestDepth = Connectivity | Regression | DeepRegression deriving (Eq, Ord, Show)
data Country = Au |NZ deriving (Eq, Ord, Show)

au = [Au]
nz = [NZ]

test1 :: TST MyInt effs MyInt MyInt
test1 = GenericTest {
              configuration = TestConfig {
                header = "test1",
                address = "test1",
                countries = au <> nz,
                level = Regression,
                enabled = True
              },
              components = undefined
            }

test2 :: TST MyInt effs MyInt MyInt
test2 = GenericTest {
              configuration = TestConfig {
                header = "test2",
                address = "test2",
                countries = nz,
                level = Regression,
                enabled = True
              },
              components = undefined
            }

test3 :: TST MyInt effs MyInt MyInt
test3 = GenericTest {
                configuration = TestConfig {
                  header = "test3",
                  address = "test3",
                  countries = au,
                  level = Connectivity,
                  enabled = True
                },
                components = undefined
            }

test4 :: TST MyInt effs MyInt MyInt
test4 = GenericTest {
              configuration = TestConfig {
                  header = "test4",
                  address = "test4",
                  countries = au,
                  level = DeepRegression,
                  enabled = True
                },
              components = undefined
            }

test5 :: TST MyInt effs MyInt MyInt
test5 = GenericTest {
              configuration = TestConfig {
                  header = "test5",
                  address = "test5",
                  countries = au,
                  level = DeepRegression,
                  enabled = False
                },
              components = undefined
            }

runRunner :: forall m m1 effs a.
                (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest TestConfig RunConfig i effs as vs -> m1 (m a))
                -> [TestGroup m1 m a effs]
runRunner f =
  [

   TestGroup {
          rollover = doNothing,
          goHome = doNothing,
          tests = [
              f test1,
              f test2,
              f test3
            ]
     },

    TestGroup {
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
     predicate = \rc tc -> enabled tc
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

filters :: [TestFilter RunConfig TestConfig]
filters = [enabledFilter, countryFilter, levelFilter]

filterList :: RunConfig -> [Either (FilterRejection TestConfig) TestConfig]
filterList rc = filterLog $ filterGroups runRunner filters rc

runFilters :: RunConfig -> [String]
runFilters rc = header <$> rights (filterList rc)

chkFilters :: [String] -> RunConfig -> Assertion
chkFilters expted rc = chkEq expted $ runFilters rc

unit_test_filter_expect_empty = chkFilters [] $ RunConfig NZ Connectivity
unit_test_filter_country = chkFilters ["test1", "test3"] $ RunConfig Au Regression
unit_test_filter_country_nz = chkFilters ["test1", "test2"] $ RunConfig NZ Regression
unit_test_filter_country2 = chkFilters ["test1", "test3", "test4"] $ RunConfig Au DeepRegression


filtersExcludeReasons :: RunConfig -> [String]
filtersExcludeReasons rc = reason <$> lefts (filterList rc)

unit_test_filter_exclude_reasons = chkEq [
                                          "depth must be within run parameters (e.g. regression test will not be run in connectiviity run)",
                                          "depth must be within run parameters (e.g. regression test will not be run in connectiviity run)",
                                          "country must match test run",
                                          "country must match test run",
                                          "test must be is enabled"
                                          ]
                                          $ filtersExcludeReasons $ RunConfig NZ Connectivity

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Item Filters %%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instance ItemClass TestItem ValState where
  identifier = iid
  whenClause = pre
  thenClause = post
  checkList = checks

i = TestItem

isOne = C.chk "is One" (== 1)

items  = [
          i 100 "Pre" "Post" mempty,
          i 110 "Pre" "Post" mempty,
          i 120 "Pre" "Post" mempty,
          i 130 "Pre" "Post" mempty,
          i 140 "Pre" "Post" mempty,
          i 150 "Pre" "Post" mempty
        ]

chkFilterError flter msg itms = chkErrorContains show msg $ filterredItems flter (itms ::  [TestItem])

chkFilter flter expted extractor itms = chkEq (Right expted) $ extractor <$> filterredItems flter itms

chkSingleton :: (ItemClass item valState) => Filter item -> [item] -> Assertion
chkSingleton flter itms = either (\r -> chk False) (chkEq (1 :: Int)) $ P.length <$> filterredItems flter itms

blahh :: IO ()
blahh = undefined

chkFilterSingle flter expted extractor itms = chkSingleton flter itms >> chkEq (Right expted) (extractor <$> filterredItems flter itms)

-- -- IID Int
idOfHead lst = iid <$> SafeList.head lst
--
unit_item_filter_iid = chkFilterSingle (IID 120) (Just 120) idOfHead items
unit_item_filter_iid_first = chkFilterSingle (IID 100) (Just 100) idOfHead items
unit_item_filter_iid_last = chkFilterSingle (IID 150) (Just 150) idOfHead items
unit_item_filter_iid_missing = chkFilterError (IID 1200) "not in item list" items
--
-- -- Last
unit_item_filter_last = chkFilterSingle Last (Just 150) idOfHead items
unit_item_filter_last_empty = chkFilterError Last "is empty" []
--
-- LastVal
items1  = [
         i 100 "Pre" "Post" isOne,
         i 110 "Pre" "Post" mempty,
         i 120 "Pre" "Post" mempty,
         i 130 "Pre" "Post" mempty,
         i 140 "Pre" "Post" mempty,
         i 150 "Pre" "Post" mempty
       ]

items2  = [
         i 100 "Pre" "Post" isOne,
         i 110 "Pre" "Post" isOne,
         i 120 "Pre" "Post" mempty,
         i 130 "Pre" "Post" mempty,
         i 140 "Pre" "Post" mempty,
         i 150 "Pre" "Post" mempty
       ]

items3  = [
        i 100 "Pre" "Post" isOne,
        i 110 "Pre" "Post" isOne,
        i 120 "Pre" "Post" mempty,
        i 130 "Pre" "Post" mempty,
        i 140 "Pre" "Post" mempty,
        i 150 "Pre" "Post" isOne
      ]

items4 = [
        i 100 "Pre" "Post" mempty,
        i 110 "Pre" "Post" mempty,
        i 120 "Pre" "Post" isOne,
        i 130 "Pre" "Post" isOne,
        i 140 "Pre" "Post" mempty,
        i 150 "Pre" "Post" mempty
      ]

unit_item_filter_lastVal_no_items_with_vals = chkFilterError LastVal "There is no item in the list with checks assigned" items

chkLastVal expectedId = chkFilterSingle LastVal (Just expectedId) idOfHead

unit_item_filter_lastVal_singleFirst = chkLastVal 100 items1
unit_item_filter_lastVal_top2 = chkLastVal 110 items2
unit_item_filter_lastVal_bottom = chkLastVal 150 items3
unit_item_filter_lastVal_middle = chkLastVal 130 items4

-- Pred (a -> Bool)
unit_item_filter_pred = chkFilterSingle (Pred $ \ii -> 150 == iid ii) (Just 150) idOfHead items

unit_item_filter_pred_toList = chkFilter (Pred $ \ii -> 110 < iid ii) [120,  130, 140, 150] (iid <$>) items
unit_item_filter_pred_missing = chkFilterError (Pred $ \ii -> 190 == iid ii) "No test items match filter function" items

 -- All
unit_item_filter_all = chkFilter All 6 SafeList.length items
unit_item_filter_all_empty = chkFilterError All "is empty" []
