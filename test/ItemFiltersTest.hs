module ItemFiltersTest where

import qualified Check         as C
import           Pyrelude      as P
import           Data.Set      as S
import           ItemFilter
import           Runner as R hiding (items)
import           Pyrelude.Test
import Data.Aeson.TH
import Data.Aeson.Types
-- import           DemoProject.Config as CFG


data TestItem = TestItem {
  iid    :: Int,
  pre    :: Text,
  post   :: Text,
  checks :: C.CheckDList DState
} deriving (Show, Generic)

instance ToJSON TestItem where
  toEncoding = genericToEncoding defaultOptions

type DState = Int

instance ItemClass TestItem DState where
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

chkFilterError flter msg itms = chkLeftContains msg $ filterredItemIds @TestItem @DState flter (itms ::  [TestItem])

chkFilter :: ItemClass TestItem DState => ItemFilter TestItem -> [Int] -> [TestItem] -> Assertion
chkFilter flter expted itms = chkEq (Right $ S.fromList expted) $ filterredItemIds @TestItem @DState flter itms

chkFilter' :: ItemClass TestItem DState => ItemFilter TestItem -> Int -> [TestItem] -> Assertion
chkFilter' flter expted itms = chkEq (Right $ S.singleton expted) $ filterredItemIds @TestItem @DState flter itms

chkSingleton :: forall i ds. (ItemClass i ds) => ItemFilter i -> [i] -> Assertion
chkSingleton flter itms = P.either (\_ -> chk False) (chkEq (1 :: Int)) $ lengthFoldable <$> filterredItemIds @i @ds flter itms

blahh :: IO ()
blahh = undefined

--
unit_item_filter_iid = chkFilter' (IID 120) 120  items
unit_item_filter_iid_first = chkFilter' (IID 100) 100 items
unit_item_filter_iid_last = chkFilter' (IID 150) 150 items
unit_item_filter_iid_missing = chkFilterError (IID 1200) "not in item list" items
--
-- -- Last
unit_item_filter_last = chkFilter' Last 150 items
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

chkLastVal = chkFilter' LastVal

unit_item_filter_lastVal_singleFirst = chkLastVal 100 items1
unit_item_filter_lastVal_top2 = chkLastVal 110 items2
unit_item_filter_lastVal_bottom = chkLastVal 150 items3
unit_item_filter_lastVal_middle = chkLastVal 130 items4

-- Pred (a -> Bool)
unit_item_filter_pred = chkFilter' (Pred $ \ii -> 150 == iid ii) 150 items

unit_item_filter_pred_toList = chkFilter (Pred $ \ii -> 110 < iid ii) [120,  130, 140, 150] items
unit_item_filter_pred_missing = chkFilterError (Pred $ \ii -> 190 == iid ii) "No test items match filter function" items

 -- All
unit_item_filter_all = chkFilter All [100, 110, 120, 130, 140, 150] items
unit_item_filter_all_empty = chkFilterError All "is empty" []

itemsDupeId = [
        i 100 "Pre" "Post" mempty,
        i 110 "Pre" "Post" mempty,
        i 120 "Pre" "Post" isOne,
        i 130 "Pre" "Post" isOne,
        i 120 "Pre" "Post" mempty,
        i 150 "Pre" "Post" mempty
      ]

unit_item_filter_dupe_error = chkFilterError All "Item id: 120 is duplicated in items list." itemsDupeId

----------------------------------------------------------------------------------------------
------------------------ Apply Test Filters to Items (applyTestFilters) ----------------------
----------------------------------------------------------------------------------------------

sampleItems = P.take 99 [1..]

-- converter :: Int -> TestConfig
-- converter i' = defaultConfig { 
--                                 countries = case i' `mod` 3 of
--                                                   0 -> auOnly
--                                                   1 -> nzOnly 
--                                                   _ -> allCountries
--                           }

-- runcfg = CFG.runConfig {country = AU}

-- auTestItems = applyTestFiltersToItems runcfg converter sampleItems 

-- -- expect 1/3 of the 99 nzOnly to be filterred out due to country filter
-- unit_filter_items_length = 66 ... length auTestItems

-- -- no items mapped to NZ only should be present
-- unit_filter_items = Nothing ... find (\i' -> i' `mod` 3 == 1) auTestItems
