module RunnerItemFiltersTest where

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
import RunnerShared

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
