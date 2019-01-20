module ItemFiltersTest where

import qualified Check           as C
import           Foundation      as F
import           Data.Set        as S
import qualified Prelude         as P
import           Runner.Internal.ItemFilters
import           Runner as R
import           Test.Extended
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types
import OrphanedInstances


data TestItem = TestItem {
  iid    :: Int,
  pre    :: String,
  post   :: String,
  checks :: C.CheckList ValState
} deriving (Show, Generic)

instance ToJSON TestItem where
  toEncoding = genericToEncoding defaultOptions

type ValState = Int

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

chkFilterError flter msg itms = chkLeftContains msg $ filterredItemIds flter (itms ::  [TestItem])

chkFilter flter expted itms = chkEq (Right $ S.fromList expted) $ filterredItemIds flter itms
chkFilter' flter expted itms = chkEq (Right $ S.singleton expted) $ filterredItemIds flter itms

chkSingleton :: (ItemClass item valState) => ItemFilter item -> [item] -> Assertion
chkSingleton flter itms = either (\_ -> chk False) (chkEq (1 :: Int)) $ P.length <$> filterredItemIds flter itms

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
