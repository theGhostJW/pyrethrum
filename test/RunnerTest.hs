
module RunnerTest where

import           Data.List.Safe  as SafeList
import           Foundation      hiding (Item)
import qualified Prelude
import           Runner.Internal
import           Test.Extended
import           TestItem

data Item = Item {
  iid  :: Int,
  pre  :: String,
  post :: String
} deriving (Eq, Show)

instance TestItem Item Item where
  identifier = iid
  whenClause = pre
  thenClause = post
  validation = mempty

i = Item

items  = [
          i 100 "Pre"  "Post",
          i 110 "Pre"  "Post",
          i 120 "Pre"  "Post",
          i 130 "Pre"  "Post",
          i 140 "Pre"  "Post",
          i 150 "Pre"  "Post"
        ]

chkFilterError flter msg itms = chkErrorContains show msg $ filterredItems flter (itms ::  [Item])
chkFilter flter expted extractor itms = chkEq (Right expted) $ extractor <$> filterredItems flter itms

-- IID Int
unit_filter_iid = chkFilter (IID 120) (Just 120) (\is -> iid <$> SafeList.head is) items
unit_filter_iid_first = chkFilter (IID 100) (Just 100) (\is -> iid <$> SafeList.head is) items
unit_filter_iid_last = chkFilter (IID 150) (Just 150) (\is -> iid <$> SafeList.head is) items
unit_filter_iid_missing = chkFilterError (IID 1200) "not in item list" items

-- Last
unit_filter_last = chkFilter Last (Just 150) (\is -> iid <$> SafeList.head is) items
unit_filter_last_empty = chkFilterError Last "is empty" []

-- LastVal
-- TODO: Implemnt

-- Pred (a -> Bool)
unit_filter_pred = chkFilter (Pred $ \ii -> 150 == iid ii) (Just 150) (\is -> iid <$> SafeList.head is) items
unit_filter_pred_missing = chkFilterError (Pred $ \ii -> 190 == iid ii) "No test items match filter function" items

-- All
unit_filter_all = chkFilter All 6 SafeList.length items
unit_filter_all_empty = chkFilterError All "is empty" []
