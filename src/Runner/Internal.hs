
module Runner.Internal where

import           AppError
import qualified Data.List.Safe      as SafeList
import           Foundation.Extended

class TestItem a where
  identifier :: a -> Int
  whenClause :: a -> String
  thenClause :: a -> String
  whenThen :: a -> String
  whenThen a = "When: " <> whenClause a  <> "\n" <>
               "Then: " <> thenClause a

data Filter a = IID Int |
               All |
               Last |
               LastVal |
               Pred (a -> Bool)


filterredItems :: (TestItem item) => Filter item -> [item] -> Either AppError [item]
filterredItems filtr items = let
                              listOrFail lst msg = null lst
                                                          ? Left (InvalidItemFilter msg)
                                                          $ Right lst
                              in case filtr of
                                IID iid -> listOrFail (filter (\i -> identifier i == iid) items) $ "id: " <> show iid <> " not in item list"
                                All -> listOrFail items "Items list is empty"
                                Last -> maybe (Left $ InvalidItemFilter "Items list is empty") (Right . pure) (SafeList.last items)
                                LastVal -> Left $ NotImplemented "LastVal handler not implemented in runTest"
                                Pred func -> listOrFail (filter func items) "No test items match filter function"
