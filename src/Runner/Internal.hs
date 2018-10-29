
module Runner.Internal where

import DSL.Ensure
import Control.Monad.Freer
import qualified Data.List.Safe      as SafeList
import           Foundation.Extended
import qualified Prelude
import           ItemClass

data Filter a = IID Int |
                All |
                Last |
                LastVal | -- return the last item with non-mempty validations
                Pred (a -> Bool)


instance Prelude.Show (Filter a) where
  show (IID i )    = "IID " <> Prelude.show i
  show All         = "All"
  show Last        = "Last"
  show LastVal     = "LastVal"
  show (Pred func) = "Pred itemPredicateFunction"

data FilterError = InvalidItemFilter String  |
                   NotImplemented String
                   deriving (Eq, Show)

filterredItems :: (ItemClass item valState) => Filter item -> [item] -> Either FilterError [item]
filterredItems filtr items = let
                              hasVals i = not $ null $ checkList i
                              lastWithVal = find hasVals $ reverse items
                              listOrFail lst msg = null lst
                                                          ? Left (InvalidItemFilter msg)
                                                          $ Right lst
                              in case filtr of
                                IID iid -> listOrFail (filter (\i -> identifier i == iid) items) $ "id: " <> show iid <> " not in item list"
                                All -> listOrFail items "Items list is empty"
                                Last -> maybe (Left $ InvalidItemFilter "Items list is empty") (Right . pure) (SafeList.last items)
                                LastVal -> maybe (Left $ InvalidItemFilter "There is no item in the list with checks assigned") (Right . pure) lastWithVal
                                Pred func -> listOrFail (filter func items) "No test items match filter function"
