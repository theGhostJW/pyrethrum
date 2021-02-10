
module ItemFilter where

import Common 
import           Pyrelude
import RunElementClasses
import qualified Prelude as P
import qualified Data.Set as S

data ItemFilter a = IID Int |
                All |
                Last |
                LastVal | -- return the last item with non-mempty validations
                Pred (a -> Bool)

instance P.Show (ItemFilter a) where
  show (IID i )    = "IID " <> P.show i
  show All         = "All"
  show Last        = "Last"
  show LastVal     = "LastVal"
  show (Pred _) = "Pred itemPredicateFunction"

filterredItemIds :: forall i ds. (ItemClass i ds) => ItemFilter i -> [i] -> Either FilterErrorType (S.Set Int)
filterredItemIds filtr items =
  let
    filterredItems :: Either FilterErrorType [Int]
    filterredItems = let
                        hasVals i = not $ nullFoldable $ checkList i
                        lastWithVal = findFoldable hasVals $ reverse items
                        listOrFail lst msg = null lst
                                                    ? Left (InvalidItemFilter msg)
                                                    $ Right lst
                      in
                        (identifier <$>) <$>
                          case filtr of
                            IID iid -> listOrFail (filter (\i -> identifier i == iid) items) $ "id: " <> txt iid <> " not in item list"
                            All -> listOrFail items "Items list is empty"
                            Last -> maybe (Left $ InvalidItemFilter "Items list is empty") (Right . pure) (last items)
                            LastVal -> maybe (Left $ InvalidItemFilter "There is no item in the list with checks assigned") (Right . pure) lastWithVal
                            Pred func -> listOrFail (filter func items) "No test items match filter function"

    checkIds :: Either FilterErrorType ()
    checkIds =
      let
        ids = identifier <$> items
        dupe = firstDuplicate ids
       in
        maybe
          (pure ())
          (\i -> Left $ DuplicateItemId i $ "Item id: " <> txt i <> " is duplicated in items list.")
          dupe
     in
       S.fromList <$> (checkIds *> filterredItems)
