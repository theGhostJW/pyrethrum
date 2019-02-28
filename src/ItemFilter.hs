
module ItemFilter where

import qualified Data.List.Safe      as SafeList
import Common 
import           Foundation.Extended
import qualified Prelude as P
import           ItemClass
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

filterredItemIds :: forall i ds. (ItemClass i ds) => ItemFilter i -> [i] -> Either FilterError (S.Set Int)
filterredItemIds filtr items =
  let
    filterredItems :: Either FilterError [Int]
    filterredItems = let
                        hasVals i = not $ null $ checkList i
                        lastWithVal = find hasVals $ reverse items
                        listOrFail lst msg = null lst
                                                    ? Left (InvalidItemFilter msg)
                                                    $ Right lst
                      in
                        (identifier <$>) <$>
                          case filtr of
                            IID iid -> listOrFail (filter (\i -> identifier i == iid) items) $ "id: " <> show iid <> " not in item list"
                            All -> listOrFail items "Items list is empty"
                            Last -> maybe (Left $ InvalidItemFilter "Items list is empty") (Right . pure) (SafeList.last items)
                            LastVal -> maybe (Left $ InvalidItemFilter "There is no item in the list with checks assigned") (Right . pure) lastWithVal
                            Pred func -> listOrFail (filter func items) "No test items match filter function"

    checkIds :: Either FilterError ()
    checkIds =
      let
        ids = identifier <$> items
        dupe = firstDuplicate ids
       in
        maybe
          (pure ())
          (\i -> Left $ DuplicateItemId i $ "Item id: " <> show i <> " is duplicated in items list.")
          dupe
     in
       S.fromList <$> (checkIds *> filterredItems)
