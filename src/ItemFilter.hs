
module ItemFilter where

import Common
import           Pyrelude
import RunElementClasses
import qualified Prelude as P
import qualified Data.Set as S
import GHC.Records ( HasField(getField) )


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
    getId = getField @"id"
    filterredItems :: Either FilterErrorType [Int]
    filterredItems = let
                        pass  = Right . pure
                        fail' = Left . InvalidItemFilter
                        hasVals i = not $ nullFoldable $ checkList @i @ds i
                        lastWithVal = findFoldable hasVals $ reverse items
                        listOrFail lst msg = null lst
                                                    ? Left (InvalidItemFilter msg)
                                                    $ Right lst
                      in
                        (getId <$>) <$>
                          case filtr of
                            IID targetId -> listOrFail (filter (\i -> targetId == getId i) items) $ "id: " <> txt targetId <> " not in item list"
                            All -> listOrFail items "Items list is empty"
                            Last -> maybe (fail' "Items list is empty") pass (last items)
                            LastVal -> maybe (fail' "There is no item in the list with checks assigned") pass lastWithVal
                            Pred func -> listOrFail (filter func items) "No test items match filter function"

    checkIds :: Either FilterErrorType ()
    checkIds =
      let
        ids = getId <$> items
        dupe = firstDuplicate ids
       in
        maybe
          (pure ())
          (\i -> Left $ DuplicateItemId i $ "Item id: " <> txt i <> " is duplicated in items list.")
          dupe
     in
       S.fromList <$> (checkIds *> filterredItems)
