module ItemFilter where

import Common
import qualified Data.Set as S
import GHC.Records (HasField (getField))
import RunElementClasses
import Prelude as P hiding (last, All, Last)
import Check (Checks, un)
import GHC.Show
import PyrethrumExtras
import List.Extra

data ItemFilter a
  = ID Int
  | All
  | Last
  | LastWithCheck -- return the last item with non-mempty checks
  | Pred (a -> Bool)

instance Show (ItemFilter a) where
  show :: ItemFilter a -> String
  show (ID i) = "ID " <> P.show i
  show All = "All"
  show Last = "Last"
  show LastWithCheck = "LastWithCheck"
  show (Pred _) = "Pred itemPredicateFunction"

filterredItemIds :: forall i ds. (HasField "id" i Int, HasField "checks" i (Checks ds)) => ItemFilter i -> [i] -> Either FilterErrorType (S.Set Int)
filterredItemIds filtr items =
  let getId = getField @"id"
      filterredItems :: Either FilterErrorType [Int]
      filterredItems =
        let pass' = Right . pure
            fail' = Left . InvalidItemFilter
            hasChecks i = not $ null i.checks.un
            lastWithChecks = find hasChecks $ reverse items
            listOrFail lst msg =
              null lst
                ? Left (InvalidItemFilter msg)
                $ Right lst
         in (getId <$>)
              <$> case filtr of
                ID targetId -> listOrFail (filter (\i -> targetId == getId i) items) $ "id: " <> txt targetId <> " not in item list"
                All -> listOrFail items "Items list is empty"
                Last -> maybe (fail' "Items list is empty") pass' (last items)
                LastWithCheck -> maybe (fail' "There is no item in the list with checks assigned") pass' lastWithChecks
                Pred func -> listOrFail (filter func items) "No test items match filter function"

      checkIds :: Either FilterErrorType ()
      checkIds =
        let ids = getId <$> items
            dupe = firstDuplicate ids
         in maybe
              (pure ())
              (\i -> Left $ DuplicateItemId i $ "Item id: " <> txt i <> " is duplicated in items list.")
              dupe
   in S.fromList <$> (checkIds *> filterredItems)
