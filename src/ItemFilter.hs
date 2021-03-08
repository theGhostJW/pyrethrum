
module ItemFilter where

import Common 
import           Prelude
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
filterredItemIds filtr items = undefined