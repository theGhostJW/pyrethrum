module TestFilters where


--- Reapplying test Filters to Items ---

import RunElementClasses
import TestFilter
import Data.Maybe
import Data.Tuple
import Control.Applicative
import Pyrelude ( ($), Text, Listy(filter), Category((.)) )


applyTestFilters :: forall i tc rc. Config tc => [TestFilter rc tc] -> rc -> ModuleDomain -> (i -> tc) -> [i] -> [i]
applyTestFilters fltrs rc md cvtr itms = 
    fst <$> filter (isNothing . snd) (applyTestFiltersToItemsShowReason fltrs rc md cvtr itms) 

-- debugging
applyTestFiltersToItemsShowReason :: forall i tc rc. Config tc => [TestFilter rc tc] -> rc -> ModuleDomain -> (i -> tc) -> [i] -> [(i, Maybe Text)]
applyTestFiltersToItemsShowReason fltrs rc md cvtr itms = 
  let 
    fltrItm :: i -> (i, Maybe Text)
    fltrItm i = (i, reasonForRejection . applyFilters fltrs rc md $ cvtr i)
  in 
    fltrItm <$> itms