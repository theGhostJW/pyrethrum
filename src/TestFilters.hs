module TestFilters where


--- Reapplying test Filters to Items ---

import RunElementClasses
import TestFilter
import Data.Maybe
import Data.Tuple
import Control.Applicative
import Prelude
import Data.Text


applyTestFilters :: forall i tc rc. TestConfigClass tc => [TestFilter rc tc] -> rc -> (i -> tc) -> [i] -> [i]
applyTestFilters fltrs rc cvtr itms = 
    fst <$> Prelude.filter (isNothing . snd) (applyTestFiltersToItemsShowReason fltrs rc cvtr itms) 

-- de bugging
applyTestFiltersToItemsShowReason :: forall i tc rc. TestConfigClass tc => [TestFilter rc tc] -> rc -> (i -> tc) -> [i] -> [(i, Maybe Text)]
applyTestFiltersToItemsShowReason fltrs rc cvtr itms = 
  let 
    fltrItm :: i -> (i, Maybe Text)
    fltrItm i = (i, reasonForRejection . filterTestCfg fltrs rc $ cvtr i)
  in 
    fltrItm <$> itms