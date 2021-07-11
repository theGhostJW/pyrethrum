module TestFilter where 

import           Pyrelude as P
import OrphanedInstances()
import RunnerBase as RB
import RunElementClasses as C
import qualified Data.List as L 

acceptFilter :: TestFilterResult -> Bool
acceptFilter = isNothing . reasonForRejection

rejectFilter :: TestFilterResult -> Bool
rejectFilter = isJust . reasonForRejection

mkTestFilterResult :: TestConfigClass tc => tc -> Maybe Text -> TestFilterResult
mkTestFilterResult tc rejection = TestFilterResult {
                                testInfo = mkDisplayInfo tc,
                                reasonForRejection = rejection
                              }

data TestFilter rc tc = TestFilter {
  title :: Text,
  predicate :: rc -> tc -> Bool
}

applyFilters :: forall rc tc. TestConfigClass tc => [TestFilter rc tc] -> rc -> tc -> TestFilterResult
applyFilters fltrs rc tc = 
 let
  fltrRslt :: Maybe Text -> TestFilterResult
  fltrRslt = mkTestFilterResult tc 

  applyFilter :: TestFilter rc tc -> TestFilterResult
  applyFilter fltr = fltrRslt $ predicate fltr rc tc 
                                            ? Nothing 
                                            $ Just $ TestFilter.title fltr

  firstRejectReason :: Maybe Text
  firstRejectReason = L.find rejectFilter (applyFilter <$> fltrs) >>= reasonForRejection
 in
  fltrRslt firstRejectReason
    

filterTest :: forall i as ds tc hi rc e effs. TestConfigClass tc => [TestFilter rc tc] -> rc -> Test e tc rc hi i as ds effs -> TestFilterResult
filterTest fltrs rc Test{ config = tc } = applyFilters fltrs rc tc


filterLog :: forall tc rc e effs. TestConfigClass tc =>
              TestSuite e tc rc effs TestFilterResult 
              -> [TestFilter rc tc]
              -> rc
              -> [TestFilterResult]
filterLog suite fltrs rc =
  let
    testFilter :: hi -> Test e tc rc hi i as ds effs -> TestFilterResult
    testFilter _ = filterTest fltrs rc

    si :: SuiteItem () effs [TestFilterResult]
    si = suite testFilter
  in
    queryElm si



acceptAnyFilter :: [TestFilterResult] -> Bool
acceptAnyFilter = P.any acceptFilter 