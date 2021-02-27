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
    

filterTest :: forall i as ds tc rc e effs. TestConfigClass tc => [TestFilter rc tc] -> rc -> Test e tc rc i as ds effs -> Identity (Identity TestFilterResult)
filterTest fltrs rc Test{ config = tc } = Identity . Identity $ applyFilters fltrs rc tc

filterRunElements :: forall tc rc e effs. TestConfigClass tc =>
              (
                (forall i as ds. (Show i, Show as, Show ds) =>
                      Test e tc rc i as ds effs -> Identity (Identity TestFilterResult)) -> RunElement Identity Identity TestFilterResult effs
              )
              -> [TestFilter rc tc]
              -> rc
              -> [TestFilterResult]
filterRunElements groupLst fltrs rc =
    let
      testFilter :: Test e tc rc i as ds effs -> Identity (Identity TestFilterResult)
      testFilter = filterTest fltrs rc
    in
      runIdentity . runIdentity <$> (tests $ groupLst testFilter)

acceptAnyFilter :: [TestFilterResult] -> Bool
acceptAnyFilter = P.any acceptFilter 