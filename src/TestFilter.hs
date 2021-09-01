module TestFilter where 

import           Pyrelude as P
import OrphanedInstances()
import RunnerBase as RB
import RunElementClasses as C
import qualified Data.List as L 
import Internal.RunnerBaseLazy (IsRoot)
import RunnerBase (AddressedElm(..))

acceptFilter :: TestFilterResult -> Bool
acceptFilter = isNothing . reasonForRejection

rejectFilter :: TestFilterResult -> Bool
rejectFilter = isJust . reasonForRejection

mkTestFilterResult :: Config tc => Address -> tc -> Maybe Text -> TestFilterResult
mkTestFilterResult d tc rejection = TestFilterResult {
                                testInfo = mkTestLogInfo d tc,
                                reasonForRejection = rejection
                              }

data TestFilter rc tc = TestFilter {
  title :: rc -> Address -> tc -> Text,
  predicate :: rc -> Address -> tc -> Bool
}

applyFilters :: forall rc tc. Config tc => [TestFilter rc tc] -> rc -> Address -> tc -> TestFilterResult
applyFilters fltrs rc adrs tc = 
 let
  fltrRslt :: Maybe Text -> TestFilterResult
  fltrRslt = mkTestFilterResult adrs tc 

  applyFilter :: TestFilter rc tc -> TestFilterResult
  applyFilter fltr = fltrRslt $ predicate fltr rc adrs tc 
                                            ? Nothing 
                                            $ Just $ TestFilter.title fltr rc adrs tc 

  firstRejectReason :: Maybe Text
  firstRejectReason = L.find rejectFilter (applyFilter <$> fltrs) >>= reasonForRejection
 in
  fltrRslt firstRejectReason
    

filterTest :: forall i as ds tc hi rc e effs. Config tc => [TestFilter rc tc] -> rc -> Address -> Test e tc rc hi i as ds effs -> TestFilterResult
filterTest fltrs rc d Test{ config = tc } = applyFilters fltrs rc d tc


filterLog :: forall tc rc e effs. Config tc =>
              TestSuite e tc rc effs TestFilterResult 
              -> [TestFilter rc tc]
              -> rc
              -> [TestFilterResult]
filterLog suite fltrs rc =
  let
    testFilter :: Address -> hi -> Test e tc rc hi i as ds effs -> TestFilterResult
    testFilter adr _ = filterTest fltrs rc adr

    si :: SuiteItem IsRoot () effs [TestFilterResult]
    si = suite testFilter
  in
    element <$> querySuite (C.title . testInfo) si



acceptAnyFilter :: [TestFilterResult] -> Bool
acceptAnyFilter = P.any acceptFilter 