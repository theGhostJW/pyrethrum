module TestFilter where 

import           Pyrelude as F
import OrphanedInstances()
import RunnerBase as RB
import RunElementClasses as C
import Data.List as L

type TestAddress = Text

acceptFilter :: FilterResult -> Bool
acceptFilter = isNothing . reasonForRejection

rejectFilter :: FilterResult -> Bool
rejectFilter = isJust . reasonForRejection

mkFilterResult :: TestConfigClass tc => tc -> Maybe Text -> FilterResult
mkFilterResult tc rejection = FilterResult {
                                testInfo = mkDisplayInfo tc,
                                reasonForRejection = rejection
                              }

data TestFilter rc tc = TestFilter {
  title :: Text,
  predicate :: rc -> tc -> Bool
}

type FilterList rc tc = [TestFilter rc tc]

filterTestCfg :: forall rc tc. TestConfigClass tc => FilterList rc tc -> rc -> tc -> FilterResult
filterTestCfg fltrs rc tc =
  let
    fltrRslt :: Maybe Text -> FilterResult
    fltrRslt = mkFilterResult tc 

    applyFilter :: TestFilter rc tc -> FilterResult
    applyFilter fltr = fltrRslt $ predicate fltr rc tc 
                                             ? Nothing 
                                             $ Just $ TestFilter.title fltr

    firstRejectReason :: Maybe Text
    firstRejectReason = L.find rejectFilter (applyFilter <$> fltrs) >>= reasonForRejection
  in
    fltrRslt firstRejectReason
    

filterTest :: forall i as ds tc rc e effs. TestConfigClass tc => FilterList rc tc -> rc -> GenericTest e tc rc i as ds effs -> Identity (Identity FilterResult)
filterTest fltrs rc GenericTest{ config = tc } = Identity . Identity $ filterTestCfg fltrs rc tc

filterGroups :: forall tc rc e effs. TestConfigClass tc =>
              (
                (forall i as ds. (Show i, Show as, Show ds) =>
                      GenericTest e tc rc i as ds effs -> Identity (Identity FilterResult)) -> [RunElement Identity Identity FilterResult effs]
              )
              -> FilterList rc tc
              -> rc
              -> [[FilterResult]]
filterGroups groupLst fltrs rc =
    let
      testFilter :: GenericTest e tc rc i as ds effs -> Identity (Identity FilterResult)
      testFilter = filterTest fltrs rc
    in
      (runIdentity . runIdentity <$>) <$> (tests <$> groupLst testFilter)

filterLog :: [[FilterResult]] -> [FilterResult]
filterLog = mconcat

filterGroupFlags :: [[FilterResult]] -> [Bool]
filterGroupFlags grpFltrRslts = F.any acceptFilter <$> grpFltrRslts
