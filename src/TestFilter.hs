module TestFilter where 

import           Pyrelude as F
import Data.Aeson.TH
import OrphanedInstances
import Data.Functor.Identity
import RunnerBase as RB
import RunElementClasses as C
import Data.List as L
import Data.Aeson

type TestAddress = String

acceptFilter :: FilterResult -> Bool
acceptFilter = isNothing . reasonForRejection

rejectFilter :: FilterResult -> Bool
rejectFilter = isJust . reasonForRejection

mkFilterResult :: TestConfigClass tc => tc -> Maybe String -> FilterResult
mkFilterResult tc rejection = FilterResult {
                                testInfo = mkDisplayInfo tc,
                                reasonForRejection = rejection
                              }

data TestFilter rc tc = TestFilter {
  title :: String,
  predicate :: rc -> tc -> Bool
}

type FilterList rc tc = [TestFilter rc tc]

filterTestCfg :: forall rc tc. TestConfigClass tc => FilterList rc tc -> rc -> tc -> FilterResult
filterTestCfg fltrs rc tc =
  let
    fltrRslt :: Maybe String -> FilterResult
    fltrRslt = mkFilterResult tc 

    applyFilter :: TestFilter rc tc -> FilterResult
    applyFilter fltr = fltrRslt $ predicate fltr rc tc 
                                             ? Nothing 
                                             $ Just $ TestFilter.title fltr

    firstRejectReason :: Maybe String
    firstRejectReason = L.find rejectFilter (applyFilter <$> fltrs) >>= reasonForRejection
  in
    fltrRslt firstRejectReason
    

filterTest :: forall i as ds tc rc effs. TestConfigClass tc => FilterList rc tc -> rc -> GenericTest tc rc i effs as ds -> Identity (Identity FilterResult)
filterTest fltrs rc GenericTest{..} = Identity . Identity $ filterTestCfg fltrs rc configuration

filterGroups :: forall tc rc effs. TestConfigClass tc =>
              (
                (forall i as ds. (Show i, Show as, Show ds) =>
                      GenericTest tc rc i effs as ds -> Identity (Identity FilterResult)) -> [TestGroup Identity Identity FilterResult effs]
              )
              -> FilterList rc tc
              -> rc
              -> [[FilterResult]]
filterGroups groupLst fltrs rc =
    let
      testFilter :: GenericTest tc rc i effs as ds -> Identity (Identity FilterResult)
      testFilter = filterTest fltrs rc
    in
      (runIdentity . runIdentity <$>) <$> (tests <$> groupLst testFilter)

filterLog :: [[FilterResult]] -> [FilterResult]
filterLog = mconcat

filterGroupFlags :: [[FilterResult]] -> [Bool]
filterGroupFlags grpFltrRslts = F.any acceptFilter <$> grpFltrRslts
