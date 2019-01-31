module TestFilter where 

import           Foundation.Extended as F
import Data.Aeson.TH
import OrphanedInstances
import Data.Functor.Identity
import RunnerBase as RB
import TestAndRunConfig as C
import Data.List as L

type TestAddress = String

-- this result is ultimately serialsed to JSON as part of the log protocol data  
-- type and can't serialise with custom typeclass constraints so forced to
-- have the redundant testModAddress and testTitle even though this
-- data is available via TestConfigClass
data FilterResult tc = FilterResult {
  testInfo  :: TestDisplayInfo tc,
  reasonForRejection :: Maybe String
}  deriving Show

deriving instance (Eq tc) => Eq (FilterResult tc)

acceptFilter :: FilterResult tc -> Bool
acceptFilter = isNothing . reasonForRejection

rejectFilter :: FilterResult tc -> Bool
rejectFilter = isJust . reasonForRejection

$(deriveJSON defaultOptions ''FilterResult)

mkFilterResult :: TestConfigClass tc => tc -> Maybe String -> FilterResult tc
mkFilterResult tc rejection= FilterResult {
                                testInfo = mkDisplayInfo tc,
                                reasonForRejection = rejection
                              }

data TestFilter rc tc = TestFilter {
  title :: String,
  predicate :: rc -> tc -> Bool
}

type FilterList rc tc = [TestFilter rc tc]

filterTestCfg :: forall rc tc. TestConfigClass tc => FilterList rc tc -> rc -> tc -> FilterResult tc
filterTestCfg fltrs rc tc =
  let
    applyFilter :: TestFilter rc tc -> FilterResult tc
    applyFilter fltr = mkFilterResult tc $ predicate fltr rc tc 
                                             ? Nothing 
                                             $ Just $ TestFilter.title fltr

    firstRejectReason :: Maybe String
    firstRejectReason = (L.find rejectFilter $ applyFilter <$> fltrs) >>= reasonForRejection
  in
    mkFilterResult tc firstRejectReason
    

filterTest :: forall i as vs tc rc effs. TestConfigClass tc => FilterList rc tc -> rc -> GenericTest tc rc i effs as vs -> Identity (Identity (FilterResult tc))
filterTest fltrs rc GenericTest{..} = Identity . Identity $ filterTestCfg fltrs rc configuration

filterGroups :: forall tc rc effs. TestConfigClass tc =>
              (
                (forall i as vs. (Show i, Show as, Show vs) =>
                      GenericTest tc rc i effs as vs -> Identity (Identity (FilterResult tc))) -> [TestGroup Identity Identity (FilterResult tc) effs]
              )
              -> FilterList rc tc
              -> rc
              -> [[FilterResult tc]]
filterGroups groupLst fltrs rc =
    let
      testFilter :: GenericTest tc rc i effs as vs -> Identity (Identity (FilterResult tc))
      testFilter = filterTest fltrs rc
    in
      (runIdentity . runIdentity <$>) <$> (tests <$> groupLst testFilter)

filterLog :: forall tc. [[FilterResult tc]] -> [FilterResult tc]
filterLog = mconcat

filterGroupFlags :: forall tc. [[FilterResult tc]] -> [Bool]
filterGroupFlags grpFltrRslts = F.any acceptFilter <$> grpFltrRslts
