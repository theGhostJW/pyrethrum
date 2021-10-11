module TestFilter where

import qualified Data.List as L
import Internal.RunnerBaseLazy (One)
import GHC.Records (HasField (getField))
import OrphanedInstances ()
import Pyrelude as P
import RunElementClasses (Address, Config, TestFilterResult (..), TestLogInfo, mkTestLogInfo)
import qualified RunElementClasses as C
import RunnerBase (AddressedElm (..))
import RunnerBase as RB
import Polysemy

acceptFilter :: TestFilterResult -> Bool
acceptFilter = isNothing . reasonForRejection

rejectFilter :: TestFilterResult -> Bool
rejectFilter = isJust . reasonForRejection

mkTestFilterResult :: Config tc => Address -> tc -> Maybe Text -> TestFilterResult
mkTestFilterResult d tc rejection =
  TestFilterResult
    { testInfo = mkTestLogInfo d tc,
      reasonForRejection = rejection
    }

data TestFilter rc tc = TestFilter
  { title :: rc -> Address -> tc -> Text,
    predicate :: rc -> Address -> tc -> Bool
  }

applyFilters :: forall rc tc. Config tc => [TestFilter rc tc] -> rc -> Address -> tc -> TestFilterResult
applyFilters fltrs rc adrs tc =
  let fltrRslt :: Maybe Text -> TestFilterResult
      fltrRslt = mkTestFilterResult adrs tc

      applyFilter :: TestFilter rc tc -> TestFilterResult
      applyFilter fltr =
        fltrRslt $
          predicate fltr rc adrs tc
            ? Nothing
            $ Just $ TestFilter.title fltr rc adrs tc

      firstRejectReason :: Maybe Text
      firstRejectReason = L.find rejectFilter (applyFilter <$> fltrs) >>= reasonForRejection
   in fltrRslt firstRejectReason

filterTest :: forall i as ds tc hi rc e effs. Config tc => [TestFilter rc tc] -> rc -> Address -> Test e tc rc hi i as ds effs -> TestFilterResult
filterTest fltrs rc d Test {config = tc} = applyFilters fltrs rc d tc

filterLog ::
  forall tc rc e effs.
  Config tc =>
  TestSuite e tc rc effs TestFilterResult ->
  [TestFilter rc tc] ->
  rc ->
  [TestFilterResult]
filterLog suite fltrs rc = uu
  let testFilter :: rc -> Address -> Test e tc rc ho i as ds effs -> TestFilterResult
      testFilter rc' adr = filterTest fltrs rc' adr

      title' :: TestFilterResult -> Text
      title' = getField @"title" . testInfo 

      filterResults :: [AddressedElm TestFilterResult]
      filterResults = querySuite' rc title' testFilter suite

   in element <$> filterResults

{-
  rc ->
  (a -> Text) ->  -- get title
  ( forall ho i as ds. -- data extractor
    (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, RC.Config tc, RC.ItemClass i ds) =>
    rc ->
    Address ->
    Test e tc rc ho i as ds effs ->
    a
  ) 
  -> TestSuite e tc rc effs a -- suiite
  -> [AddressedElm a]
-}

acceptAnyFilter :: [TestFilterResult] -> Bool
acceptAnyFilter = P.any acceptFilter