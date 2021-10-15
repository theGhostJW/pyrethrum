module TestFilter where

import qualified Data.List as L
import GHC.Records (HasField (getField))
import Internal.RunnerBaseLazy (One)
import OrphanedInstances ()
import Pyrelude as P
import qualified Prelude as PRL
import RunElementClasses (Address (unAddress), Config, TestFilterResult (..), TestLogInfo, mkTestLogInfo, render)
import qualified RunElementClasses as C
import RunnerBase (AddressedElm (..))
import RunnerBase as RB
import qualified Data.Set as S

acceptFilter :: TestFilterResult -> Bool
acceptFilter = isNothing . reasonForRejection

rejectFilter :: TestFilterResult -> Bool
rejectFilter = isJust . reasonForRejection

acceptAnyFilter :: [TestFilterResult] -> Bool
acceptAnyFilter = P.any acceptFilter

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
filterLog suite fltrs rc =
  let testFilter :: rc -> Address -> Test e tc rc ho i as ds effs -> TestFilterResult
      testFilter rc' adr = filterTest fltrs rc' adr

      title' :: TestFilterResult -> Text
      title' = getField @"title" . C.testInfo

      filterResults :: [AddressedElm TestFilterResult]
      filterResults = querySuite' rc title' testFilter suite
   in element <$> filterResults

data FilterLog = FilterLog {
  log :: [TestFilterResult],
  included ::  S.Set Address
}

activeAddresses :: [TestFilterResult] -> S.Set Address
activeAddresses r =
  let includedAddresses :: [Address]
      includedAddresses = (C.address :: TestLogInfo -> Address). C.testInfo <$> filter (isNothing . reasonForRejection) r

      subSet :: Address -> S.Set Address
      subSet add = S.fromList $ C.Address <$> (reverse . P.dropWhile null . subsequences . reverse $ unAddress add)
   in foldl' S.union S.empty $ subSet <$> includedAddresses


filterSuite :: Config tc => rc -> (forall a. TestSuite e tc rc effs a) -> [TestFilter rc tc] -> Either Text FilterLog
filterSuite rc suite filters =
  let log ::  [TestFilterResult]
      log = filterLog suite filters rc

      includedAddresses :: S.Set Address
      includedAddresses = activeAddresses log

      dupeAddress :: Maybe Text
      dupeAddress = toS <$> firstDuplicate (toS @_ @PRL.String . render . (C.address :: TestLogInfo -> Address) . C.testInfo <$> log)
  in 
    maybe (Right $ FilterLog log includedAddresses) Left dupeAddress

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
