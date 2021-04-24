{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

module Runner (
  mkEndpointSem
  , RunParams(..)
  , mkSem
  , module RB
  , module ItemFilter
  , module C
) where

import Common as C
    ( dList,
      indentText,
      DetailedInfo(..),
      FileSystemErrorType(..),
      HookLocation(..),
      FilterErrorType(..),
      FrameworkError(..),
      OutputDListText )
import DSL.Interpreter ( MinEffs )
import DSL.Logger ( logItem, Logger )
import DSL.LogProtocol as LP (
      GroupTitle(GroupTitle),
      ItemId(ItemId),
      LogProtocolBase(..),
      RunTitle(RunTitle),
      ThenClause(ThenClause),
      WhenClause(WhenClause) )
import DSL.CurrentTime ( utcOffset )
import Data.Either.Extra ( Either, eitherToMaybe )
import Pyrelude as P
    ( zip,
      fst,
      snd,
      ($),
      Eq((==)),
      Show,
      Applicative(pure, (*>)),
      Semigroup((<>)),
      Bool(True),
      Int,
      Maybe(..),
      Either(..),
      Text,
      Category((.)),
      (<$>),
      sequence_,
      maybe,
      catMaybes,
      unless,
      eitherf,
      firstDuplicate,
      txt,
      uu,
      toS,
      (?),
      Listy(..), Traversable (sequenceA), fold, (>>), debug )
import Polysemy ( Sem, Member )
import Polysemy.Error as PE ( Error, catch, throw )
import ItemFilter  (ItemFilter (..), filterredItemIds)
import qualified Data.Set as S
import RunElementClasses as C
    ( mkDisplayInfo,
      mkTestAddress,
      toString,
      ItemClass(..),
      RunConfigClass,
      TestAddress(..),
      TestConfigClass(..),
      TestDisplayInfo(..),
      TestFilterResult(..),
      Titled(..) )
import OrphanedInstances()
import Data.Aeson as A ( ToJSON(toJSON) )
import TestFilter
    ( acceptFilter,
      filterSuite,
      applyFilters,
      TestFilter(..), acceptAnyFilter )
import RunnerBase as RB
    ( doNothing,
      groupAddresses,
      groupName,
      GenericResult(..),
      ItemRunner,
      PreRun(..),
      SuiteItem(..),
      Test(..),
      Suite )
import qualified Prelude

runTestItems :: forall i as ds tc rc e effs. (ToJSON e, Show e, TestConfigClass tc, ToJSON i, ItemClass i ds, Member (Logger e) effs) =>
      Maybe (S.Set Int)                                                    -- target Ids
      -> [i]   
      -> rc                                                            -- items
      -> Test e tc rc i as ds effs
      -> ItemRunner e as ds i tc rc effs
      -> [Sem effs ()]
runTestItems iIds items rc test@Test{ config = tc } itemRunner =
  let
    startTest :: Sem effs ()
    startTest = logItem . StartTest $ mkDisplayInfo tc

    endTest :: Sem effs ()
    endTest = logItem . EndTest $ moduleAddress tc

    filteredItems :: [i]
    filteredItems = filter inTargIds items

    applyRunner :: i -> Sem effs ()
    applyRunner i =  
      let
        iid :: ItemId
        iid = ItemId (moduleAddress tc) (identifier @_ @ds i)
      in
        do
          logItem . StartIteration iid (WhenClause $ whenClause @_ @ds i) (ThenClause $ thenClause @_ @ds  i) $ toJSON i
          itemRunner rc test i
          logItem $ EndIteration iid

    inTargIds :: i -> Bool
    inTargIds i = maybe True (S.member (identifier @_ @ds i)) iIds

  in
    case filteredItems of
      [] -> []
      [x] -> [startTest *> applyRunner x *> endTest]
      x : xs -> (startTest *> applyRunner x)
                : (applyRunner <$> Prelude.init xs)
                <> [applyRunner (Prelude.last xs) *> endTest]

runTest ::  forall i rc as ds tc e effs. (ItemClass i ds, TestConfigClass tc, ToJSON e, ToJSON as, ToJSON ds, Show e, Show as, Show ds, Member (Logger e) effs, ToJSON i) =>
                   RunParams Maybe e rc tc effs          -- Run Params
                   -> Test e tc rc i as ds effs          -- Test Case
                   -> [Sem effs ()]                      -- [TestIterations]
runTest RunParams {filters, rc, itemIds, itemRunner}  test@Test {config = tc, items} =
    acceptFilter (applyFilters filters rc tc)
      ? runTestItems itemIds (items rc) rc test itemRunner
      $ []

logLPError ::  forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => FrameworkError e -> Sem effs ()
logLPError = logItem . LP.Error

data RunParams m e rc tc effs = RunParams {
  suite :: forall a. Suite e tc rc effs a,
  filters :: [TestFilter rc tc],
  itemIds :: m (S.Set Int),   
  itemRunner :: forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON i, ToJSON ds) => ItemRunner e as ds i tc rc effs,
  rc :: rc
}

emptyElm :: forall a effs. SuiteItem effs [a] -> Bool
emptyElm
  = \case
      Tests t -> null t
      Hook _ _ _ s -> all emptyElm s
      Group _ s -> all emptyElm s

-- TODO - Error handling especially outside tests eg. in hooks
exeElm :: forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => 
  Sem effs () -- ^ beforeEach
  -> Sem effs () -- ^ afterEach
  -> SuiteItem effs [[Sem effs ()]] -- ^ 
  -> Sem effs ()
exeElm beforeEach afterEach suiteElm = 
  emptyElm suiteElm ?
    pure () $
    case suiteElm of
      Tests { tests } -> sequence_ $ fold $ ((\i -> beforeEach >> i >> afterEach) <$>) <$> tests

      Hook {location, title = ttl, hook, subElms } -> 
        let 
          loggedHook = do 
                        logItem $ StartHook location ttl
                        hook
                        logItem $ EndHook location ttl
        in
        case location of 
          BeforeAll -> loggedHook >> sequence_ (exeElm beforeEach afterEach <$> subElms)
          AfterAll  -> sequence_ (exeElm beforeEach afterEach <$> subElms) >> loggedHook

          BeforeEach -> sequence_ $ exeElm (loggedHook >> beforeEach) afterEach <$> subElms
          AfterEach -> sequence_ $ exeElm beforeEach (afterEach >> loggedHook) <$> subElms
          
      Group { title = t, subElms } -> 
        do
          logItem . StartGroup $ GroupTitle t
          sequence_ $ exeElm beforeEach afterEach <$> subElms
          logItem . EndGroup $ GroupTitle t


mkSem :: forall rc tc e effs. (ToJSON e, Show e, RunConfigClass rc, TestConfigClass tc, MinEffs e effs) =>
                    RunParams Maybe e rc tc effs
                    -> Sem effs ()
mkSem rp@RunParams {suite, filters, rc} =
  let
    root :: SuiteItem effs [[Sem effs ()]]
    root = suite $ runTest rp

    filterInfo :: [TestFilterResult]
    filterInfo = filterSuite suite filters rc

    run' :: Sem effs ()
    run' = do
            offset' <- utcOffset
            logItem . StartRun (RunTitle $ C.title rc) offset' $ toJSON rc
            logItem . FilterLog $ filterInfo
            exeElm (pure ()) (pure ()) root
            logItem EndRun
  in
    maybe 
      run'
      (\da -> logLPError . C.Error $ "Test Run Configuration Error. Duplicate Group Names: " <> da)
      -- all the string conversion shannanigans below is due to descrmination which drives firstDuplicate
      -- only working with chars / strings
      (toS <$> firstDuplicate (toS @_ @Prelude.String <$> groupAddresses root))


mkEndpointSem :: forall rc tc e effs. (RunConfigClass rc, TestConfigClass tc, ToJSON e, Show e, MinEffs e effs) =>
                   RunParams (Either FilterErrorType) e rc tc effs
                   -> TestAddress                            -- test address
                   -> Either FilterErrorType (S.Set Int)    -- a set of item Ids used for test case endpoints                                               -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> Sem effs ()
mkEndpointSem runParams@RunParams { filters, itemIds } tstAddress iIds =
  let
    endpointFilter :: TestAddress -> TestFilter rc tc
    endpointFilter targAddress = TestFilter {
      title = "test address does not match endpoint target: " <> toString targAddress,
      predicate = \_ tc -> moduleAddress tc == targAddress
    }

    allFilters :: [TestFilter rc tc]
    allFilters = endpointFilter tstAddress : filters
  in
    eitherf itemIds
      (logItem . LP.Error . FilterError)
      (\idSet -> mkSem runParams { filters = allFilters, itemIds = eitherToMaybe itemIds })