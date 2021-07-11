-- TODO: work out why this is needed - investigate polykinds
{-# LANGUAGE NoPolyKinds #-} 
-- {-# LANGUAGE NoStrictData #-} 

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
      HookCardinality(..),
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
      Bool(True, False),
      Int,
      Maybe(..),
      Either(..),
      Text,
      Category((.)),
      (&),
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
      Listy(..),
      Traversable (sequenceA),
      fold,
      (>>),
      debug,
      void, not, const, error, join )
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
import qualified TestFilter as F
    ( acceptFilter,
      filterLog,
      applyFilters,
      TestFilter(..), acceptAnyFilter )
import RunnerBase as RB
    ( groupAddresses,
      groupName,
      GenericResult(..),
      ItemRunner,
      SuiteItem(..),
      Test(..),
      TestSuite )
import qualified Prelude as PRL

runTestItems :: forall i as ds hi tc rc e effs. (ToJSON e, Show e, TestConfigClass tc, ToJSON i, ItemClass i ds, Member (Logger e) effs) =>
      Maybe (S.Set Int)     -- target Ids
      -> [i]                -- ids
      -> Sem effs hi        --before each
      -> (hi -> Sem effs ()) -- after each
      -> rc                 -- runcoonfig
      -> Test e tc rc hi i as ds effs
      -> ItemRunner e as ds i hi tc rc effs
      -> [Sem effs ()]
runTestItems iIds items beforEach afterEach rc test@Test{ config = tc } itemRunner =
  let
    startTest :: Sem effs ()
    startTest = logItem . StartTest $ mkDisplayInfo tc

    endTest :: Sem effs ()
    endTest = logItem . EndTest $ moduleAddress tc

    filteredItems :: [i]
    filteredItems = filter inTargIds items

    runItem' :: hi -> i -> Sem effs ()
    runItem' i = uu --

    applyRunner :: i -> Sem effs ()
    applyRunner i =  
      let
        iid :: ItemId
        iid = ItemId (moduleAddress tc) (identifier @_ @ds i)
      in
        do
          logItem . StartIteration iid (WhenClause $ whenClause @_ @ds i) (ThenClause $ thenClause @_ @ds  i) $ toJSON i
          hi <- beforEach
          itemRunner rc hi test i
          afterEach hi
          logItem $ EndIteration iid

    inTargIds :: i -> Bool
    inTargIds i = maybe True (S.member (identifier @_ @ds i)) iIds
  in
    case filteredItems of
      [] -> []
      xs -> [startTest >> sequence_ (applyRunner <$> xs) >> endTest]

runTest :: forall i rc hi as ds tc e effs. (ItemClass i ds, TestConfigClass tc, ToJSON e, ToJSON as, ToJSON ds, Show e, Show as, Show ds, Member (Logger e) effs, ToJSON i) =>
                   RunParams Maybe e rc tc effs ()    -- Run Params
                   -> Sem effs hi                     -- before each
                   -> (hi -> Sem effs ())             -- after each
                   -> Test e tc rc hi i as ds effs    -- Test Case
                   -> [Sem effs ()]                   -- [Test Iterations]
runTest RunParams {filters, rc, itemIds, itemRunner} be ae test@Test {config = tc, items}  =
     F.acceptFilter (F.applyFilters filters rc tc)
        ? runTestItems itemIds (items rc) be ae rc test itemRunner
        $ []

logLPError ::  forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => FrameworkError e -> Sem effs ()
logLPError = logItem . LP.Error

data RunParams m e rc tc effs a = RunParams {
  suite :: TestSuite e tc rc effs a,
  filters :: [F.TestFilter rc tc],
  itemIds :: m (S.Set Int),   
  itemRunner :: forall hi as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON i, ToJSON ds) => ItemRunner e as ds i hi tc rc effs,
  rc :: rc
}


-- TODO - Error handling especially outside tests eg. in hooks
exeElm :: forall hi e effs a. (ToJSON e, Show e, Member (Logger e) effs) => 
  (forall hii. hii -> a -> Sem effs ()) 
  -> hi
  -> SuiteItem hi effs [a] 
  -> Sem effs ()
exeElm runner hi si =

  do
    mt <- emptyElm si
    mt ? 
      pure () $
      case si of
        Tests { tests } -> sequence_ $ runner hi <$> tests

        BeforeHook {cardinality, title = ttl, bHook , bhElms } -> 
            let 
              logRun = do 
                logItem $ StartHook cardinality ttl
                o <- bHook
                logItem $ EndHook cardinality ttl
                pure o
            in
            case cardinality of
              ExeOnce ->
                  do 
                    o <- logRun
                    sequence_ $ (\f -> exeElm runner o $ f o) <$> bhElms

              ExeForEach -> let 
                              runElm f = do 
                                          o <- logRun
                                          exeElm runner o $ f o
                            in
                              sequence_ $ runElm <$> bhElms

        AfterHook {cardinality , title = ttl, aHook , ahElms } ->
          let 
            logRun = do 
                      logItem $ StartHook cardinality ttl
                      o <- aHook
                      logItem $ EndHook cardinality ttl
          in
          case cardinality of 
            ExeOnce -> do 
                        sequence_ $ (\f -> exeElm runner hi $ f hi) <$> ahElms
                        logRun

            ExeForEach -> sequence_ $ (\f -> exeElm runner hi (f hi) >> logRun) <$> ahElms
            
        Group { title = ttl, gElms } -> 
          do 
            logItem . StartGroup . GroupTitle $ ttl
            sequence_ $ exeElm runner hi <$> gElms
            logItem . EndGroup . GroupTitle $ ttl

emptyElm :: forall hi a effs. SuiteItem hi effs [a] -> Sem effs Bool
emptyElm si = uu
  -- case si of
  --    Tests { tests } -> pure . null $ filter pred tests
  --    BeforeHook { bhElms } -> uu -- [hi -> SuiteItem hi effs t]

mkSem :: forall rc tc e effs. (ToJSON e, Show e, RunConfigClass rc, TestConfigClass tc, MinEffs e effs) =>
                    RunParams Maybe e rc tc effs ()
                    -> Sem effs ()
mkSem rp@RunParams {suite, filters, rc} = uu
  -- let
  
  --   -- forall i as ds. Test e tc rc hi i as ds effs -> a) -> SuiteItem hi effs [a]
  --   root :: forall hii. SuiteItem hii effs [[Sem effs hii -> Sem effs () -> Sem effs ()]] 
  --   root = (runTest rp) suite  -- Test e tc rc () i0 as0 ds0 effs -> [Sem effs ()]


  --   filterInfo :: [TestFilterResult]
  --   filterInfo = filterSuite suite filters rc

  --   run' :: Sem effs ()
  --   run' = do
  --           offset' <- utcOffset
  --           logItem . StartRun (RunTitle $ C.title rc) offset' $ toJSON rc
  --           logItem . FilterLog $ filterInfo
  --           exeElm pure pure root
  --           logItem EndRun
  -- in
  --   maybe 
  --     run'
  --     (\da -> logLPError . C.Error $ "Test Run Configuration Error. Duplicate Group Names: " <> da)
  --     -- all the string conversion shannanigans below is due to descrmination which drives firstDuplicate
  --     -- only working with chars / strings
  --     (toS <$> firstDuplicate (toS @_ @PRL.String <$> groupAddresses root))


mkEndpointSem :: forall rc tc e effs. (RunConfigClass rc, TestConfigClass tc, ToJSON e, Show e, MinEffs e effs) =>
                   RunParams (Either FilterErrorType) e rc tc effs ()
                   -> TestAddress                            -- test address
                   -> Either FilterErrorType (S.Set Int)    -- a set of item Ids used for test case endpoints                                               -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> Sem effs ()
mkEndpointSem runParams@RunParams { filters, itemIds } tstAddress iIds =
  let
    endpointFilter :: TestAddress -> F.TestFilter rc tc
    endpointFilter targAddress = F.TestFilter {
      title = "test address does not match endpoint target: " <> toString targAddress,
      predicate = \_ tc -> moduleAddress tc == targAddress
    }

    allFilters :: [F.TestFilter rc tc]
    allFilters = endpointFilter tstAddress : filters
  in
    eitherf itemIds
      (logItem . LP.Error . FilterError)
      (\idSet -> mkSem runParams { filters = allFilters, itemIds = eitherToMaybe itemIds })