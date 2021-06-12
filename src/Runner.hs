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
      Listy(..), 
      Traversable (sequenceA), 
      fold, 
      (>>), 
      debug, 
      void )
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
                   -> [Sem effs ()]                   -- [TestIterations]
runTest RunParams {filters, rc, itemIds, itemRunner} be ae test@Test {config = tc, items}  =
     F.acceptFilter (F.applyFilters filters rc tc)
        ? runTestItems itemIds (items rc) be ae rc test itemRunner
        $ []

logLPError ::  forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => FrameworkError e -> Sem effs ()
logLPError = logItem . LP.Error

data RunParams m e rc tc effs a = RunParams {
  suite :: forall hi. Suite e tc rc hi effs a,
  filters :: [F.TestFilter rc tc],
  itemIds :: m (S.Set Int),   
  itemRunner :: forall hi as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON i, ToJSON ds) => ItemRunner e as ds i hi tc rc effs,
  rc :: rc
}

emptyElm :: forall hi a effs. SuiteItem hi effs [a] -> Bool
emptyElm
  = \case
      Tests t -> null t
      Hook _ _ _ s -> all emptyElm s
      Group _ s -> all emptyElm s

{-
exeElm :: forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => 
  Sem effs () -- ^ beforeEach
  -> Sem effs () -- ^ afterEach
  -> SuiteItem effs [[Sem effs () -> Sem effs () -> Sem effs ()]] -- ^ 
  -> Sem effs ()
-}

-- TODO - Error handling especially outside tests eg. in hooks
exeElm :: forall hi ho e effs. (ToJSON e, Show e, Member (Logger e) effs) => 
  Sem effs hi
  -> (hi -> Sem effs ho) -- ^ beforeEach
  -> (ho -> Sem effs ()) -- ^ afterEach
  -> SuiteItem ho effs [[Sem effs ho -> Sem effs () -> Sem effs ()]] -- ^ test list - [beforeEach -> afterEach -> testIteration]
  -> Sem effs ()
exeElm hiSem beforeEach afterEach suiteElm = uu
  emptyElm suiteElm ?
    pure () $
    case suiteElm of
      Tests { tests } -> 
        sequence_ $ fold $ ((\f -> f (beforeEach hi) afterEach) <$>) <$> tests

      Hook {location, hkTitle = title, hook, hElms } -> 
        let 
          hkResult :: Sem effs ho
          hkResult = do 
                        logItem $ StartHook location title
                        hi <- hiSem
                        ho <- hook hi
                        logItem $ EndHook location title
                        pure ho
        in
         case location of 
           BeforeAll -> sequence_ (exeElm hkResult beforeEach afterEach <$> hElms)
           AfterAll  -> sequence_ (exeElm hi beforeEach afterEach <$> hElms) >> void hkResult 
           BeforeEach -> sequence_ $ exeElm hi (hkResult >> beforeEach) afterEach <$> hElms
           AfterEach -> sequence_ $ exeElm beforeEach (afterEach >> hkResult) <$> hElms
          
      Group { grpTitle = title, gElms } -> 
        do
          logItem . StartGroup $ GroupTitle title
          sequence_ $ exeElm hi beforeEach afterEach <$> gElms
          logItem . EndGroup $ GroupTitle title


mkSem :: forall rc tc e effs. (ToJSON e, Show e, RunConfigClass rc, TestConfigClass tc, MinEffs e effs) =>
                    RunParams Maybe e rc tc effs ()
                    -> Sem effs ()
mkSem rp@RunParams {suite, filters, rc} = uu {-
  let
    -- data RunParams m e rc tc effs a = RunParams {
    --   suite :: forall hi. Suite e tc rc hi effs a,
    --   filters :: [F.TestFilter rc tc],
    --   itemIds :: m (S.Set Int),   
    --   itemRunner :: rc -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs (),
    --   rc :: rc
    -- }

--   runTest :: forall i rc hi as ds tc e effs. (ItemClass i ds, TestConfigClass tc, ToJSON e, ToJSON as, ToJSON ds, Show e, Show as, Show ds, Member (Logger e) effs, ToJSON i) =>
--                    RunParams Maybe e rc tc effs ()    -- Run Params
--                    -> Test e tc rc hi i as ds effs    -- Test Case
--                    -> Sem effs hi                     -- before each
--                    -> (hi -> Sem effs ())             -- after each
--                    -> [Sem effs ()]                   -- [TestIterations]
-- runTest RunParams {filters, rc, itemIds, itemRunner} test@Test {config = tc, items} be ae =

    --   suite :: (Test e tc rc hi i as ds effs -> a) -> SuiteItem hi effs [a]
    -- type Suite e tc rc hi effs a = 
    --   (forall i as ds. Test e tc rc hi i as ds effs -> a) -> SuiteItem hi effs [a]

    -- exeElm :: forall hi ho e effs. (ToJSON e, Show e, Member (Logger e) effs) => 
    --   hi
    --   -> (hi -> Sem effs ho) -- ^ beforeEach
    --   -> (ho -> Sem effs ()) -- ^ afterEach
    --   -> SuiteItem ho effs [[Sem effs ho -> Sem effs () -> Sem effs ()]] -- ^ test list - [beforeEach -> afterEach -> test]
    --   -> Sem effs ()

    -- suite :: (Test e tc rc hi i as ds effs -> a) -> SuiteItem hi effs [a]

    {-
      Expected type: Test e tc rc hii i as ds effs -> [Sem effs hii -> Sem effs () -> Sem effs ()]
        Actual type: Sem effs hi1 -> (hi1 -> Sem effs ()) -> Test e tc rc hi1 i0 as0 ds0 effs -> [Sem effs ()]
    -}
  
    root :: forall hii. SuiteItem hii effs [[Sem effs hii -> Sem effs () -> Sem effs ()]] 
    root = (runTest rp) suite  -- Test e tc rc () i0 as0 ds0 effs -> [Sem effs ()]

    -- runtest rp -- Test e tc rc hi2 i0 as0 ds0 effs -> Sem effs hi2 -> (hi2 -> Sem effs ()) -> [Sem effs ()]
    -- suite :: (Test e tc rc hi i as ds effs -> a) -> SuiteItem hi effs [a]

    filterInfo :: [TestFilterResult]
    filterInfo = filterSuite suite filters rc

    run' :: Sem effs ()
    run' = do
            offset' <- utcOffset
            logItem . StartRun (RunTitle $ C.title rc) offset' $ toJSON rc
            logItem . FilterLog $ filterInfo
            exeElm pure pure root
            logItem EndRun
  in
    maybe 
      run'
      (\da -> logLPError . C.Error $ "Test Run Configuration Error. Duplicate Group Names: " <> da)
      -- all the string conversion shannanigans below is due to descrmination which drives firstDuplicate
      -- only working with chars / strings
      (toS <$> firstDuplicate (toS @_ @PRL.String <$> groupAddresses root))
-}

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