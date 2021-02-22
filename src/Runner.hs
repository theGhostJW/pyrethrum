{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Runner (
  mkEndpointSem
  , RunParams(..)
  , mkRunSem
  , module RB
  , module ItemFilter
  , module C
) where

import Common as C
    ( dList,
      indentText,
      DetailedInfo(..),
      FileSystemErrorType(..),
      FilterErrorType(..),
      FrameworkError(..),
      OutputDListText,
      PreTestStage(..) )
import DSL.Interpreter ( ApEffs )
import DSL.Logger ( logItem, Logger )
import DSL.LogProtocol as LP
import DSL.CurrentTime ( utcOffset )
import Data.Either.Extra
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
      Listy(filter), Traversable (sequenceA), fold )
import Polysemy
import Polysemy.Error as PE ( Error, catch, throw )
import ItemFilter  (ItemFilter (..), filterredItemIds)
import qualified Data.Set as S
import RunElementClasses as C
import OrphanedInstances()
import Data.Aeson as A
import TestFilter
    ( acceptFilter,
      filterRunElements,
      applyFilters,
      TestFilter(..), acceptAnyFilter )
import RunnerBase as RB
    ( doNothing,
      groupName,
      Ensurable,
      GenericResult(..),
      HookLocation(..),
      ItemRunner,
      PreRun(..),
      RunElement(..),
      Test(..),
      TestPlanBase )
import qualified Prelude

logBoundry :: forall e effs. (Show e, A.ToJSON e, Member (Logger e) effs) => BoundaryEvent -> Sem effs ()
logBoundry = logItem . BoundaryLog

runTestItems :: forall i as ds tc rc e effs. (ToJSON e, Show e, TestConfigClass tc, ItemClass i ds, Member (Logger e) effs) =>
      Maybe (S.Set Int)                                                    -- target Ids
      -> [i]   
      -> rc                                                            -- items
      -> Test e tc rc i as ds effs
      -> ItemRunner e as ds i tc rc effs
      -> [Sem effs ()]
runTestItems iIds items rc test@Test{ config = tc } itemRunner =
  let
    startTest :: Sem effs ()
    startTest = logBoundry . StartTest $ mkDisplayInfo tc

    endTest :: Sem effs ()
    endTest = logBoundry . EndTest $ moduleAddress tc

    filteredItems :: [i]
    filteredItems = filter inTargIds items

    applyRunner :: i -> Sem effs ()
    applyRunner i =  
      let
        iid :: ItemId
        iid = ItemId (moduleAddress tc) (identifier i)
      in
        do
          logBoundry . StartIteration iid (WhenClause $ whenClause i) (ThenClause $ thenClause i) $ toJSON i
          itemRunner rc test i
          logBoundry $ EndIteration iid

    inTargIds :: i -> Bool
    inTargIds i = maybe True (S.member (identifier i)) iIds

  in
    case filteredItems of
      [] -> []
      [x] -> [startTest *> applyRunner x *> endTest]
      x : xs -> (startTest *> applyRunner x)
                : (applyRunner <$> Prelude.init xs)
                <> [applyRunner (Prelude.last xs) *> endTest]

runTest ::  forall i rc as ds tc e effs. (ItemClass i ds, TestConfigClass tc, ToJSON e, ToJSON as, ToJSON ds, Show e, Show as, Show ds, Member (Logger e) effs) =>
                   RunParams Maybe e rc tc effs          -- Run Params
                   -> Test e tc rc i as ds effs          -- Test Case
                   -> [Sem effs ()]                      -- [TestIterations]
runTest RunParams {filters, rc, itemIds, itemRunner}  test@Test {config = tc, items} =
    acceptFilter (applyFilters filters rc tc)
      ? runTestItems itemIds (items rc) rc test itemRunner
      $ []

logLPError ::  forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => FrameworkError e -> Sem effs ()
logLPError = logItem . logRun . LP.Error

data RunParams m e rc tc effs = RunParams {
  plan :: forall mo mi a. TestPlanBase e tc rc mo mi a effs,
  filters :: [TestFilter rc tc],
  itemIds :: m (S.Set Int),   
  itemRunner :: forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => ItemRunner e as ds i tc rc effs,
  rc :: rc
}

data Hooks effs = Hooks {
  beforeAll :: Sem effs (),
  beforeEach :: Sem effs (),
  afterEach :: Sem effs (),
  afterAll :: Sem effs ()
}



-- TODO - Error handling especially outside tests 
exeElm :: 
  Sem effs () ->
  Sem effs () ->
  RunElement [] (Sem effs) () effs -> 
  Sem effs ()
exeElm beforeEach afterEach runElm = uu
  -- case runElm of
  --   Tests tests' -> sequence_ $ \t -> beforeEach >> t >> afterEach <$> fold tests'

  --   Hook location' hook' subElms' -> 
  --     case location' of 
  --       BeforeAll -> hook' >> exeElm beforeEach afterEach subElms'
  --       AfterAll  -> exeElm beforeEach afterEach subElms' >> hook'

  --       BeforeEach -> exeElm (hook' >> beforeEach) afterEach subElms'
  --       AfterEach -> exeElm beforeEach (afterEach >> hook') subElms'
        
  --   Group title' subElms' -> 
  --       do
  --         logBoundry . StartGroup $ GroupTitle title'
  --         exeElm beforeEach afterEach subElms'
  --         logBoundry $ EndGroup title'


mkSem :: forall rc tc e effs. (ToJSON e, Show e, RunConfigClass rc, TestConfigClass tc, ApEffs e effs) =>
                    RunParams Maybe e rc tc effs
                    -> Sem effs ()
mkSem rp@RunParams {plan, filters, rc} = uu
  let
    allElms :: RunElement [] (Sem effs) () effs
    allElms = plan $ runTest rp

    filterInfo :: [TestFilterResult]
    filterInfo = filterRunElements plan filters rc
  in
    maybe
    -- (
    --   do
    --     offset' <- utcOffset
    --     logBoundry . StartRun (RunTitle $ C.title rc) offset' $ toJSON rc
    --     logBoundry . FilterLog $ filterInfo
    --     sequence_ $ exeElm (pure ()) (pure ()) <$> filter acceptAnyFilter allElms
    --     logBoundry EndRun
    -- )
    uu
    uu --(\dupeTxt -> logLPError . C.Error $ "Test Run Configuration Error. Duplicate Group Names: " <> dupeTxt)
    uu --toS <$> firstDuplicate (toS . C.title <$> groupNames :: [Prelude.String])

mkRunSem :: forall rc tc e effs. (RunConfigClass rc, TestConfigClass tc, ToJSON e, Show e, ApEffs e effs) => RunParams Maybe e rc tc effs -> Sem effs ()
mkRunSem = mkSem 

mkEndpointSem :: forall rc tc e effs. (RunConfigClass rc, TestConfigClass tc, ToJSON e, Show e, ApEffs e effs) =>
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
      (logItem . logRun . LP.Error . FilterError)
      (\idSet -> mkSem runParams { filters = allFilters, itemIds = eitherToMaybe itemIds })