-- TODO: work out why this is needed - investigate polykinds
{-# LANGUAGE NoPolyKinds #-}

-- {-# LANGUAGE NoStrictData #-}

module Runner
  ( mkEndpointSem,
    RunParams (..),
    mkSem,
    module RB,
    module ItemFilter,
    module C,
  )
where

import Common
  ( DetailedInfo (..),
    FileSystemErrorType (..),
    FilterErrorType (..),
    FrameworkError (..),
    HookType,
    OutputDListText,
    dList,
    indentText,
  )
import qualified Common as C
  ( HookType (..),
  )
import DSL.CurrentTime (utcOffset)
import DSL.Interpreter (MinEffs)
import DSL.LogProtocol as LP
  ( GroupTitle (GroupTitle),
    ItemId (ItemId),
    LogProtocolBase (..),
    RunTitle (RunTitle),
    ThenClause (ThenClause),
    WhenClause (WhenClause),
  )
import DSL.Logger (Logger, logItem)
import Data.Aeson as A (ToJSON (toJSON), Value (Bool))
import Data.Either.Extra (Either, eitherToMaybe)
import Data.List (dropWhile)
import qualified Data.Set as S
import GHC.Records (HasField (getField))
import ItemFilter (ItemFilter (..), filterredItemIds)
import OrphanedInstances ()
import Polysemy (Member, Sem)
import Polysemy.Error as PE (Error, catch, throw)
import Pyrelude as P
  ( Applicative (pure, (*>)),
    Bool (False, True),
    Category ((.)),
    Either (..),
    Eq ((==)),
    Int,
    Listy (..),
    Maybe (..),
    Semigroup ((<>)),
    Show,
    Text,
    Traversable (sequenceA),
    catMaybes,
    const,
    debug,
    eitherf,
    error,
    firstDuplicate,
    fold,
    fst,
    id,
    isNothing,
    join,
    maybe,
    not,
    sequence_,
    snd,
    subsequences,
    toS,
    txt,
    unless,
    uu,
    void,
    zip,
    ($),
    (&),
    (<$>),
    (>>),
    (>>=),
    (?),
  )
import RunElementClasses as C
  ( Address,
    AddressElem,
    AddressedElm (AddressedElm, element),
    Config,
    HasId,
    HasTitle,
    ItemClass,
    TestFilterResult (..),
    TestLogInfo (..),
    mkTestLogInfo,
    push,
    render,
    rootAddress,
    unAddress,
  )
import qualified RunElementClasses as RC
import RunnerBase as RB
  ( GenericResult (..),
    ItemRunner,
    SuiteItem (..),
    Test (..),
    TestSuite,
    One,
    Many,
    queryElm
  )
import qualified TestFilter as F
  ( TestFilter (..),
    acceptAnyFilter,
    acceptFilter,
    applyFilters,
    filterLog,
  )
import qualified Prelude as PRL
import qualified Check

getId :: HasField "id" i Int => i -> Int
getId = getField @"id"

runTestItems ::
  forall i as ds hi ho tc rc e effs.
  (ToJSON e, Show e, Config tc, ToJSON i, ItemClass i ds, Member (Logger e) effs) =>
  Maybe (S.Set Int) -> -- target Ids
  [i] -> -- ids
  rc -> -- runcoonfig
  Address ->
  hi ->
  (hi -> Sem effs ho) -> -- beforeEach
  (ho -> Sem effs ()) -> -- AfterEach
  ItemRunner e as ds i ho tc rc effs -> --rc -> Address -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs ()
  Test e tc rc ho i as ds effs ->
  [Sem effs ()]
runTestItems iIds items rc add hi beforeEach afterEach itemRunner test@Test {config = tc}  =
  let startTest :: Sem effs ()
      startTest = logItem . StartTest $ mkTestLogInfo add tc

      endTest :: Sem effs ()
      endTest = logItem $ EndTest add

      filteredItems :: [i]
      filteredItems = filter inTargIds items

      applyRunner :: i -> Sem effs ()
      applyRunner i =
        let iid :: ItemId
            iid = ItemId add (getId i)
         in do
              logItem . StartIteration iid (getField @"title" i) $ toJSON i
              ho <- beforeEach hi
              itemRunner rc add ho test i
              afterEach ho
              logItem $ EndIteration iid

      inTargIds :: i -> Bool
      inTargIds i = maybe True (S.member $ getId i) iIds
   in case filteredItems of
        [] -> []
        xs -> [startTest >> sequence_ (applyRunner <$> xs) >> endTest]

runTest ::
  forall i rc hi ho as ds tc e effs.
  (ItemClass i ds, Config tc, ToJSON e, ToJSON as, ToJSON ds, Show e, Show as, Show ds, Member (Logger e) effs, ToJSON i) =>
  RunParams Maybe e rc tc effs -> -- Run Params
  Address ->
  hi -> 
  (hi -> Sem effs ho) -> -- beforeEach
  (ho -> Sem effs ()) -> -- AfterEach
  Test e tc rc ho i as ds effs -> -- Test Case
  [Sem effs ()] -- [Test Iterations]
runTest RunParams {filters, rc, itemIds, itemRunner} add hi beforeEach afterEach test@Test {config = tc, items} =
  F.acceptFilter (F.applyFilters filters rc add tc)
    ? runTestItems itemIds (items rc) rc add hi beforeEach afterEach itemRunner test
    $ []

logLPError :: forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => FrameworkError e -> Sem effs ()
logLPError = logItem . LP.Error



data RunParams m e rc tc effs = RunParams
  { suite :: forall a. TestSuite e tc rc effs a,
    filters :: [F.TestFilter rc tc],
    itemIds :: m (S.Set Int),
    itemRunner :: forall hi as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON i, ToJSON ds) => ItemRunner e as ds i hi tc rc effs,
    rc :: rc
  }

-- TODO - Error handling especially outside tests eg. in hooks
exeElm ::
  forall c hi ho e effs a.
  (ToJSON e, Show e, Member (Logger e) effs) =>
  S.Set Address ->
  (forall hii. Address -> hii -> a -> Sem effs ()) ->
  Address ->
  hi ->
  SuiteItem c hi ho effs [a] ->
  Sem effs ()
exeElm targAddresses runner address hi si = uu
  -- let log' :: LogProtocolBase e -> Sem effs ()
  --     log' = logItem

  --     exeHook :: HookType -> Text -> Sem effs o -> Sem effs o
  --     exeHook hookType ttl hook = do
  --       log' $ StartHook hookType ttl
  --       o <- hook
  --       log' $ EndHook hookType ttl
  --       pure o

  --     exeNxt :: forall c' hin hout. Address -> hin -> SuiteItem c' hin hout effs [a] -> Sem effs ()
  --     exeNxt = exeElm targAddresses runner 

  --  in do
  --       S.notMember address targAddresses
  --         ? pure ()
  --         $ case si of
  --           Root subElms -> sequence_ $ exeNxt address () <$> subElms
  --           Tests {tests} -> sequence_ $ runner address hi <$> tests
  --           BeforeAll {title = ttl, bHook, bhElms} -> do
  --             o <- exeHook C.BeforeAll ttl (bHook hi)
  --             sequence_ $ (\f -> exeNxt address o $ f address o) <$> bhElms
  --           BeforeEach {title' = ttl, bHook', bhElms'} ->
  --             let runElm f = do
  --                   o <- exeHook C.BeforeEach ttl (bHook' hi)
  --                   exeNxt address o $ f address o
  --              in sequence_ $ runElm <$> bhElms'
  --           AfterEach {RB.title' = ttl, aHook', ahElms'} ->
  --             sequence_ $ (\f -> exeNxt address hi (f address hi) >> exeHook C.AfterEach ttl (aHook' hi)) <$> ahElms'
  --           AfterAll {title = ttl, aHook, ahElms} -> do
  --             sequence_ $ (\f -> exeNxt address hi (f address hi)) <$> ahElms
  --             exeHook C.AfterAll ttl $ aHook hi
  --           Group {title = ttl, gElms} ->
  --             do
  --               logItem . StartGroup . GroupTitle $ ttl
  --               sequence_ $ (\f -> exeNxt address hi $ f address hi) <$> gElms
  --               logItem . EndGroup . GroupTitle $ ttl

-- exeElm' ::
--   forall c hi ho e effs a.
--   (ToJSON e, Show e, Member (Logger e) effs) =>
--   S.Set Address ->
--   Address ->
--   hi ->
--   SuiteItem c hi ho effs [[Sem effs ()]] ->
--   Sem effs ()
-- exeElm' targAddresses address hi si = 
--    let log' :: LogProtocolBase e -> Sem effs ()
--        log' = logItem

--        exeHook :: HookType -> Text -> Sem effs o -> Sem effs o
--        exeHook hookType ttl hook = do
--         log' $ StartHook hookType ttl
--         o <- hook
--         log' $ EndHook hookType ttl
--         pure o
--    in
--         S.notMember address targAddresses
--           ? pure ()
--           $ case si of
--               Tests {tests} -> sequence_ $ join tests --sequence_ $ runner address hi <$> tests
--               _ ->  pure ()

activeAddresses :: [TestFilterResult] -> S.Set Address
activeAddresses r =
  let includedAddresses :: [Address]
      includedAddresses = address . testInfo <$> filter (isNothing . reasonForRejection) r

      subSet :: Address -> S.Set Address
      subSet add = S.fromList $ RC.Address <$> (reverse . P.dropWhile null . subsequences . reverse $ unAddress add)
   in foldl' S.union S.empty $ subSet <$> includedAddresses


mkSem ::
  forall rc tc e effs.
  (ToJSON e, Show e, Config rc, Config tc, MinEffs e effs) =>
  RunParams Maybe e rc tc effs ->
  Sem effs ()
mkSem rp@RunParams {suite, filters, rc, itemRunner} =
  let filterInfo :: [TestFilterResult]
      filterInfo = F.filterLog suite filters rc

      includedAddresses :: S.Set Address
      includedAddresses = activeAddresses filterInfo

      dupeAddress :: Maybe Text
      dupeAddress = toS <$> firstDuplicate (toS @_ @PRL.String . render . address . testInfo <$> filterInfo)

      -- testRunner :: forall hi i as ds. ( Show as, ToJSON as, Show ds, ToJSON ds, HasField "checks" i (Check.Checks ds), HasField "id" i Int, HasField "title" i Text, ToJSON i) => Address -> hi -> Test e tc rc hi i as ds effs -> [Sem effs ()] 
      -- testRunner = runTest rp

      -- semTree :: SuiteItem One () () effs [[Sem effs ()]]
      -- semTree = suite $ runTest rp

      -- mockSuite :: forall effs a. (forall hi i as ds. (Show i, Show as, Show ds) => Address -> hi -> MockTest hi i as ds effs -> a) -> SuiteItem () effs [a]
      -- mockSuite = suite $ runTest rp
      -- exeElmRunner:: forall hii. Address -> hii -> a -> Sem effs ()


      
-- runTest ::
--   RunParams Maybe e rc tc effs -> -- Run Params
--   Address ->
--   hi -> 
--   Test e tc rc hi i as ds effs -> -- Test Case
--   [Sem effs ()] -- [Test Iterations]


      --   itemRunner -> runtest include hooks -> exceute elems threads each hooks

--       root :: forall hii. SuiteItem hii effs [[Sem effs hii -> Sem effs () -> Sem effs ()]]
      -- root = suite itemRunner -- Test e tc rc () i0 as0 ds0 effs -> [Sem effs ()]

      -- run' :: Sem effs ()
      -- run' = do
      --         offset' <- utcOffset
      --         logItem . StartRun (RunTitle $ C.title rc) offset' $ toJSON rc
      --         logItem . FilterLog $ filterInfo
      --         exeElm includedAddresses itemRunner pure root
      --         logItem EndRun

   in uu

-- let

-- --   -- forall i as ds. Test e tc rc hi i as ds effs -> a) -> SuiteItem hi effs [a]
--  root :: forall hii. SuiteItem hii effs [[Sem effs hii -> Sem effs () -> Sem effs ()]]
--       root = (runTest rp) suite  -- Test e tc rc () i0 as0 ds0 effs -> [Sem effs ()]

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

mkEndpointSem ::
  forall rc tc e effs.
  (Config rc, Config tc, ToJSON e, Show e, MinEffs e effs) =>
  RunParams (Either FilterErrorType) e rc tc effs ->
  Address -> -- test address
  Either FilterErrorType (S.Set Int) -> -- a set of item Ids used for test case endpoints                                               -- test case processor function is applied to a hard coded list of test goups and returns a list of results
  Sem effs ()
mkEndpointSem runParams@RunParams {filters, itemIds} tstAddress iIds =
  let endpointFilter :: Address -> F.TestFilter rc tc
      endpointFilter targAddress =
        F.TestFilter
          { title = \_ _ _ -> "test address does not match endpoint target: " <> render targAddress,
            predicate = \_ address' _ -> address' == targAddress
          }

      allFilters :: [F.TestFilter rc tc]
      allFilters = endpointFilter tstAddress : filters
   in eitherf
        itemIds
        (logItem . LP.Error . FilterError)
        (\idSet -> mkSem runParams {filters = allFilters, itemIds = eitherToMaybe itemIds})