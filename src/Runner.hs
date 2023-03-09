
module Runner
  ( mkEndpointSem,
    RunParams (..),
    mkSem,
    module RB,
    module ItemFilter,
    module C,
  )
where

import qualified Check
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
import DSL.Logger (Logger (LogError), log, logError, logItem)
import Data.Aeson as A (ToJSON (toJSON), Value (Bool))
import Data.Either.Extra (Either, eitherToMaybe)
import Data.List (dropWhile)
import qualified Data.Set as S
import GHC.IO.Encoding.Types (TextEncoding (textEncodingName))
import GHC.Records (HasField (getField))
import Internal.RunnerBaseLazy (SuiteItem (..))
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
    ListLike (..),
    Maybe (..),
    Semigroup ((<>)),
    Show,
    Text,
    Traversable (sequenceA),
    catMaybes,
    const,
    debug,
    debug',
    either,
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
    parent,
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
    (?), IO
  )
import RunElementClasses as C
  ( Address,
    AddressElem,
    AddressElemType,
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
    SuiteSource,
    Test (..),
    TestSuite (..),
    queryElm,
  )
import TestFilter (activeAddresses, filterSuite, included)
import qualified TestFilter as F
  ( FilterLog (..),
    TestFilter (..),
    acceptAnyFilter,
    acceptFilter,
    applyFilters,
    filterLog,
  )
import qualified Prelude as PRL

getId :: HasField "id" i Int => i -> Int
getId = getField @"id"

runTestItems ::
  forall i as ds hd tc rc e effs.
  (ToJSON e, Show e, Config tc, ToJSON i, ItemClass i ds, Member (Logger e) effs) =>
  Maybe (S.Set Int) -> -- target Ids
  [i] -> -- ids
  rc -> -- runcoonfig
  Address ->
  hd ->
  ItemRunner e as ds i hd tc rc effs -> --rc -> Address -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs ()
  Test e tc rc hd i as ds effs ->
  [Sem effs ()]
runTestItems iIds items rc add hd itemRunner test@Test {config = tc} =
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
              itemRunner rc add hd test i
              logItem $ EndIteration iid

      inTargIds :: i -> Bool
      inTargIds i = maybe True (S.member $ getId i) iIds
   in case filteredItems of
        [] -> []
        xs -> [startTest >> sequence_ (applyRunner <$> xs) >> endTest]

runTest ::
  forall i rc hd as ds tc e effs.
  (ItemClass i ds, Config tc, ToJSON e, ToJSON as, ToJSON ds, Show e, Show as, Show ds, Member (Logger e) effs, ToJSON i) =>
  RunParams Maybe e rc tc effs -> -- Run Params
  Address ->
  hd ->
  Test e tc rc hd i as ds effs -> -- Test Case
  [Sem effs ()] -- [Test Iterations]
runTest RunParams {filters, rc, itemIds, itemRunner} add hd test@Test {config = tc, items} =
  F.acceptFilter (F.applyFilters filters rc add tc)
    ? runTestItems itemIds (items rc) rc add hd itemRunner test
    $ []

logLPError :: forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => FrameworkError e -> Sem effs ()
logLPError = logItem . LP.Error

data RunParams m e rc tc effs = RunParams
  { suite :: forall a. SuiteSource e tc rc effs a,
    filters :: [F.TestFilter rc tc],
    itemIds :: m (S.Set Int),
    itemRunner :: forall hi as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON i, ToJSON ds) => ItemRunner e as ds i hi tc rc effs,
    rc :: rc
  }

-- TODO - Error handling especially outside tests eg. in hooks
-- separate
exeElm ::
  forall hi e effs.
  (ToJSON e, Show e, Member (Logger e) effs) =>
  S.Set Address ->
  Address ->
  hi ->
  SuiteItem hi effs [Sem effs ()] ->
  Sem effs ()
exeElm includedAddresses parentAddress hi =
  let exElm' :: forall hi'. Address -> hi' -> SuiteItem hi' effs [Sem effs ()] -> Sem effs ()
      exElm' = exeElm includedAddresses

      hook = RC.Hook
      group' = RC.Group

      nxtAddress :: Text -> AddressElemType -> Address
      nxtAddress ttl at = push ttl at parentAddress

      exclude :: Text -> AddressElemType -> Bool
      exclude title at = S.notMember (nxtAddress title at) includedAddresses
   in --  TODO exceptions - run in terms of bracket / resource
      \case
        Tests {tests} ->
          sequence_ . join $ tests parentAddress hi
        --
        OnceHook {title = t, bHook, aHook, hkElms} ->
          let adr = nxtAddress t hook
           in exclude t hook ? pure () $
                do
                  logItem $ StartHook C.BeforeAll t
                  ho <- bHook hi
                  logItem $ EndHook C.BeforeAll t
                  sequence_ $ exElm' adr ho <$> hkElms
                  logItem $ StartHook C.AfterAll t
                  aHook ho
                  logItem $ EndHook C.AfterAll t
        --
        Group {title = t, gElms} ->
          exclude t group' ? pure () $
            do
              logItem $ StaXTGroup $ GroupTitle t
              sequence_ $ exElm' (nxtAddress t group') hi <$> gElms
              logItem $ EndGroup $ GroupTitle t

data RunComponents effs = RunComponents
  { suitItems :: [SuiteItem () effs [Sem effs ()]],
    filterLog :: F.FilterLog
  }

calcComponents ::
  forall rc tc e effs.
  (ToJSON e, Show e, Config tc, MinEffs e effs) =>
  RunParams Maybe e rc tc effs ->
  Either Text (RunComponents effs)
calcComponents runPrms@RunParams {suite, filters, rc, itemRunner} =
  do
    fl <- filterSuite rc suite filters
    Right $
      RunComponents
        { suitItems = (.un) $ suite $ runTest runPrms,
          filterLog = fl
        }

runSuite ::
  forall rc tc e effs.
  -- (ToJSON e, Show e, Config rc, Config tc, MinEffs e effs) =>
  RunParams Maybe e rc tc effs ->
  IO ()
runSuite rp@RunParams {suite, filters, rc, itemRunner} = uu
  -- let ethRunComponents :: Either Text (RunComponents effs)
  --     ethRunComponents = calcComponents rp

  --     run :: RunComponents effs -> IO ()
  --     run rCmp = uu
  --       -- do
  --       -- let flg = filterLog rCmp
  --       -- offset' <- utcOffset
  --       -- logItem . StartRun (RunTitle $ getField @"title" rc) offset' $ toJSON rc
  --       -- logItem . FilterLog . F.log $ flg
  --       -- sequence_ $ exeElm (included flg) rootAddress () <$> suitItems rCmp
  --       -- logItem EndRun

  --     lgError :: Text -> Sem effs ()
  --     lgError t = logError $ "Test Run Configuration Error. Duplicate Group Names: " <> t
  --  in uu

mkSem ::
  forall rc tc e effs.
  (ToJSON e, Show e, Config rc, Config tc, MinEffs e effs) =>
  RunParams Maybe e rc tc effs ->
  Sem effs ()
mkSem rp@RunParams {suite, filters, rc, itemRunner} =
  let ethRunComponents :: Either Text (RunComponents effs)
      ethRunComponents = calcComponents rp

      run :: RunComponents effs -> Sem effs ()
      run rCmp = do
        let  F.FilterLog {included, log=lg} = rCmp.filterLog
        offset' <- utcOffset
        logItem . StartRun (RunTitle $ getField @"title" rc) offset' $ toJSON rc
        logItem $ FilterLog lg
        sequence_ $ exeElm included rootAddress () <$> rCmp.suitItems
        logItem EndRun

      lgError :: Text -> Sem effs ()
      lgError t = logError $ "Test Run Configuration Error. Duplicate Group Names: " <> t
   in either lgError run ethRunComponents

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