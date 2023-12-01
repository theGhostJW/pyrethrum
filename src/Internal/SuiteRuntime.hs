--  should be able to remove this in later versions of GHC
-- https://gitlab.haskell.org/ghc/ghc/-/issues/21443
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Internal.SuiteRuntime where

import BasePrelude (retry)
import GHC.Exts
import Internal.RunTimeLogging (
  ExeEvent (..),
  ExeEventType (TestHook, TestHookRelease, ThreadHookRelease),
  LogControls (LogControls),
  SThreadId,
  logWorker,
  mkFailure,
  mkLogger,
  mkParentFailure,
  sink,
  stopWorker,
 )

import qualified Internal.RunTimeLoggingNew as NL

import qualified Internal.RunTimeLogging as L

import qualified Core as C
import qualified DSL.Internal.ApEvent as AE
import PyrethrumExtras (catchAll, txt, uu, (?))
import UnliftIO (
  concurrently_,
  finally,
  forConcurrently_,
  newIORef,
  tryAny,
  writeTMVar,
 )
import UnliftIO.Concurrent
import UnliftIO.STM (
  TQueue,
  atomically,
  modifyTVar,
  newEmptyTMVarIO,
  newTQueueIO,
  newTVarIO,
  tryReadTQueue,
  writeTQueue,
 )
import Prelude hiding (atomically, id, newEmptyTMVarIO, newTVarIO, readMVar)

-- PRENODE TO BE DEPRECATED
import Internal.PreNode (Context (..), OnceHook (..), PreNode (testHook), TestHook (..), ThreadHook (..))
import qualified Internal.PreNode as PN (
  Fixture (..),
  PreNode (..),
  PreNodeRoot,
  Test (..),
 )

import qualified Internal.ThreadEvent as TE

-- NEW

import Foreign.C (eMULTIHOP)
import GHC.RTS.Flags (DebugFlags (interpreter))
import Internal.RunTimeLoggingNew (FailPoint)
import Internal.ThreadEvent (EventType, ThreadEvent)
import qualified Internal.ThreadEvent as F (Frequency (..))
import qualified Prepare as C
import qualified Prepare as P

newtype ThreadCount = ThreadCount Int
  deriving (Show)

-- todo :: mkThreadCOunt with validation

executeNew :: (C.Config rc, C.Config tc) => ThreadCount -> NL.LogControls m NL.ExePath AE.ApEvent -> C.ExeParams [] rc tc effs -> IO ()
executeNew
  (ThreadCount maxThreads)
  NL.LogControls
    { sink
    , logWorker
    , stopWorker
    }
  C.ExeParams
    { suite
    , interpreter
    , runConfig
    } =
    concurrently_ logWorker linkExecute
   where
    linkExecute :: IO ()
    linkExecute =
      finally
        ( do
            let nodeList = P.prepare $ C.SuitePrepParams suite interpreter runConfig
            xtree <- mkXTreeNew (NL.ExePath []) nodeList
            executeNodesNew sink xtree maxThreads
        )
        stopWorker

-- executeNodesNew :: (NL.EngineEvent NL.ExePath a -> IO ()) -> ChildQ (ExeTreeNew ()) -> Int -> IO ()
executeNodesNew :: (ThreadEvent NL.ExePath AE.ApEvent -> IO ()) -> ChildQ (ExeTreeNew ()) -> Int -> IO ()
executeNodesNew sink nodes maxThreads =
  do
    rootLogger <- newLogger
    finally
      ( rootLogger NL.StartExecution
          >> forConcurrently_
            thrdTokens
            ( const do
                logger <- newLogger
                uu
                -- runNodes logger nodes
            )
      )
      ({- waitDone nodes >>-} rootLogger NL.EndExecution)
 where
  hkIn = Right (ExeIn () () ())
  thrdTokens = replicate maxThreads True
  newLogger = NL.mkLogger sink <$> UnliftIO.newIORef (-1) <*> myThreadId

data NFrequency = Each | Thread
  deriving (Show, Eq)

data ExeTreeNew hi where
  AfterOnce ::
    { path :: NL.ExePath
    , status' :: TVar AfterStatus
    , subNodes' :: ChildQ (ExeTreeNew hi)
    , after :: P.ApEventSink -> IO ()
    } ->
    ExeTreeNew hi
  AroundOnce ::
    { path :: NL.ExePath
    , setup :: P.ApEventSink -> hi -> IO ho
    , status :: TVar AroundStatus
    , cache :: TMVar (Either NL.FailPoint ho)
    , subNodes :: ChildQ (ExeTreeNew ho)
    , teardown :: Maybe (P.ApEventSink -> ho -> IO ())
    } ->
    ExeTreeNew hi
  After ::
    { path :: NL.ExePath
    , frequency :: NFrequency
    , subNodes' :: ChildQ (ExeTreeNew hi)
    , after :: P.ApEventSink -> IO ()
    } ->
    ExeTreeNew hi
  Around ::
    { path :: NL.ExePath
    , frequency :: NFrequency
    , setup :: P.ApEventSink -> hi -> IO ho
    , subNodes :: ChildQ (ExeTreeNew ho)
    , teardown :: Maybe (P.ApEventSink -> ho -> IO ())
    } ->
    ExeTreeNew hi
  Test ::
    { path :: NL.ExePath
    , title :: Text
    , tests :: ChildQ (P.TestItem IO hi)
    } ->
    ExeTreeNew hi

mkXTreeNew :: NL.ExePath -> NonEmpty (P.PreNode IO NonEmpty hi) -> IO (ChildQ (ExeTreeNew hi))
mkXTreeNew xpth preNodes =
  do
    subTrees <- traverse mkNode preNodes
    mkChildQ subTrees
 where
  mkNode :: forall hi. P.PreNode IO NonEmpty hi -> IO (ExeTreeNew hi)
  mkNode pn =
    pn & \case
      P.Before
        { frequency
        , action
        , subNodes
        } ->
          frequency & \case
            F.Once -> mkOnceHook subNodes action Nothing
            F.Thread -> mkNHook' Thread
            F.Each -> mkNHook' Each
         where
          mkNHook' = mkNHook subNodes action Nothing
      P.After{frequency, subNodes', after} ->
        do
          cq <- mkXTreeNew path subNodes'
          let mkAfter fq = pure $ After{path, frequency = fq, subNodes' = cq, after}
          frequency & \case
            F.Once -> do
              status' <- newTVarIO AfterQPending
              pure $ AfterOnce{path, status', subNodes' = cq, after}
            F.Thread -> mkAfter Thread
            F.Each -> mkAfter Each
      P.Around
        { frequency
        , setup
        , subNodes
        , teardown
        } ->
          frequency & \case
            F.Once -> mkOnceHook subNodes setup teardown'
            F.Thread -> nHook' Thread
            F.Each -> nHook' Each
         where
          teardown' = Just teardown
          nHook' = mkNHook subNodes setup teardown'
      P.Test
        { config = c
        , tests
        } -> do
          cq <- mkChildQ tests
          pure $ Test{path, title = c.title, tests = cq}
   where
    path = NL.ExePath $ pn.path : xpth.unExePath

    mkOnceHook :: forall hi' ho'. NonEmpty (P.PreNode IO NonEmpty ho') -> (P.ApEventSink -> hi' -> IO ho') -> Maybe (P.ApEventSink -> ho' -> IO ()) -> IO (ExeTreeNew hi')
    mkOnceHook subNodes' setup teardown = do
      status <- newTVarIO SetupPending
      cache <- newEmptyTMVarIO
      subNodes <- mkXTreeNew path subNodes'
      pure
        $ AroundOnce
          { path
          , setup
          , status
          , cache
          , subNodes
          , teardown
          }

    mkNHook :: forall hi' ho'. NonEmpty (P.PreNode IO NonEmpty ho') -> (P.ApEventSink -> hi' -> IO ho') -> Maybe (P.ApEventSink -> ho' -> IO ()) -> NFrequency -> IO (ExeTreeNew hi')
    mkNHook subNodes' setup teardown frequency = do
      s <- newTVarIO HookPending
      cache <- newEmptyTMVarIO
      subNodes <- mkXTreeNew path subNodes'
      pure
        $ Around
          { path
          , setup
          , frequency
          , subNodes
          , teardown
          }

{-
  ##########################################################################
  ############################ OLD CODE ####################################
  ##########################################################################
-}

data HookStatus
  = HookVoid
  | HookPending
  | HookRunning
  | HookComplete
  | HookReleaseRunning
  | HookReleased
  deriving (Show, Eq)

data BeforeStatus
  = BeforePending
  | BeforeRunning
  | BeforeQRunning
  | BeforeDone
  | BeforeAbandoning
  deriving (Show, Eq)

data AroundStatus
  = SetupPending
  | SetupRunning
  | AroundQRunning
  | TeardownRunning
  | AroundDone
  | AroundAbandoning
  deriving (Show, Eq)

data AfterStatus
  = AfterQPending
  | AfterQRunning
  | AfterRunning
  | AfterDone
  | AfterAbandoning
  deriving (Show, Eq)

data XContext a = XContext
  { loc :: L.Loc
  , evtLogger :: L.ExeEvent L.Loc a -> IO ()
  }

mkCtx :: forall a. XContext a -> Context a
mkCtx XContext{loc, evtLogger} =
  let
    msgLogger :: a -> IO ()
    msgLogger = evtLogger . ApEvent
   in
    Context loc msgLogger

waitDone :: ExeTree a oi ti -> IO ()
waitDone rg = atomically $ do
  s <- nodeStatus rg
  unless (s == Done) retry

data IdxLst a = IdxLst
  { maxIndex :: Int
  , lst :: [a]
  , currIdx :: TVar Int
  }

mkIdxLst :: a -> STM (IdxLst a)
mkIdxLst elm = IdxLst 0 [elm] <$> newTVar 0

data ChildQ a = ChildQ
  { status :: TVar CanRun
  , childNodes :: TQueue a
  , runningCount :: TVar Int
  }

data CanRun
  = Runnable
  | Saturated
  | Done
  deriving (Show, Eq)

data Concurrency
  = Sequential
  | Concurrent
  deriving (Show, Eq)

data XTest a si ti ii = XTest
  { loc :: L.Loc
  , test :: XContext a -> si -> ti -> ii -> IO ()
  }

runTestHook :: forall so' to' tsti' ho' a. XContext a -> Either AbandonOld (ExeIn so' to' tsti') -> TestHook a so' to' tsti' ho' -> IO (Either AbandonOld (ExeIn so' to' ho'))
runTestHook ctx@XContext{loc = testLoc} tstIn testHk =
  tstIn
    & either
      logReturnAbandonned
      runHook
 where
  tstCtx :: XContext a
  tstCtx = ctx{loc = mkTestChildLoc testLoc TestHook}
  logReturnAbandonned a =
    let
      result = pure (Left a)
      loga = logAbandonned tstCtx TestHook a >> result
     in
      testHk & \case
        TestNone{} -> result
        TestBefore{} -> loga
        TestAfter{} -> result
        TestAround{} -> loga

  runHook (ExeIn oi ti tsti) =
    catchAll
      ( let
          exHk h = ExeIn oi ti <<$>> Right <$> withStartEnd tstCtx TestHook (h (mkCtx tstCtx) oi ti tsti)
          voidHk = pure @IO $ Right @AbandonOld (ExeIn oi ti tsti)
         in
          testHk & \case
            TestNone{} -> voidHk
            TestBefore{hook} -> exHk hook
            TestAfter{} -> voidHk
            TestAround{hook} -> exHk hook
      )
      (logReturnFailure tstCtx TestHook)

mkTestChildLoc :: (Show a) => L.Loc -> a -> L.Loc
mkTestChildLoc testLoc evt = L.Node testLoc $ txt evt

releaseTestHook :: forall so' to' tsti' ho' a. XContext a -> Either AbandonOld ho' -> TestHook a so' to' tsti' ho' -> IO ()
releaseTestHook ctx@XContext{loc = testLoc} tsti = \case
  TestNone -> noRelease
  TestBefore{} -> noRelease
  TestAfter{releaseOnly} -> runRelease releaseOnly
  TestAround{release} -> runRelease release
 where
  noRelease = pure ()
  runRelease = releaseHook (ctx{loc = mkTestChildLoc testLoc TestHookRelease}) TestHookRelease tsti

canRunChildQ :: ChildQ a -> STM Bool
canRunChildQ cq =
  runnable <$> readTVar cq.status
 where
  runnable = \case
    Runnable -> True
    Saturated -> False
    Done -> False

newtype QElementRun = QElementRun {hasRun :: Bool}

runChildQ :: forall a. Concurrency -> (a -> IO ()) -> (a -> STM CanRun) -> ChildQ a -> IO QElementRun
runChildQ concurrency runner childCanRun q@ChildQ{childNodes, status, runningCount} =
  runChildQ' (QElementRun False)
 where
  runChildQ' :: QElementRun -> IO QElementRun
  runChildQ' hasRun =
    do
      eNext <- atomically $ do
        rc <- readTVar runningCount
        mNext <- tryReadTQueue childNodes
        mNext
          & maybe
            ( do
                if
                  -- broken
                  | rc < 0 -> bug @Void $ error "framework error - this should not happen - child que running count below zero"
                  -- nothing running an nothing in the q we must be done
                  | rc == 0 -> writeTVar status Done
                  -- q is empty but still threads running
                  | otherwise -> writeTVar status Saturated
                pure Nothing
            )
            ( \a -> do
                modifyTVar runningCount succ
                pure $ Just a
            )

      eNext
        & maybe
          (pure hasRun)
          ( \a -> do
              finally
                do
                  cr <- atomically $ childCanRun a
                  cr & \case
                    -- runner MUST ensure the integrity of sub element status and handle all exceptions
                    Runnable -> do
                      -- when Concurrent, the element is placed back on the end of the q before running so
                      -- can be picked up by other threads child qs are concurrent test items are not
                      when (concurrency == Concurrent)
                        $ atomically (writeTQueue childNodes a)
                      runner a
                    -- when not runnabel clean up the q by returning without adding the element back
                    Saturated -> pure ()
                    Done -> pure ()
                (atomically $ modifyTVar runningCount pred)
              runChildQ' $ QElementRun True
          )

mkChildQ :: (Foldable m) => m a -> IO (ChildQ a)
mkChildQ children = do
  s <- newTVarIO Runnable
  q <- newTQueueIO
  rc <- newTVarIO 0
  atomically $ traverse_ (writeTQueue q) children
  pure
    $ ChildQ
      { status = s
      , childNodes = q
      , runningCount = rc
      }

data OnceVal a oi oo = OnceVal
  { hook :: OnceHook a oi oo
  , status :: TVar HookStatus
  , value :: TMVar (Either AbandonOld oo)
  }

mkOnceVal :: OnceHook a oi oo -> IO (OnceVal a oi oo)
mkOnceVal h = OnceVal h <$> newTVarIO HookPending <*> newEmptyTMVarIO

data XFixture a oi ti tsti where
  XFixture ::
    { loc :: L.Loc
    , onceHook :: OnceVal a oi oo
    , threadHook :: ThreadHook a oo ti to
    , testHook :: TestHook a oo to tsti tsto
    , tests :: ChildQ (XTest a oo to tsto)
    , threadLimit :: ThreadLimit
    } ->
    XFixture a oi ti tsti

withOnceHook :: XContext a -> ThreadLimit -> ChildQ b -> Either AbandonOld (ExeIn si ti tsti) -> OnceVal a si so -> (Either AbandonOld (ExeIn so ti tsti) -> IO ()) -> IO ()
withOnceHook ctx tl@ThreadLimit{maxThreads, runningThreads} childq hkIn onceHook childAction = do
  cr <- atomically canRun
  when cr $ do
    eso <- onceHookVal ctx ((.onceIn) <$> hkIn) onceHook
    finally
      ( do
          childAction $ do
            hi <- hkIn
            so <- eso
            Right $ hi{onceIn = so}
      )
      (releaseOnceHookIfReady ctx runningThreads childq eso onceHook)
 where
  canRun = do
    hs <- readTVar onceHook.status
    cq <- canRunChildQ childq
    mt <- withinThreadLimit tl
    let r = hs /= HookReleased && hs /= HookReleaseRunning && cq && mt
    when r $ modifyTVar runningThreads succ
    pure r

withThreadHook :: XContext a -> Either AbandonOld (ExeIn oi ti tsti) -> ThreadHook a oi ti to -> (Either AbandonOld (ExeIn oi to tsti) -> IO ()) -> IO ()
withThreadHook ctx hkIn threadHook childAction =
  do
    eto <-
      threadHookVal
        ctx
        hkIn
        threadHook
    finally
      ( childAction $ do
          hi <- hkIn
          to <- eto
          Right $ hi{threadIn = to}
      )
      ( let
          doRelease = releaseHook ctx ThreadHookRelease eto
         in
          threadHook & \case
            ThreadNone -> pure ()
            ThreadBefore{} -> pure ()
            ThreadAfter{releaseOnly} -> doRelease releaseOnly
            ThreadAround{release} -> doRelease release
      )

data ThreadLimit = ThreadLimit
  { maxThreads :: Maybe Int
  , runningThreads :: TVar Int
  }

withinThreadLimit :: ThreadLimit -> STM Bool
withinThreadLimit ThreadLimit{maxThreads, runningThreads} =
  (\rt -> maybe True (rt <) maxThreads) <$> readTVar runningThreads

runXFixture ::
  forall oi ti ii a.
  (ExeEvent L.Loc a -> IO ()) ->
  Either AbandonOld (ExeIn oi ti ()) ->
  TestHook a oi ti () ii ->
  XFixture a oi ti ii ->
  IO ()
runXFixture
  evtLgr
  exin
  prntTstHk
  fx@XFixture
    { loc
    , onceHook
    , threadHook
    , testHook
    , tests
    , threadLimit
    } =
    do
      canRun' <- atomically $ canRunChildQ tests
      when canRun'
        $ withOnceHook ctx threadLimit tests exin onceHook
        $ \trdIn ->
          withThreadHook ctx trdIn threadHook $ \tstIn ->
            void $ runTests tstIn
   where
    ctx = XContext loc evtLgr
    runTst fxIn tst = do
      prntTstOut <- runTestHook ctx exin prntTstHk
      let tstIn' = do
            ExeIn{onceIn, threadIn} <- fxIn
            ExeIn{tstIn} <- prntTstOut
            Right $ ExeIn{onceIn, threadIn, tstIn}
      finally
        (runXTest ctx tstIn' testHook tst)
        (releaseTestHook ctx ((.tstIn) <$> prntTstOut) prntTstHk)
    runTests fxIn = runChildQ Sequential (runTst fxIn) (const $ pure Runnable) tests

runXTest :: forall so' to' tsti' ho' a. XContext a -> Either AbandonOld (ExeIn so' to' tsti') -> TestHook a so' to' tsti' ho' -> XTest a so' to' ho' -> IO ()
runXTest ctx@XContext{loc = fxLoc} fxIpts testHk test@XTest{loc = tstLoc, test = tstAction} =
  do
    let tstCtx = ctx{loc = tstLoc} :: XContext a
    eho <- runTestHook tstCtx fxIpts testHk
    eho
      & either
        (logAbandonned tstCtx L.Test)
        ( \(ExeIn oi ti tsti') ->
            finally
              ( withStartEnd ctx L.Test
                  $ catchAll
                    (tstAction tstCtx oi ti tsti')
                    (void . logReturnFailure ctx L.Test)
              )
              (releaseTestHook tstCtx ((.tstIn) <$> eho) testHk)
        )

mkThreadLimit :: Maybe Int -> IO ThreadLimit
mkThreadLimit mi = ThreadLimit mi <$> newTVarIO 0

mkXFixture ::
  L.Loc ->
  PN.Fixture a oi ti tsti ->
  IO (XFixture a oi ti tsti)
mkXFixture loc PN.Fixture{onceHook, threadHook, testHook, tests, maxThreads, title} = do
  oh <- mkOnceVal onceHook
  ts <- mkChildQ $ mkXTest <$> tests
  threadLimit <- mkThreadLimit maxThreads
  pure
    $ XFixture
      { loc = fxLoc
      , onceHook = oh
      , threadHook
      , testHook
      , tests = ts
      , threadLimit
      }
 where
  fxLoc = L.Node loc title
  mkXTest PN.Test{id, test} =
    XTest
      { loc = L.Node fxLoc $ "Test :: " <> id
      , test = test . mkCtx
      }

data ExeTree a oi ti where
  XGroup ::
    { loc :: L.Loc
    , threadLimit :: ThreadLimit
    , onceHook :: OnceVal a oi oo
    , threadHook :: ThreadHook a oo ti to
    , subNodes :: ChildQ (ExeTree a oo to)
    } ->
    ExeTree a oi ti
  XFixtures ::
    { loc :: L.Loc
    , threadLimit :: ThreadLimit
    , testHook :: TestHook a oi ti () tsto
    , fixtures :: ChildQ (XFixture a oi ti tsto)
    } ->
    ExeTree a oi ti

prepare :: forall o t a. PreNode a o t -> IO (ExeTree a o t)
prepare =
  prepare' L.Root 0
 where
  prepare' :: forall oi ti. L.Loc -> Int -> PN.PreNode a oi ti -> IO (ExeTree a oi ti)
  prepare' parentLoc subElmIdx pn = do
    let nodeLoc = L.Node parentLoc
    case pn of
      PN.Group
        { title
        , threadLimit = maxThreads
        , onceHook
        , threadHook
        , subNodes = nodes
        } -> do
          onceHk <- mkOnceVal onceHook
          let loc = nodeLoc title
          childlst <- traverse (prepare' parentLoc 0) nodes
          subNodes <- mkChildQ childlst
          tl <- newTVarIO 0
          threadLimit <- mkThreadLimit maxThreads
          pure
            $ XGroup
              { loc
              , threadLimit
              , onceHook = onceHk
              , threadHook
              , subNodes
              }
      PN.Fixtures
        { title
        , threadLimit = maxThreads
        , testHook
        , fixtures = arFxs
        } ->
          do
            let loc = nodeLoc title
            runningCount <- newTVarIO 0
            fxs <- traverse (mkXFixture loc) arFxs
            fixtures <- mkChildQ fxs
            threadLimit <- mkThreadLimit maxThreads
            pure
              $ XFixtures
                { loc
                , threadLimit
                , fixtures
                , testHook
                }

logAbandonned :: XContext a -> ExeEventType -> AbandonOld -> IO ()
logAbandonned XContext{loc, evtLogger} fet AbandonOld{sourceLoc, sourceEventType, exception} =
  evtLogger (mkParentFailure loc fet sourceLoc sourceEventType exception)

logReturnFailure :: XContext a -> ExeEventType -> SomeException -> IO (Either AbandonOld b)
logReturnFailure XContext{loc, evtLogger} et e =
  do
    evtLogger (mkFailure loc et (txt et <> "Failed at: " <> txt loc) e)
    pure $ Left $ AbandonOld loc et e

readOrLockHook :: TVar HookStatus -> TMVar (Either AbandonOld ho) -> STM (Maybe (Either AbandonOld ho))
readOrLockHook hs hVal =
  do
    s <- readTVar hs
    (==) s HookPending
      ? (writeTVar hs HookRunning >> pure Nothing)
      -- blocks until the other thread has written the MVar
      $ (Just <$> readTMVar hVal)

setHookComplete :: TVar HookStatus -> TMVar (Either AbandonOld ho) -> Either AbandonOld ho -> STM (Either AbandonOld ho)
setHookComplete hs hVal eso = do
  putTMVar hVal eso
  writeTVar hs HookComplete
  pure eso

tryLockRun :: TVar HookStatus -> TMVar (Either AbandonOld ho) -> IO (Either AbandonOld ho) -> IO (Either AbandonOld ho)
tryLockRun hkStatus hkVal hkAction =
  atomically (readOrLockHook hkStatus hkVal)
    >>= maybe
      hkAction
      pure

runLogHook :: forall hi ho a. XContext a -> ExeEventType -> (Context a -> hi -> IO ho) -> hi -> IO (Either AbandonOld ho)
runLogHook ctx@XContext{loc} hkEvent hook hi =
  withStartEnd ctx hkEvent
    $ catchAll
      (Right <$> hook (mkCtx ctx) hi)
      (logReturnFailure ctx hkEvent)

withStartEnd :: XContext a -> ExeEventType -> IO b -> IO b
withStartEnd XContext{loc, evtLogger} evt io = do
  evtLogger $ Start evt loc
  finally io . evtLogger $ End evt loc

abandonLogHook :: XContext a -> ExeEventType -> AbandonOld -> IO (Either AbandonOld b)
abandonLogHook ctx evtTp abandonOld =
  do
    withStartEnd ctx evtTp
      $ logAbandonned ctx evtTp abandonOld
    pure $ Left abandonOld

abandonnedOnceHookVal :: forall ho a. XContext a -> AbandonOld -> TVar HookStatus -> TMVar (Either AbandonOld ho) -> IO (Either AbandonOld ho)
abandonnedOnceHookVal ctx abandonOld hs hkVal =
  tryLockRun
    hs
    hkVal
    ( do
        abandonLogHook ctx L.OnceHook abandonOld
        atomically $ writeTVar hs HookComplete
        pure $ Left abandonOld
    )

threadHookVal :: forall oi ti tsti to a. XContext a -> Either AbandonOld (ExeIn oi ti tsti) -> ThreadHook a oi ti to -> IO (Either AbandonOld to)
threadHookVal ctx hkIn thook =
  hkIn
    & either
      (abandonLogHook ctx L.ThreadHook)
      ( \(ExeIn oi ti _tsti) ->
          let
            thrdHk :: (Context a -> oi -> ti -> IO to) -> Context a -> ti -> IO to
            thrdHk thHk = flip thHk oi
            runHook hook' = runLogHook ctx L.ThreadHook (thrdHk hook') ti
            passThrough = pure @IO $ Right @AbandonOld ti
           in
            thook & \case
              ThreadNone -> passThrough
              ThreadBefore{hook} -> runHook hook
              ThreadAfter{} -> passThrough
              ThreadAround{hook} -> runHook hook
      )

onceHookVal :: forall hi ho a. XContext a -> Either AbandonOld hi -> OnceVal a hi ho -> IO (Either AbandonOld ho)
onceHookVal ctx ehi OnceVal{hook = oHook, status, value} =
  ehi
    & either
      (\abandonOld -> abandonnedOnceHookVal ctx abandonOld status value)
      ( \hi' ->
          let
            passThrough = pure @IO $ Right @AbandonOld hi'
            runHk hk =
              tryLockRun
                status
                value
                ( runLogHook ctx L.OnceHook hk hi'
                    >>= atomically
                    . setHookComplete status value
                )
           in
            oHook & \case
              OnceNone -> passThrough
              OnceAfter{} -> passThrough
              OnceBefore{hook} -> runHk hook
              OnceAround{hook} -> runHk hook
      )

releaseHook :: XContext a -> ExeEventType -> Either AbandonOld ho -> (Context a -> ho -> IO ()) -> IO ()
releaseHook ctx@XContext{evtLogger, loc} evt eho hkRelease =
  withStartEnd ctx evt
    $ eho
    & either
      (logAbandonned ctx evt)
      ( \so ->
          catchAll
            (hkRelease (mkCtx ctx) so)
            (evtLogger . mkFailure loc evt ("Hook Release Failed: " <> txt evt <> " " <> txt loc))
      )

releaseOnceHookIfReady ::
  XContext a ->
  TVar Int ->
  ChildQ b ->
  Either AbandonOld ho ->
  OnceVal a oi ho ->
  IO ()
releaseOnceHookIfReady ctx cntr childq eho hk =
  do
    locked <- atomically tryLock
    when locked
      $ finally
        ( hk.hook & \case
            OnceNone -> pure ()
            OnceBefore{} -> pure ()
            OnceAfter{releaseOnly} -> doRelease releaseOnly
            OnceAround{release} -> doRelease release
        )
        (atomically $ writeTVar hk.status HookReleased)
 where
  doRelease = releaseHook ctx L.OnceHookRelease eho
  tryLock :: STM Bool
  tryLock =
    do
      c <- readTVar cntr
      let nxt = pred c
      writeTVar cntr nxt
      hks <- readTVar hk.status
      s <- readTVar childq.status
      let r =
            nxt
              == 0
              && canRelease hks
              && s
              == Done
      when r
        $ writeTVar hk.status HookReleaseRunning
      pure r
   where
    canRelease = \case
      HookVoid -> False
      HookPending -> False
      HookRunning -> False
      HookComplete -> True
      HookReleaseRunning -> False
      HookReleased -> False

data AbandonOld = AbandonOld
  { sourceLoc :: L.Loc
  , sourceEventType :: ExeEventType
  , exception :: SomeException
  }
  deriving (Show)

data ExeIn oi ti tsti = ExeIn
  { onceIn :: oi
  , threadIn :: ti
  , tstIn :: tsti
  }

nodeStatus =
  readTVar
    . ( \case
          XGroup{subNodes = s} -> s.status
          XFixtures{fixtures} -> fixtures.status
      )

type Logger = NL.EngineEvent NL.ExePath AE.ApEvent -> IO ()

logAbandonnedNew :: Logger -> NL.ExePath -> EventType -> NL.FailPoint -> IO ()
logAbandonnedNew lgr p e a =
  lgr
    $ NL.ParentFailure
      { loc = p
      , eventType = e
      , parentLoc = a.path
      , parentEventType = a.eventType
      }

logReturnFailureNew :: Logger -> NL.ExePath -> EventType -> SomeException -> IO (Either NL.FailPoint b)
logReturnFailureNew lgr p et e =
  do
    lgr (NL.mkFailure p et e)
    pure . Left $ NL.FailPoint p et

data CanAbandon = None | Partial | Full
  deriving (Show, Eq)

canAbandon :: ExeTreeNew hi -> STM CanAbandon
canAbandon = \case
  AfterOnce{subNodes', status'} -> do
    s <- readTVar status'
    pure $ s & \case
      AfterQPending -> Full
      AfterQRunning -> Partial
      AfterRunning -> None
      AfterAbandoning -> None
      AfterDone -> None
  AroundOnce{subNodes, status} -> do
    s <- readTVar status
    pure $ s & \case
      SetupPending -> Full
      SetupRunning -> Partial
      AroundQRunning -> Partial
      AroundAbandoning -> None
      TeardownRunning -> None
      AroundDone -> None

  -- base non singleton can run status on underlying q
  After{subNodes'} -> qStatus subNodes'
  Around{subNodes} -> qStatus subNodes
  Test{tests} -> qStatus tests
 where
  qStatus q = convert <$> readTVar q.status
  convert = \case
    Runnable -> Full
    Saturated -> None
    Done -> None

canRunXTree :: ExeTreeNew hi -> STM CanRun
canRunXTree = \case
  AfterOnce{subNodes', status'} ->
    do
      s <- readTVar status'
      qs <- readTVar subNodes'.status
      pure $ s & \case
        AfterQPending -> Runnable
        AfterQRunning -> stepDownQStatus qs
        AfterRunning -> Saturated
        AfterAbandoning -> Saturated
        AfterDone -> Done
  AroundOnce{subNodes, status} -> do
    s <- readTVar status
    qs <- readTVar subNodes.status
    pure $ s & \case
      SetupPending -> Runnable
      SetupRunning -> Runnable
      AroundQRunning -> stepDownQStatus qs
      TeardownRunning -> Saturated
      AroundDone -> Done

  -- base non singleton can run status on underlying q
  After{subNodes'} -> qStatus subNodes'
  Around{subNodes} -> qStatus subNodes
  Test{tests} -> qStatus tests
 where
  qStatus q = readTVar q.status
  stepDownQStatus = \case
    Runnable -> Runnable
    Saturated -> Saturated
    Done -> Saturated

abandonTree :: forall hi. Logger -> NL.FailPoint -> ExeTreeNew hi -> IO ()
abandonTree lgr ab = \case
  ao@AfterOnce{path, status', subNodes'} ->
    do
      aStatus <- atomically $ do
        ca <- canAbandon ao
        when (ca == Full)
          $ writeTVar status' AfterAbandoning
        pure ca
      aStatus & \case
        Full ->
          finally
            (abandonChildren' subNodes')
            ( do
                -- only AbandonOld after if no nodes had already started
                logAbandon path $ TE.Hook TE.Once TE.After
                atomically $ writeTVar status' AfterDone
            )
        Partial -> abandonChildren' subNodes'
        None -> pure ()
  AroundOnce{path, status, cache, subNodes, teardown} -> do
    setUpLocked <- atomically $ do
      s <- readTVar status
      -- HERE
      let locked = s == SetupPending
          canAbandon =
            s & \case
              SetupPending -> True
              SetupRunning -> False
              AroundQRunning -> False
              TeardownRunning -> False
              AroundDone -> False
              AroundAbandoning -> False
      when locked
        $ writeTVar status AroundAbandoning
      pure locked
    when setUpLocked
      $ logAbandon path (TE.Hook TE.Once TE.SetUp)
    finally
      (abandonChildren' subNodes)
      ( -- only AbandonOld teardown if setup has not started
        when setUpLocked $ do
          finally
            (logAbandon path $ TE.Hook TE.Once TE.TearDown)
            (atomically $ writeTVar status AroundDone)
      )

  -- HERE THIS FUNCTION NEEDS TO BE GENERALISED TO RUN ~ finish runNodenew and come back here
  After{path, frequency, subNodes', after} -> do
    qlocked <- atomically $ do
      qs <- readTVar subNodes'.status
      rc <- readTVar subNodes'.runningCount
      let locked = rc == 0 && qs == Runnable
      when locked
        $ writeTVar subNodes'.status Saturated
      pure locked
    -- finally
    --   (abandonChildren' subNodes')
    --   ( -- only AbandonOld teardown if setup has not started
    --     when qlocked $ do
    --       finally
    --         (logAbandon path $ TE.Hook TE.Once TE.TearDown)
    --         (atomically $ writeTVar frequency F.Once)
    --   )

    uu -- abandonChildren subNodes'
    -- todo:: generate runner to abndon nodes and travese que
    -- WRITE EXE CODE TGHEN COME BACK TO THIS

  --  finally
  --   ()
  --   ( -- only AbandonOld teardown if setup has not started
  --     when setUpLocked $ do
  --       finally
  --         (logAbandon path $ TE.Hook TE.Once TE.TearDown)
  --         (atomically $ writeTVar status $ Teardown Complete)
  --   )

  -- do
  -- lgr $ NL.mkParentFailure path NL.After ab
  -- atomically $ abandonTree lgr subNodes' ab
  -- after $ logAbandonnedNew lgr path NL.After ab

  Around{path, frequency, setup, subNodes, teardown} -> uu
  -- todo:: generate runner to abndon nodes and travese que
  -- lgr $ NL.mkParentFailure path NL.Around ab
  -- atomically $ abandonTree lgr subNodes ab
  -- teardown & \case
  --   Nothing -> pure ()
  --   Just td -> td $ logAbandonnedNew lgr path NL.AroundRelease ab
  Test{path, title, tests} ->
    void $ runChildQ Sequential (abndonTest path) (const $ pure Runnable) tests
 where
  abandonChildren' :: forall hi''. ChildQ (ExeTreeNew hi'') -> IO ()
  abandonChildren' = abandonChildren lgr ab

  abndonTest :: NL.ExePath -> P.TestItem IO hi -> IO ()
  abndonTest fxPath tst = logAbandonnedNew lgr (mkTestPath fxPath tst) TE.Test ab

  mkTestPath :: NL.ExePath -> P.TestItem IO hi -> NL.ExePath
  mkTestPath p P.TestItem{id, title} = NL.ExePath $ AE.TestPath{id, title} : p.unExePath

  logAbandon p et = logAbandonnedNew lgr p et ab

abandonChildren :: forall hi'. Logger -> NL.FailPoint -> ChildQ (ExeTreeNew hi') -> IO ()
abandonChildren lgr ab =
  void . runChildQ Sequential (abandonTree lgr ab) canRunXTree
 where
  abandonChildren' :: forall hi''. ChildQ (ExeTreeNew hi'') -> IO ()
  abandonChildren' = abandonChildren lgr ab

-- TODO: warning on uu
--  xtr & \case
-- AfterOnce
--   { path
--   , status'
--   , subNodes'
--   , after
--   } -> uu
-- AroundOnce{subNodes, setup, teardown} -> uu
-- After{subNodes', after} -> uu
-- Around{subNodes, setup, teardown} -> uu
-- Test{tests} -> uu

-- returns
--   Nothing -> cache empty hook needs to be run
--   value (ho or AbandonOld) -> cache has value and is ready to be used
--    - will block until cach is full if already running
-- readOrLockHookNew :: forall s ho. TVar s -> (s -> Bool) -> s -> TMVar (Either AbandonOld ho) -> STM (Maybe (Either AbandonOld ho))
-- readOrLockHookNew hs isPending runningStatus hVal =
--   do
--     s <- readTVar hs
--     isPending s
--       ? (writeTVar hs runningStatus >> pure Nothing)
--       -- blocks until the other thread has written the MVar
--       $ (Just <$> readTMVar hVal)

--- HERE !!!!!!!!!

canRunAfterOnce :: AfterStatus -> CanRun -> Bool
canRunAfterOnce s qs = case s of
  AfterQPending -> True
  AfterQRunning -> case qs of
    Runnable -> True
    Saturated -> False
    Done -> False
  AfterRunning -> False
  AfterAbandoning -> False
  AfterDone -> False

canLockAfterOnce :: AfterStatus -> CanRun -> Bool
canLockAfterOnce s qs = case s of
  AfterQPending -> False
  AfterQRunning -> case qs of
    Runnable -> False
    Saturated -> False
    Done -> True
  AfterRunning -> False
  AfterAbandoning -> False
  AfterDone -> False

canLockSetup :: AroundStatus -> CanRun -> Bool
canLockSetup s _qs = case s of
  SetupPending -> True
  SetupRunning -> False
  AroundQRunning -> False
  TeardownRunning -> False
  AroundDone -> False
  AroundAbandoning -> False

canLockTeardown :: AroundStatus -> CanRun -> Bool
canLockTeardown s qs = case s of
  SetupPending -> False
  SetupRunning -> False
  AroundQRunning -> case qs of
    Runnable -> False
    Saturated -> False
    Done -> True
  TeardownRunning -> False
  AroundDone -> False
  AroundAbandoning -> False

runNodeNew ::
  forall hi.
  Logger ->
  NodeIn hi ->
  ExeTreeNew hi ->
  IO ()
runNodeNew lgr hi xt =
  case hi of
    Abandon ab -> abandonTree lgr ab xt
    hi' ->
      let
        logRun' :: TE.EventType -> IO b -> IO (Either NL.FailPoint b)
        logRun' = logRun lgr xt.path

        logRun_ :: TE.EventType -> IO b -> IO ()
        logRun_ et io = void $ logRun' et io

        logAbandonned' = logAbandonnedNew lgr xt.path

        runSubNodes :: forall hi'. NodeIn hi' -> ChildQ (ExeTreeNew hi') -> IO QElementRun
        runSubNodes hi'' = runChildQ Concurrent (runNodeNew lgr hi'') canRunXTree

        runSubNodes_ :: forall hi'. NodeIn hi' -> ChildQ (ExeTreeNew hi') -> IO ()
        runSubNodes_ n = void . runSubNodes n

        -- tree generation is restricted by typeclasses so unless the typeclass constrint implmentation is wrong
        -- execution trees with invalid structure (Thread or Once depending on Each) should never be generated.
        shouldNeverHappen cst = bug @Void . error $ "EachIn should not be passed to: " <> cst <> " " <> txt xt.path
        sink = lgr . NL.ApEvent
       in
        case xt of
          -- as we know tree shaking has been executed prior to running we can assume we
          -- always need to execute once hooks if the status is correct. There is no
          -- possibility of empty subnodes due to tree shaking.
          AfterOnce
            { path
            , status'
            , subNodes'
            , after
            } -> do
              run <- atomically $ do
                s <- readTVar status'
                qs <- readTVar subNodes'.status
                when (s == AfterQPending)
                  $ writeTVar status' AfterQRunning
                pure $ canRunAfterOnce s qs
              when run
                $ finally
                  (runSubNodes_ hi subNodes')
                  ( do
                      locked <- atomically $ tryLock status' subNodes' canLockAfterOnce AfterRunning
                      when locked
                        $ finally
                          (void $ logRun' (TE.Hook TE.Once TE.After) (after sink))
                          (atomically $ writeTVar status' AfterDone)
                  )
          -- as we know tree shaking has been executed prior to running we can assume we
          -- always need to execute once hooks if the status is correct. There is no
          -- possibility of empty subnodes due to tree shaking.
          AroundOnce{path, setup, status, cache, subNodes, teardown} ->
            case hi' of
              EachIn{} -> shouldNeverHappen "AroundOnce"
              ThreadIn _ -> shouldNeverHappen "AroundOnce"
              OnceIn ioHi ->
                do
                  i <- ioHi
                  setUpLocked <- atomically $ tryLock status subNodes canLockSetup SetupRunning
                  eho <-
                    if setUpLocked
                      then do
                        eho <- logRun' (TE.Hook TE.Once TE.SetUp) (setup sink i)
                        atomically $ writeTMVar cache eho
                        eho
                          & either
                            ( \fail' -> do
                                atomically $ writeTVar status AroundAbandoning
                                finally
                                  ( do
                                      abandonChildren lgr fail' subNodes
                                      whenJust teardown
                                        $ const (logAbandonned' (TE.Hook TE.Once TE.TearDown) fail')
                                  )
                                  (atomically $ writeTVar status AroundDone)
                            )
                            (const $ atomically $ writeTVar status AroundQRunning)
                        pure eho
                      else atomically (readTMVar cache)
                  whenRight_
                    eho
                    ( \ho ->
                        finally
                          (runSubNodes_ (ThreadIn . pure $ Right ho) subNodes)
                          ( whenJust teardown
                              $ \td -> do
                                locked <- atomically $ tryLock status subNodes canLockTeardown TeardownRunning
                                when locked
                                  $ finally
                                    (logRun_ (TE.Hook TE.Once TE.TearDown) (td sink ho))
                                    (atomically $ writeTVar status AroundDone)
                          )
                    )
          After{frequency, subNodes', after} ->
            case frequency of
              Each -> case hi' of
                EachIn{after = oldAfter, ..} ->
                  runSubNodes_
                    EachIn
                      { after = oldAfter >> runAfter
                      , ..
                      }
                    subNodes'
                OnceIn ioHi ->
                  runSubNodes_
                    EachIn
                      { setup = Right <$> ioHi
                      , teardown = const $ pure ()
                      , after = runAfter
                      }
                    subNodes'
                ThreadIn ioHi ->
                  runSubNodes_
                    EachIn
                      { setup = ioHi
                      , teardown = const $ pure ()
                      , after = runAfter
                      }
                    subNodes'
               where
                runAfter = logRun_ (TE.Hook TE.Each TE.After) (after sink)
              Thread -> case hi' of
                EachIn{} -> shouldNeverHappen "After Thread"
                OnceIn _ -> runSubNodesAfter
                ThreadIn _ -> runSubNodesAfter
               where
                runSubNodesAfter =
                  do
                    run <- runSubNodes hi' subNodes'
                    when run.hasRun
                      $ logRun_ (TE.Hook TE.Thread TE.After) (after sink)
          Around{frequency, setup, subNodes, teardown = mteardown} ->
            let
              runThreadAround ioHo hoVar =
                finally
                  (runSubNodes_ (ThreadIn ioHo) subNodes)
                  ( do
                      whenJust mteardown
                        $ \td -> do
                          mho <- atomically $ tryReadTMVar hoVar
                          whenJust mho
                            $ either
                              (logAbandonned' (TE.Hook TE.Thread TE.TearDown))
                              (logRun_ (TE.Hook TE.Thread TE.TearDown) . td sink)
                  )
             in
              case frequency of
                Each -> case hi' of
                  EachIn{} -> uu
                  OnceIn{} -> uu
                  ThreadIn ioHi -> uu
                Thread -> case hi' of
                  EachIn{} -> shouldNeverHappen "Around Thread"
                  OnceIn ioHi -> do
                    hoVar <- newEmptyTMVarIO
                    -- I think I need to do this because the thread can't be run until its
                    -- actually needed by a test. There is a possibilty of the hook enclosing
                    -- an empty or saturated subNode list plain old laziness might be enough
                    let ioHo = mkHo hoVar
                    runThreadAround ioHo hoVar
                   where
                    mkHo hov = do
                      mho <- atomically $ tryReadTMVar hov
                      mho
                        & maybe
                          ( do
                              hi'' <- ioHi
                              ho <- logRun' (TE.Hook TE.Thread TE.SetUp) (setup sink hi'')
                              atomically $ putTMVar hov ho
                              pure ho
                          )
                          pure
                  ThreadIn ioeHi -> do
                    hoVar <- newEmptyTMVarIO
                    -- I think I need to do this because the thread can't be run until its
                    -- actually needed by a test. There is a possibilty of the hook enclosing
                    -- an empty or saturated subNode list plain old laziness might be enough
                    -- TODO: test this
                    let ioHo = mkHo hoVar
                    runThreadAround ioHo hoVar
                   where
                    mkHo hov = do
                      mho <- atomically $ tryReadTMVar hov
                      mho
                        & maybe
                          ( do
                              ethi <- ioeHi
                              ho <-
                                either
                                  (pure . Left)
                                  (logRun' (TE.Hook TE.Thread TE.SetUp) . setup sink)
                                  ethi
                              atomically $ putTMVar hov ho
                              pure ho
                          )
                          pure
          Test{path, title, tests} ->
            case hi' of
              EachIn{} -> uu
              OnceIn{} -> uu
              ThreadIn ioHi -> uu

-- >>= either
--   (\ab -> abandonTree lgr ab xt)
--   ( \hi ->
--       let
--         logRun' :: TE.EventType -> IO b -> IO (Either NL.FailPoint b)
--         logRun' = logRun lgr xt.path

--         logAbandonned' = logAbandonnedNew lgr xt.path

--         runSubNodes :: forall hi'. hi' -> ChildQ (ExeTreeNew hi') -> IO QElementRun
--         runSubNodes hi' = runChildQ Concurrent (runNodeNew lgr . pure $ Right hi') canRunXTree

--         sink = lgr . NL.ApEvent
--        in
--         xt & \case
--           AfterOnce
--             { path
--             , status'
--             , subNodes'
--             , after
--             } -> do
--               run <- atomically $ do
--                 s <- readTVar status'
--                 qs <- readTVar subNodes'.status
--                 when (s == AfterQPending)
--                   $ writeTVar status' AfterQRunning
--                 pure $ canRunAfterOnce s qs
--               when run
--                 $ finally
--                   (void $ runSubNodes hi subNodes')
--                   ( do
--                       locked <- atomically $ tryLock status' subNodes' canLockAfterOnce AfterRunning
--                       when locked
--                         $ finally
--                           (void $ logRun' (TE.Hook TE.Once TE.After) (after sink))
--                           (atomically $ writeTVar status' AfterDone)
--                   )
--           AroundOnce{path, setup, status, cache, subNodes, teardown} ->
--             do
--               setUpLocked <- atomically $ tryLock status subNodes canLockSetup SetupRunning
--               eho <-
--                 if setUpLocked
--                   then do
--                     eho <- logRun' (TE.Hook TE.Once TE.SetUp) (setup sink hi)
--                     atomically $ writeTMVar cache eho
--                     eho
--                       & either
--                         ( \AbandonOld -> do
--                             atomically $ writeTVar status AroundAbandoning
--                             finally
--                               ( do
--                                   abandonChildren lgr AbandonOld subNodes
--                                   whenJust teardown
--                                     $ const (logAbandonned' (TE.Hook TE.Once TE.TearDown) AbandonOld)
--                               )
--                               (atomically $ writeTVar status AroundDone)
--                         )
--                         (const $ atomically $ writeTVar status AroundQRunning)
--                     pure eho
--                   else -- waits till populated
--                     atomically (readTMVar cache)
--               whenRight_
--                 eho
--                 ( \ho ->
--                     finally
--                       (void $ runSubNodes ho subNodes)
--                       ( whenJust teardown
--                           $ \td -> do
--                             locked <- atomically $ tryLock status subNodes canLockTeardown TeardownRunning
--                             when locked
--                               $ finally
--                                 (void $ logRun' (TE.Hook TE.Once TE.TearDown) (td sink ho))
--                                 (atomically $ writeTVar status AroundDone)
--                       )
--                 )
--           After{path, frequency, subNodes', after} ->
--             case frequency of
--               Each -> uu
--               Thread -> uu
--           Around{path, frequency, setup, subNodes, teardown} -> uu
--             case frequency of
--               Each -> uu
--               Thread -> uu
--           Test{path, title, tests} -> uu
-- )

data NodeIn hi
  = Abandon FailPoint
  | OnceIn (IO hi)
  | ThreadIn (IO (Either FailPoint hi))
  | EachIn
      { -- get rid of one param , how to compose
        setup :: IO (Either FailPoint hi) -- pure ()
      , teardown :: hi -> IO () -- const $ pure ()
      , after :: IO () -- pure ()
      }

{-
 1. get runtest working
 2. genrate testin including logging start finish for functions
 3.  IO (Either NL.FailPoint hi) => NodeIn  = AbandonOld NL.FailPoint | HookIn (IO hi) | TestIn hi

runTest :: Logger -> TestIn hi -> P.TestItem IO hi -> IO ()
runTest lgr TestIn{setup, teardown, after} P.TestItem{id, title, action} =
  uu
 where
  sink = lgr . NL.ApEvent
-}

-- runNodesNew :: Logger -> ChildQ (ExeTreeNew hi) -> IO ()
-- runNodesNew logger cq =
--   void $ runChildQ Concurrent (runNode eventLogger tstIn) nodeStatus subNodes
--  where
--   --  runChildQ Concurrent (runner . pure $ Right ()) canRun cq

--   runner :: forall hi. IO (Either NL.FailPoint hi) -> ExeTreeNew hi -> IO ()
--   runner ehIn xtr =
--     do
--       ehi <- ehIn
--       ehi
--         & either
--           (\abd -> abandonChildren logger xtr)
--           uu

tryLock :: TVar s -> ChildQ a -> (s -> CanRun -> Bool) -> s -> STM Bool
tryLock hs cq canLock lockStatus =
  do
    s <- readTVar hs
    qs <- readTVar cq.status
    let cl = canLock s qs
    when cl
      $ writeTVar hs lockStatus
    pure cl

-- readOrLockHook :: TVar HookStatus -> TMVar (Either AbandonOld ho) -> STM (Maybe (Either AbandonOld ho))
-- readOrLockHook hs hVal =
--   do
--     s <- readTVar hs
--     (==) s HookPending
--       ? (writeTVar hs HookRunning >> pure Nothing)
--       -- blocks until the other thread has written the MVar
--       $ (Just <$> readTMVar hVal)

logRun :: Logger -> NL.ExePath -> TE.EventType -> IO b -> IO (Either NL.FailPoint b)
logRun lgr path evt action = do
  lgr $ NL.Start evt path
  finally
    ( catchAll
        -- TODO :: test for strictness issues esp with failing thread hook
        -- eg returns handle and handle is closed
        (Right <$> action)
        (logReturnFailureNew lgr path evt)
    )
    (lgr $ NL.End evt path)

setHookCompleteNew :: TVar HookStatus -> TMVar (Either AbandonOld ho) -> Either AbandonOld ho -> STM (Either AbandonOld ho)
setHookCompleteNew hs hVal eso = do
  putTMVar hVal eso
  writeTVar hs HookComplete
  pure eso

runLogHookNew :: forall hi ho a. XContext a -> ExeEventType -> (Context a -> hi -> IO ho) -> hi -> IO (Either AbandonOld ho)
runLogHookNew ctx@XContext{loc} hkEvent hook hi =
  withStartEnd ctx hkEvent
    $ catchAll
      (Right <$> hook (mkCtx ctx) hi)
      (logReturnFailure ctx hkEvent)

runNode ::
  forall oi ti a.
  (L.ExeEvent L.Loc a -> IO ()) ->
  Either AbandonOld (ExeIn oi ti ()) ->
  ExeTree a oi ti ->
  IO ()
runNode eventLogger hkIn =
  \case
    XGroup
      { loc
      , threadLimit
      , onceHook
      , threadHook
      , subNodes
      } ->
        do
          let ctx = XContext loc eventLogger
          canRun' <- atomically $ canRunChildQ subNodes
          when canRun'
            $ withOnceHook ctx threadLimit subNodes hkIn onceHook
            $ \trdIn ->
              withThreadHook ctx trdIn threadHook $ \tstIn ->
                void $ runChildQ Concurrent (runNode eventLogger tstIn) nodeStatus subNodes
    XFixtures
      { loc
      , threadLimit = tl@ThreadLimit{maxThreads, runningThreads}
      , fixtures
      , testHook
      } ->
        do
          canRun' <- atomically xfxRunnable
          when canRun'
            $ finally
              (void $ runChildQ Concurrent runFixture canRunFixture fixtures)
              (atomically $ modifyTVar' runningThreads pred)
       where
        canRunFixture XFixture{tests} = readTVar tests.status
        runFixture = runXFixture eventLogger hkIn testHook
        xfxRunnable = do
          t <- withinThreadLimit tl
          q <- canRunChildQ fixtures
          let r = t && q
          when r $ modifyTVar' runningThreads succ
          pure r

runTree :: (L.ExeLog L.Loc a -> IO ()) -> ExeTree a () () -> Int -> IO ()
runTree sink xtri maxThreads =
  do
    rootLogger <- newLogger
    finally
      ( rootLogger StartExecution
          >> forConcurrently_
            thrdTokens
            ( const do
                logger' <- newLogger
                runNode logger' hkIn xtri
            )
      )
      (waitDone xtri >> rootLogger EndExecution)
 where
  hkIn = Right (ExeIn () () ())
  thrdTokens = replicate maxThreads True
  newLogger = mkLogger sink <$> UnliftIO.newIORef (-1) <*> myThreadId

execute :: Int -> LogControls m L.Loc a -> PN.PreNodeRoot a -> IO ()
execute
  maxThreads
  LogControls
    { sink
    , logWorker
    , stopWorker
    }
  preRoot =
    -- TODO - Validatte prenode
    -- fixture titles are unique ??
    concurrently_ logWorker linkExecute
   where
    linkExecute :: IO ()
    linkExecute =
      finally
        ( do
            exeTree <- prepare preRoot
            runTree sink exeTree maxThreads
        )
        stopWorker
