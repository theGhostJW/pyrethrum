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
import Prelude hiding (atomically, id, newEmptyTMVarIO, newTVarIO)

-- PRENODE TO BE DEPRECATED
import Internal.PreNode (Context (..), OnceHook (..), PreNode (testHook), TestHook (..), ThreadHook (..))
import qualified Internal.PreNode as PN (
  Fixture (..),
  PreNode (..),
  PreNodeRoot,
  Test (..),
 )

-- NEW

import GHC.RTS.Flags (DebugFlags (interpreter))
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
    -- TODO - Validatte prenode
    -- fixture titles are unique ??
    concurrently_ logWorker linkExecute
   where
    linkExecute :: IO ()
    linkExecute =
      finally
        ( do
            engSnk <- NL.mkLogger sink <$> UnliftIO.newIORef (-1) <*> myThreadId
            let eventSink = engSnk . NL.ApEvent
                nodeList = P.prepare $ C.SuitePrepParams suite eventSink interpreter runConfig
            xtree <- mkXTreeNew (NL.ExePath []) nodeList
            executeNodesNew engSnk xtree maxThreads

            -- runTree sink exeTree maxThreads
        )
        stopWorker

executeNodesNew :: (NL.EngineEvent NL.ExePath a -> IO ()) -> ChildQ (ExeTreeNew ()) -> Int -> IO ()
executeNodesNew sink nodes maxThreads =
  do
    rootLogger <- newLogger
    finally
      ( rootLogger StartExecution
          >> forConcurrently_
            thrdTokens
            ( const do
                logger' <- newLogger
                uu -- runNode logger' hkIn xtri
            )
      )
      uu -- (waitDone xtri >> rootLogger EndExecution)
 where
  hkIn = Right (ExeIn () () ())
  thrdTokens = replicate maxThreads True
  newLogger = uu -- mkLogger sink <$> UnliftIO.newIORef (-1) <*> myThreadId

data NFrequency = Each | Thread
  deriving (Show, Eq)

data ExeTreeNew hi where
  AfterOnce ::
    { path :: NL.ExePath
    , status :: TVar HookStatus
    , subNodes' :: ChildQ (ExeTreeNew hi)
    , after :: IO ()
    } ->
    ExeTreeNew hi
  AroundOnce ::
    { path :: NL.ExePath
    , setup :: hi -> IO ho
    , status :: TVar HookStatus
    , cache :: TMVar (Either Abandon ho)
    , subNodes :: ChildQ (ExeTreeNew ho)
    , teardown :: Maybe (ho -> IO ())
    } ->
    ExeTreeNew hi
  After ::
    { path :: NL.ExePath
    , frequency :: NFrequency
    , subNodes' :: ChildQ (ExeTreeNew hi)
    , after :: IO ()
    } ->
    ExeTreeNew hi
  Around ::
    { path :: NL.ExePath
    , frequency :: NFrequency
    , setup :: hi -> IO o
    , subNodes :: ChildQ (ExeTreeNew ho)
    , teardown :: Maybe (ho -> IO ())
    } ->
    ExeTreeNew hi
  Test ::
    { path :: NL.ExePath
    , title :: Text
    , threadLimit :: Int
    , tests :: ChildQ (hi -> IO ())
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
            C.Once -> mkOnceHook subNodes action Nothing
            C.Thread -> mkNHook' Thread
            C.Each -> mkNHook' Each
         where
          mkNHook' = mkNHook subNodes action Nothing
      P.After{frequency, subNodes', after} ->
        do
          cq <- mkXTreeNew path subNodes'
          let mkAfter fq = pure $ After{path, frequency = fq, subNodes' = cq, after}
          frequency & \case
            C.Once -> do
              status <- newTVarIO HookPending
              pure $ AfterOnce{path, status, subNodes' = cq, after}
            C.Thread -> mkAfter Thread
            C.Each -> mkAfter Each
      P.Around
        { frequency
        , setup
        , subNodes
        , teardown
        } ->
          frequency & \case
            C.Once -> mkOnceHook subNodes setup teardown'
            C.Thread -> nHook' Thread
            C.Each -> nHook' Each
         where
          teardown' = Just teardown
          nHook' = mkNHook subNodes setup teardown'
      P.Test
        { config = c
        , tests
        } -> do
          cq <- mkChildQ tests
          pure $ Test{path, title = c.title, threadLimit = c.maxThreads, tests = cq}
   where
    path = NL.ExePath $ pn.path : xpth.unExePath

    mkOnceHook :: forall hi' ho'. NonEmpty (P.PreNode IO NonEmpty ho') -> (hi' -> IO ho') -> Maybe (ho' -> IO ()) -> IO (ExeTreeNew hi')
    mkOnceHook subNodes setup teardown = do
      s <- newTVarIO HookPending
      cache <- newEmptyTMVarIO
      cq <- mkXTreeNew path subNodes
      pure
        $ AroundOnce
          { path
          , setup
          , status = s
          , cache
          , subNodes = cq
          , teardown
          }

    mkNHook :: forall hi' ho'. NonEmpty (P.PreNode IO NonEmpty ho') -> (hi' -> IO ho') -> Maybe (ho' -> IO ()) -> NFrequency -> IO (ExeTreeNew hi')
    mkNHook subNodes setup teardown frequency = do
      s <- newTVarIO HookPending
      cache <- newEmptyTMVarIO
      cq <- mkXTreeNew path subNodes
      pure
        $ Around
          { path
          , setup
          , frequency
          , subNodes = cq
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

runTestHook :: forall so' to' tsti' ho' a. XContext a -> Either Abandon (ExeIn so' to' tsti') -> TestHook a so' to' tsti' ho' -> IO (Either Abandon (ExeIn so' to' ho'))
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
          voidHk = pure @IO $ Right @Abandon (ExeIn oi ti tsti)
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

releaseTestHook :: forall so' to' tsti' ho' a. XContext a -> Either Abandon ho' -> TestHook a so' to' tsti' ho' -> IO ()
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

runChildQ :: forall a. Concurrency -> (a -> IO ()) -> (a -> STM CanRun) -> ChildQ a -> IO CanRun
runChildQ concurrency runner canRun' q@ChildQ{childNodes, status, runningCount} =
  do
    eNext <- atomically $ do
      let pop = tryReadTQueue childNodes
      rc <- readTVar runningCount
      mNext <- pop
      mNext
        & maybe
          ( do
              if
                -- broken
                | rc < 0 -> bug @Void $ error "framework error - this should not happen - running count below zero"
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
        (atomically $ readTVar status)
        ( \a -> do
            finally
              do
                cr <- atomically $ canRun' a
                cr & \case
                  -- runner MUST ensure the integrity of sub element status and handle all exceptions
                  Runnable -> do
                    -- when childConcurrent, the element is placed back on the end of the q before running so
                    -- can be picked up by other threads
                    when (concurrency == Concurrent)
                      $ atomically (writeTQueue childNodes a)
                    runner a
                  Saturated -> pure ()
                  Done -> pure ()
              (atomically $ modifyTVar runningCount pred)
            runChildQ concurrency runner canRun' q
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
  , value :: TMVar (Either Abandon oo)
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

withOnceHook :: XContext a -> ThreadLimit -> ChildQ b -> Either Abandon (ExeIn si ti tsti) -> OnceVal a si so -> (Either Abandon (ExeIn so ti tsti) -> IO ()) -> IO ()
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

withThreadHook :: XContext a -> Either Abandon (ExeIn oi ti tsti) -> ThreadHook a oi ti to -> (Either Abandon (ExeIn oi to tsti) -> IO ()) -> IO ()
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
  Either Abandon (ExeIn oi ti ()) ->
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

runXTest :: forall so' to' tsti' ho' a. XContext a -> Either Abandon (ExeIn so' to' tsti') -> TestHook a so' to' tsti' ho' -> XTest a so' to' ho' -> IO ()
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

logAbandonned :: XContext a -> ExeEventType -> Abandon -> IO ()
logAbandonned XContext{loc, evtLogger} fet Abandon{sourceLoc, sourceEventType, exception} =
  evtLogger (mkParentFailure loc fet sourceLoc sourceEventType exception)

logReturnFailure :: XContext a -> ExeEventType -> SomeException -> IO (Either Abandon b)
logReturnFailure XContext{loc, evtLogger} et e =
  do
    evtLogger (mkFailure loc et (txt et <> "Failed at: " <> txt loc) e)
    pure $ Left $ Abandon loc et e

readOrLockHook :: TVar HookStatus -> TMVar (Either Abandon ho) -> STM (Maybe (Either Abandon ho))
readOrLockHook hs hVal =
  do
    s <- readTVar hs
    (==) s HookPending
      ? (writeTVar hs HookRunning >> pure Nothing)
      $ (Just <$> readTMVar hVal)

setHookComplete :: TVar HookStatus -> TMVar (Either Abandon ho) -> Either Abandon ho -> STM (Either Abandon ho)
setHookComplete hs hVal eso = do
  putTMVar hVal eso
  writeTVar hs HookComplete
  pure eso

tryLockRun :: TVar HookStatus -> TMVar (Either Abandon ho) -> IO (Either Abandon ho) -> IO (Either Abandon ho)
tryLockRun hkStatus hkVal hkAction =
  atomically (readOrLockHook hkStatus hkVal)
    >>= maybe
      hkAction
      pure

runLogHook :: forall hi ho a. XContext a -> ExeEventType -> (Context a -> hi -> IO ho) -> hi -> IO (Either Abandon ho)
runLogHook ctx@XContext{loc} hkEvent hook hi =
  withStartEnd ctx hkEvent
    $ catchAll
      (Right <$> hook (mkCtx ctx) hi)
      (logReturnFailure ctx hkEvent)

withStartEnd :: XContext a -> ExeEventType -> IO b -> IO b
withStartEnd XContext{loc, evtLogger} evt io = do
  evtLogger $ Start evt loc
  finally io . evtLogger $ End evt loc

abandonLogHook :: XContext a -> ExeEventType -> Abandon -> IO (Either Abandon b)
abandonLogHook ctx evtTp abandon =
  do
    withStartEnd ctx evtTp
      $ logAbandonned ctx evtTp abandon
    pure $ Left abandon

abandonnedOnceHookVal :: forall ho a. XContext a -> Abandon -> TVar HookStatus -> TMVar (Either Abandon ho) -> IO (Either Abandon ho)
abandonnedOnceHookVal ctx abandon hs hkVal =
  tryLockRun
    hs
    hkVal
    ( do
        abandonLogHook ctx L.OnceHook abandon
        atomically $ writeTVar hs HookComplete
        pure $ Left abandon
    )

threadHookVal :: forall oi ti tsti to a. XContext a -> Either Abandon (ExeIn oi ti tsti) -> ThreadHook a oi ti to -> IO (Either Abandon to)
threadHookVal ctx hkIn thook =
  hkIn
    & either
      (abandonLogHook ctx L.ThreadHook)
      ( \(ExeIn oi ti _tsti) ->
          let
            thrdHk :: (Context a -> oi -> ti -> IO to) -> Context a -> ti -> IO to
            thrdHk thHk = flip thHk oi
            runHook hook' = runLogHook ctx L.ThreadHook (thrdHk hook') ti
            passThrough = pure @IO $ Right @Abandon ti
           in
            thook & \case
              ThreadNone -> passThrough
              ThreadBefore{hook} -> runHook hook
              ThreadAfter{} -> passThrough
              ThreadAround{hook} -> runHook hook
      )

onceHookVal :: forall hi ho a. XContext a -> Either Abandon hi -> OnceVal a hi ho -> IO (Either Abandon ho)
onceHookVal ctx ehi OnceVal{hook = oHook, status, value} =
  ehi
    & either
      (\abandon -> abandonnedOnceHookVal ctx abandon status value)
      ( \hi' ->
          let
            passThrough = pure @IO $ Right @Abandon hi'
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

releaseHook :: XContext a -> ExeEventType -> Either Abandon ho -> (Context a -> ho -> IO ()) -> IO ()
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
  Either Abandon ho ->
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

data Abandon = Abandon
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

runNode ::
  forall oi ti a.
  (L.ExeEvent L.Loc a -> IO ()) ->
  Either Abandon (ExeIn oi ti ()) ->
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
