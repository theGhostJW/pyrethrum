--  should be able to remove this in later versions of GHC
-- https://gitlab.haskell.org/ghc/ghc/-/issues/21443
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Internal.SuiteRuntime where

import BasePrelude (retry)
import GHC.Exts
import Internal.PreNode (Context (..), OnceHook (..), PreNode (testHook), TestHook (..), ThreadHook (..))
import qualified Internal.PreNode as PN (
  Fixture (..),
  PreNode (..),
  PreNodeRoot,
  Test (..),
 )
import Internal.RunTimeLogging (
  EventSink,
  ExeEvent (..),
  ExeEventType (TestHook, TestHookRelease, ThreadHookRelease),
  Loc (Node, Root),
  LogControls (LogControls),
  SThreadId,
  logWorker,
  mkFailure,
  mkLogger,
  mkParentFailure,
  sink,
  stopWorker,
 )

import qualified Internal.RunTimeLogging as L

import PyrethrumExtras (catchAll, txt, (?))
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

data HookStatus
  = HookVoid
  | HookPending
  | HookRunning
  | HookComplete
  | HookReleaseRunning
  | HookReleased
  deriving (Show, Eq)

data XContext = XContext
  { loc :: Loc
  , evtLogger :: EventLogger
  }

type EventLogger = (Int -> SThreadId -> ExeEvent Loc) -> IO ()

mkCtx :: XContext -> Context
mkCtx XContext{loc, evtLogger} =
  let
    msgLogger :: Text -> IO ()
    msgLogger msg = evtLogger $ \idx trdId ->
      ApLog
        { idx = idx
        , threadId = trdId
        , msg = msg
        }
   in
    Context loc msgLogger

waitDone :: ExeTree oi ti -> IO ()
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

data XTest si ti ii = XTest
  { loc :: Loc
  , test :: XContext -> si -> ti -> ii -> IO ()
  }

runTestHook :: forall so' to' tsti' ho'. XContext -> Either Abandon (ExeIn so' to' tsti') -> TestHook so' to' tsti' ho' -> IO (Either Abandon (ExeIn so' to' ho'))
runTestHook ctx@XContext{loc = testLoc} tstIn testHk =
  tstIn
    & either
      logReturnAbandonned
      runHook
 where
  tstCtx :: XContext
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

mkTestChildLoc :: (Show a) => Loc -> a -> Loc
mkTestChildLoc testLoc evt = Node testLoc $ txt evt

releaseTestHook :: forall so' to' tsti' ho'. XContext -> Either Abandon ho' -> TestHook so' to' tsti' ho' -> IO ()
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

runChildQ :: forall a. Bool -> (a -> IO ()) -> (a -> STM CanRun) -> ChildQ a -> IO CanRun
runChildQ childConcurrent runner canRun' q@ChildQ{childNodes, status, runningCount} =
  do
    eNext <- atomically $ do
      let pop = tryReadTQueue childNodes
      rc <- readTVar runningCount
      mNext <- pop
      mNext
        & maybe
          ( do
              if
                  | rc < 0 -> error "framework error - this should not happen - running count below zero"
                  | rc == 0 -> writeTVar status Done
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
                    when childConcurrent $
                      atomically (writeTQueue childNodes a)
                    runner a
                  Saturated -> pure ()
                  Done -> pure ()
              (atomically $ modifyTVar runningCount pred)
            runChildQ childConcurrent runner canRun' q
        )

mkChildQ :: Foldable m => m a -> IO (ChildQ a)
mkChildQ children = do
  s <- newTVarIO Runnable
  q <- newTQueueIO
  rc <- newTVarIO 0
  atomically $ traverse_ (writeTQueue q) children
  pure $
    ChildQ
      { status = s
      , childNodes = q
      , runningCount = rc
      }

data OnceVal oi oo = OnceVal
  { hook :: OnceHook oi oo
  , status :: TVar HookStatus
  , value :: TMVar (Either Abandon oo)
  }

mkOnceVal :: OnceHook oi oo -> IO (OnceVal oi oo)
mkOnceVal h = OnceVal h <$> newTVarIO HookPending <*> newEmptyTMVarIO

data XFixture oi ti tsti where
  XFixture ::
    { loc :: Loc
    , onceHook :: OnceVal oi oo
    , threadHook :: ThreadHook oo ti to
    , testHook :: TestHook oo to tsti tsto
    , tests :: ChildQ (XTest oo to tsto)
    , threadLimit :: ThreadLimit
    } ->
    XFixture oi ti tsti

withOnceHook :: XContext -> ThreadLimit -> ChildQ a -> Either Abandon (ExeIn si ti tsti) -> OnceVal si so -> (Either Abandon (ExeIn so ti tsti) -> IO ()) -> IO ()
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

withThreadHook :: XContext -> Either Abandon (ExeIn oi ti tsti) -> ThreadHook oi ti to -> (Either Abandon (ExeIn oi to tsti) -> IO ()) -> IO ()
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

runXFixture :: forall oi ti ii. EventLogger -> Either Abandon (ExeIn oi ti ()) -> TestHook oi ti () ii -> XFixture oi ti ii -> IO ()
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
      when canRun' $
        withOnceHook ctx threadLimit tests exin onceHook $ \trdIn ->
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
    runTests fxIn = runChildQ False (runTst fxIn) (const $ pure Runnable) tests

runXTest :: forall so' to' tsti' ho'. XContext -> Either Abandon (ExeIn so' to' tsti') -> TestHook so' to' tsti' ho' -> XTest so' to' ho' -> IO ()
runXTest ctx@XContext{loc = fxLoc} fxIpts testHk test@XTest{loc = tstLoc, test = tstAction} =
  do
    let tstCtx = ctx{loc = tstLoc} :: XContext
    eho <- runTestHook tstCtx fxIpts testHk
    eho
      & either
        (logAbandonned tstCtx L.Test)
        ( \(ExeIn oi ti tsti') ->
            finally
              ( withStartEnd ctx L.Test $
                  catchAll
                    (tstAction tstCtx oi ti tsti')
                    (void . logReturnFailure ctx L.Test)
              )
              (releaseTestHook tstCtx ((.tstIn) <$> eho) testHk)
        )

mkThreadLimit :: Maybe Int -> IO ThreadLimit
mkThreadLimit mi = ThreadLimit mi <$> newTVarIO 0

mkXFixture ::
  Loc ->
  PN.Fixture oi ti tsti ->
  IO (XFixture oi ti tsti)
mkXFixture loc PN.Fixture{onceHook, threadHook, testHook, tests, maxThreads, title} = do
  oh <- mkOnceVal onceHook
  ts <- mkChildQ $ mkXTest <$> tests
  threadLimit <- mkThreadLimit maxThreads
  pure $
    XFixture
      { loc = fxLoc
      , onceHook = oh
      , threadHook
      , testHook
      , tests = ts
      , threadLimit
      }
 where
  fxLoc = Node loc title
  mkXTest PN.Test{id, test} =
    XTest
      { loc = Node fxLoc $ "Test :: " <> id
      , test = test . mkCtx
      }

data ExeTree oi ti where
  XGroup ::
    { loc :: Loc
    , threadLimit :: ThreadLimit
    , onceHook :: OnceVal oi oo
    , threadHook :: ThreadHook oo ti to
    , subNodes :: ChildQ (ExeTree oo to)
    } ->
    ExeTree oi ti
  XFixtures ::
    { loc :: Loc
    , threadLimit :: ThreadLimit
    , testHook :: TestHook oi ti () tsto
    , fixtures :: ChildQ (XFixture oi ti tsto)
    } ->
    ExeTree oi ti

prepare :: PreNode o t -> IO (ExeTree o t)
prepare =
  prepare' Root 0
 where
  prepare' :: Loc -> Int -> PN.PreNode oi ti -> IO (ExeTree oi ti)
  prepare' parentLoc subElmIdx pn = do
    let nodeLoc = Node parentLoc
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
          pure $
            XGroup
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
            pure $
              XFixtures
                { loc
                , threadLimit
                , fixtures
                , testHook
                }

logAbandonned :: XContext -> ExeEventType -> Abandon -> IO ()
logAbandonned XContext{loc, evtLogger} fet Abandon{sourceLoc, sourceEventType, exception} =
  evtLogger (mkParentFailure loc fet sourceLoc sourceEventType exception)

logReturnFailure :: XContext -> ExeEventType -> SomeException -> IO (Either Abandon a)
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

runLogHook :: forall hi ho. XContext -> ExeEventType -> (Context -> hi -> IO ho) -> hi -> IO (Either Abandon ho)
runLogHook ctx@XContext{loc} hkEvent hook hi =
  withStartEnd ctx hkEvent $
    catchAll
      (Right <$> hook (mkCtx ctx) hi)
      (logReturnFailure ctx hkEvent)

withStartEnd :: XContext -> ExeEventType -> IO a -> IO a
withStartEnd XContext{loc, evtLogger} evt io = do
  evtLogger $ Start evt loc
  finally io . evtLogger $ End evt loc

abandonLogHook :: XContext -> ExeEventType -> Abandon -> IO (Either Abandon a)
abandonLogHook ctx evtTp abandon =
  do
    withStartEnd ctx evtTp $
      logAbandonned ctx evtTp abandon
    pure $ Left abandon

abandonnedOnceHookVal :: forall ho. XContext -> Abandon -> TVar HookStatus -> TMVar (Either Abandon ho) -> IO (Either Abandon ho)
abandonnedOnceHookVal ctx abandon hs hkVal =
  tryLockRun
    hs
    hkVal
    ( do
        abandonLogHook ctx L.OnceHook abandon
        atomically $ writeTVar hs HookComplete
        pure $ Left abandon
    )

threadHookVal :: forall oi ti tsti to. XContext -> Either Abandon (ExeIn oi ti tsti) -> ThreadHook oi ti to -> IO (Either Abandon to)
threadHookVal ctx hkIn thook =
  hkIn
    & either
      (abandonLogHook ctx L.ThreadHook)
      ( \(ExeIn oi ti _tsti) ->
          let
            thrdHk :: (Context -> oi -> ti -> IO to) -> Context -> ti -> IO to
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

onceHookVal :: forall hi ho. XContext -> Either Abandon hi -> OnceVal hi ho -> IO (Either Abandon ho)
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
                    >>= atomically . setHookComplete status value
                )
           in
            oHook & \case
              OnceNone -> passThrough
              OnceAfter{} -> passThrough
              OnceBefore{hook} -> runHk hook
              OnceAround{hook} -> runHk hook
      )

releaseHook :: XContext -> ExeEventType -> Either Abandon ho -> (Context -> ho -> IO ()) -> IO ()
releaseHook ctx@XContext{evtLogger, loc} evt eho hkRelease =
  withStartEnd ctx evt $
    eho
      & either
        (logAbandonned ctx evt)
        ( \so ->
            catchAll
              (hkRelease (mkCtx ctx) so)
              (evtLogger . mkFailure loc evt ("Hook Release Failed: " <> txt evt <> " " <> txt loc))
        )

releaseOnceHookIfReady ::
  XContext ->
  TVar Int ->
  ChildQ a ->
  Either Abandon ho ->
  OnceVal oi ho ->
  IO ()
releaseOnceHookIfReady ctx cntr childq eho hk =
  do
    locked <- atomically tryLock
    when locked $
      finally
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
            nxt == 0
              && canRelease hks
              && s == Done
      when r $
        writeTVar hk.status HookReleaseRunning
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
  { sourceLoc :: Loc
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

runNode :: forall oi ti. EventLogger -> Either Abandon (ExeIn oi ti ()) -> ExeTree oi ti -> IO ()
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
          when canRun' $
            withOnceHook ctx threadLimit subNodes hkIn onceHook $ \trdIn ->
              withThreadHook ctx trdIn threadHook $ \tstIn ->
                void $ runChildQ True (runNode eventLogger tstIn) nodeStatus subNodes
    XFixtures
      { loc
      , threadLimit = tl@ThreadLimit{maxThreads, runningThreads}
      , fixtures
      , testHook
      } ->
        do
          canRun' <- atomically xfxRunnable
          when canRun' $
            finally
              (void $ runChildQ True runFixture canRunFixture fixtures)
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

newLogger :: EventSink Loc -> ThreadId -> IO EventLogger
newLogger sink tid =
  do
    ir <- UnliftIO.newIORef (-1)
    pure $ mkLogger sink ir tid

executeGraph :: EventSink Loc -> ExeTree () () -> Int -> IO ()
executeGraph sink xtri maxThreads =
  let hkIn = Right (ExeIn () () ())
      thrdTokens = replicate maxThreads True
   in do
        rootThreadId <- myThreadId
        rootLogger <- newLogger sink rootThreadId
        finally
          ( rootLogger StartExecution
              >> forConcurrently_
                thrdTokens
                ( const do
                    tid' <- myThreadId
                    logger' <- newLogger sink tid'
                    runNode logger' hkIn xtri
                )
          )
          (waitDone xtri >> rootLogger EndExecution)

execute :: Int -> LogControls m Loc -> PN.PreNodeRoot -> IO ()
execute
  maxThreads
  LogControls
    { sink
    , logWorker
    , stopWorker
    }
  preRoot =
    concurrently_ logWorker linkExecute
   where
    linkExecute :: IO ()
    linkExecute =
      finally
        ( do
            exeTree <- prepare preRoot
            executeGraph sink exeTree maxThreads
        )
        stopWorker