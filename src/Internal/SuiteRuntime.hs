module Internal.SuiteRuntime where

import Core qualified as C
import DSL.Internal.ApEvent qualified as AE
import Internal.RunTimeLogging (FailPoint)
import Internal.RunTimeLogging qualified as L
import Internal.ThreadEvent hiding (Test)
import Internal.ThreadEvent qualified as TE
import Prepare qualified as P
import PyrethrumExtras (catchAll, txt, (?))
import UnliftIO (
  concurrently_,
  finally,
  forConcurrently_,
  newIORef,
  writeTMVar,
 )
import UnliftIO.Concurrent (myThreadId)
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
import Prelude hiding (All, atomically, id, newEmptyTMVarIO, newTVarIO, readMVar)

{-
todo :: define defect properties with sum type type and typeclass which returns defect info

-}

newtype ThreadCount = ThreadCount {maxThreads :: Int}
  deriving (Show)

execute :: (C.Config rc, C.Config tc) => ThreadCount -> L.LogControls L.ExePath AE.ApEvent -> C.ExeParams m rc tc -> IO ()
execute
  tc
  lc
  C.ExeParams
    { suite
    , interpreter
    , runConfig
    } = executeNodeList tc lc (P.prepare $ P.SuitePrepParams suite interpreter runConfig)

executeNodeList :: ThreadCount -> L.LogControls L.ExePath AE.ApEvent -> [P.PreNode IO ()] -> IO ()
executeNodeList
  tc
  L.LogControls
    { sink
    , logWorker
    , stopWorker
    }
  nodeList =
    do
      xtree <- mkXTree (L.ExePath []) nodeList
      -- logWorker and execution run concurrently
      -- logworker serialises the log events emitted by the execution
      concurrently_
        logWorker
        ( finally
            (executeNodes sink xtree tc)
            stopWorker
        )

executeNodes :: (ThreadEvent L.ExePath AE.ApEvent -> IO ()) -> ChildQ (ExeTree ()) -> ThreadCount -> IO ()
executeNodes sink nodes tc =
  do
    rootLogger <- newLogger
    finally
      ( rootLogger L.StartExecution
          >> forConcurrently_
            thrdTokens
            ( const do
                logger <- newLogger
                runChildQ Concurrent (runNode logger $ OnceIn ()) canRunXTree nodes
            )
      )
      (rootLogger L.EndExecution)
 where
  thrdTokens = replicate tc.maxThreads True
  newLogger = L.mkLogger sink <$> UnliftIO.newIORef (-1) <*> myThreadId

data ExeTree hi where
  OnceBefore ::
    { path :: L.ExePath
    , before :: P.ApEventSink -> hi -> IO ho
    , beforeStatus :: TVar BeforeStatus
    , cache :: TMVar (Either L.FailPoint ho)
    , subNodes :: ChildQ (ExeTree ho)
    } ->
    ExeTree hi
  OnceAround ::
    { path :: L.ExePath
    , setup :: P.ApEventSink -> hi -> IO ho
    , status :: TVar AroundStatus
    , cache :: TMVar (Either L.FailPoint ho)
    , subNodes :: ChildQ (ExeTree ho)
    , teardown :: P.ApEventSink -> ho -> IO ()
    } ->
    ExeTree hi
  OnceAfter ::
    { path :: L.ExePath
    , status' :: TVar AfterStatus
    , subNodes' :: ChildQ (ExeTree hi)
    , after :: P.ApEventSink -> IO ()
    } ->
    ExeTree hi
  ThreadBefore ::
    { path :: L.ExePath
    , before :: P.ApEventSink -> hi -> IO ho
    , subNodes :: ChildQ (ExeTree ho)
    } ->
    ExeTree hi
  ThreadAround ::
    { path :: L.ExePath
    , setup :: P.ApEventSink -> hi -> IO ho
    , subNodes :: ChildQ (ExeTree ho)
    , teardown :: P.ApEventSink -> ho -> IO ()
    } ->
    ExeTree hi
  ThreadAfter ::
    { path :: L.ExePath
    , subNodes' :: ChildQ (ExeTree hi)
    , after :: P.ApEventSink -> IO ()
    } ->
    ExeTree hi
  EachBefore ::
    { path :: L.ExePath
    , before :: P.ApEventSink -> hi -> IO ho
    , subNodes :: ChildQ (ExeTree ho)
    } ->
    ExeTree hi
  EachAround ::
    { path :: L.ExePath
    , setup :: P.ApEventSink -> hi -> IO ho
    , subNodes :: ChildQ (ExeTree ho)
    , teardown :: P.ApEventSink -> ho -> IO ()
    } ->
    ExeTree hi
  EachAfter ::
    { path :: L.ExePath
    , subNodes' :: ChildQ (ExeTree hi)
    , after :: P.ApEventSink -> IO ()
    } ->
    ExeTree hi
  Fixture ::
    { path :: L.ExePath
    , title :: Text
    , tests :: TestSource hi
    } ->
    ExeTree hi

data TestSource hi = Queue (ChildQ (P.Test IO hi) )| PropertyTest hi

loadTests ::  C.DataSource (P.Test IO hi) -> IO (TestSource hi)
loadTests = \case 
  C.ItemList tests -> Queue <$> mkChildQ tests
  C.Property _i -> noImpPropertyError


mkXTree :: L.ExePath -> [P.PreNode IO hi ]-> IO (ChildQ (ExeTree hi))
mkXTree xpth preNodes =
  do
    subTrees <- traverse mkNode preNodes
    mkChildQ subTrees
 where
  mkNode :: forall hi. P.PreNode IO hi -> IO (ExeTree hi)
  mkNode pn =
    pn & \case
      P.Before
        { frequency
        , action
        , subNodes
        } ->
          do
            childTree <- mkXTree path subNodes
            frequency & \case
              Once -> do
                cache <- newEmptyTMVarIO
                status <- newTVarIO BeforePending
                pure $ OnceBefore path action status cache childTree
              Thread -> pure $ ThreadBefore path action childTree
              Each -> pure $ EachBefore path action childTree
      --
      P.After{frequency, subNodes', after} ->
        do
          childTree <- mkXTree path subNodes'
          -- let mkAfter fq = pure $ After{path, frequency = fq, subNodes' = cq, after}
          frequency & \case
            Once -> do
              status <- newTVarIO AfterQPending
              pure $ OnceAfter path status childTree after
            Thread -> pure $ ThreadAfter path childTree after
            TE.Each -> pure $ EachAfter path childTree after
      --
      P.Around
        { frequency
        , setup
        , subNodes
        , teardown
        } ->
          do
            childTree <- mkXTree path subNodes
            frequency & \case
              TE.Once -> do
                status <- newTVarIO SetupPending
                cache <- newEmptyTMVarIO
                pure $
                  OnceAround
                    { path
                    , setup
                    , status
                    , cache
                    , subNodes = childTree
                    , teardown
                    }
              TE.Thread ->
                pure $ ThreadAround path setup childTree teardown
              TE.Each ->
                pure $ EachAround path setup childTree teardown
      --
      P.Fixture
        { config = c
        , tests
        } -> do
          cq <- loadTests tests
          pure $ Fixture{path, title = c.title, tests = cq}
   where
    path = L.ExePath $ pn.path : coerce xpth

data BeforeStatus
  = BeforePending
  | BeforeRunning
  | BeforeQRunning
  | BeforeAbandoning
  | BeforeDone
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

canRunChildQ :: ChildQ a -> STM Bool
canRunChildQ cq =
  runnable <$> readTVar cq.status
 where
  runnable = \case
    Runnable -> True
    Saturated -> False
    Done -> False

newtype QElementRun = QElementRun {hasRun :: Bool}

runChildQ :: forall a. Concurrency -> (a -> IO QElementRun) -> (a -> STM CanRun) -> ChildQ a -> IO QElementRun
runChildQ concurrency runner childCanRun ChildQ{childNodes, status, runningCount} =
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
              hasRun' <-
                finally
                  do
                    cr <- atomically $ childCanRun a
                    cr & \case
                      -- runner MUST ensure the integrity of sub element status and handle all exceptions
                      Runnable -> do
                        -- when Concurrent, the element is placed back on the end of the q before running so
                        -- can be picked up by other threads child qs are concurrent test items are not
                        when (concurrency == Concurrent) $
                          atomically (writeTQueue childNodes a)
                        runner a
                      -- when not runnabel clean up the q by returning without adding the element back
                      Saturated -> pure $ QElementRun False
                      Done -> pure $ QElementRun False
                  (atomically $ modifyTVar runningCount pred)
              runChildQ' $ QElementRun (hasRun'.hasRun || hasRun.hasRun)
          )

mkChildQ :: (Foldable m) => m a -> IO (ChildQ a)
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

data ExeIn oi ti tsti = ExeIn
  { onceIn :: oi
  , threadIn :: ti
  , tstIn :: tsti
  }

type Logger = L.EngineEvent L.ExePath AE.ApEvent -> IO ()

logAbandonned :: Logger -> L.ExePath -> SuiteEvent -> L.FailPoint -> IO ()
logAbandonned lgr p e a =
  lgr $
    L.ParentFailure
      { loc = p
      , suiteEvent = e
      , failLoc = a.path
      , failSuiteEvent = a.suiteEvent
      }

ioLeft :: forall a. L.FailPoint -> IO (Either L.FailPoint a)
ioLeft = pure . Left

ioRight :: forall a. a -> IO (Either L.FailPoint a)
ioRight = pure . Right

noImpPropertyError :: any
noImpPropertyError = error "property tests not implemented"

logReturnFailure :: Logger -> L.ExePath -> SuiteEvent -> SomeException -> IO (Either L.FailPoint b)
logReturnFailure lgr p et e =
  lgr (L.mkFailure p et e) >> ioLeft (L.FailPoint p et)

data CanAbandon = None | Partial | All
  deriving (Show, Eq)

-- TODO: double check logic with all calls to this after refactor is done
-- esp use of Partial vs All
canAbandon :: ExeTree hi -> STM CanAbandon
canAbandon = \case
  -- Once* will be redundant until killing a test run is implemented
  -- because the runner will only be abandoning nodes before they are started
  -- Once* hooks are effectively single threaded
  OnceBefore{beforeStatus} -> do
    s <- readTVar beforeStatus
    pure $
      s & \case
        BeforePending -> All
        BeforeRunning -> Partial
        BeforeQRunning -> Partial
        BeforeDone -> None
        BeforeAbandoning -> None
  OnceAround{status} -> do
    s <- readTVar status
    pure $
      s & \case
        SetupPending -> All
        SetupRunning -> Partial
        AroundQRunning -> Partial
        AroundAbandoning -> None
        TeardownRunning -> None
        AroundDone -> None
  OnceAfter{status'} -> do
    s <- readTVar status'
    pure $
      s & \case
        AfterQPending -> All
        AfterQRunning -> Partial
        AfterRunning -> None
        AfterAbandoning -> None
        AfterDone -> None

  -- base non singleton can run status on underlying q
  -- the q is the only state for these constructors
  ThreadAfter{subNodes'} -> canAbandonQ subNodes'
  ThreadAround{subNodes} -> canAbandonQ subNodes
  ThreadBefore{subNodes} -> canAbandonQ subNodes
  --
  EachBefore{subNodes} -> canAbandonQ subNodes
  EachAround{subNodes} -> canAbandonQ subNodes
  EachAfter{subNodes'} -> canAbandonQ subNodes'
  --
  Fixture{tests} -> tests & \case 
      Queue q -> canAbandonQ q
      PropertyTest _ -> noImpPropertyError
 where
  canAbandonQ :: ChildQ a -> STM CanAbandon
  canAbandonQ q = 
    convert <$> readTVar q.status
    where 
      convert = \case
        Runnable -> All
        Saturated -> None
        Done -> None



canRunXTree :: ExeTree hi -> STM CanRun
canRunXTree = \case
  OnceBefore{subNodes, beforeStatus} -> do
    s <- readTVar beforeStatus
    qs <- readTVar subNodes.status
    pure $
      s & \case
        BeforePending -> Runnable
        BeforeRunning -> Runnable
        BeforeQRunning -> stepDownQStatus qs
        BeforeAbandoning -> Saturated
        BeforeDone -> Done
  OnceAround{subNodes, status} -> do
    s <- readTVar status
    qs <- readTVar subNodes.status
    pure $
      s & \case
        SetupPending -> Runnable
        SetupRunning -> Runnable
        AroundQRunning -> stepDownQStatus qs
        AroundAbandoning -> Saturated
        TeardownRunning -> Saturated
        AroundDone -> Done
  OnceAfter{subNodes', status'} ->
    do
      s <- readTVar status'
      qs <- readTVar subNodes'.status
      pure $
        s & \case
          AfterQPending -> Runnable
          AfterQRunning -> stepDownQStatus qs
          AfterRunning -> Saturated
          AfterAbandoning -> Saturated
          AfterDone -> Done

  -- base non singleton canRun status on underlying q
  ThreadBefore{subNodes} -> canRunQ subNodes
  ThreadAround{subNodes} -> canRunQ subNodes
  ThreadAfter{subNodes'} -> canRunQ subNodes'
  EachBefore{subNodes} -> canRunQ subNodes
  EachAround{subNodes} -> canRunQ subNodes
  EachAfter{subNodes'} -> canRunQ subNodes'
  Fixture{tests} -> tests & \case 
      Queue q -> canRunQ q
      PropertyTest _ -> noImpPropertyError
 where
  canRunQ q = readTVar q.status
  stepDownQStatus = \case
    Runnable -> Runnable
    Saturated -> Saturated
    Done -> Saturated

canRunBeforeOnce :: BeforeStatus -> CanRun -> Bool
canRunBeforeOnce s qs = case s of
  BeforePending -> True
  BeforeRunning -> True
  BeforeQRunning ->
    qs & \case
      Runnable -> True
      Saturated -> False
      Done -> False
  BeforeDone -> False
  BeforeAbandoning -> True

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

canLockBefore :: BeforeStatus -> CanRun -> Bool
canLockBefore s _qs = case s of
  BeforePending -> True
  BeforeRunning -> False
  BeforeQRunning -> False
  BeforeDone -> False
  BeforeAbandoning -> False

canLockTeardown :: AroundStatus -> CanRun -> Bool
canLockTeardown s qs =
  case s of
    SetupPending -> False
    SetupRunning -> False
    AroundQRunning -> qDone
    TeardownRunning -> False
    AroundDone -> False
    AroundAbandoning -> qDone
 where
  qDone = case qs of
    Runnable -> False
    Saturated -> False
    Done -> True

-- TODO play with case statements
-- runNode2 :: ExeTree hi -> a
-- runNode2 AfterOnce{ } = undefined

-- runNode2 n = case n of
--    AfterOnce {} -> undefined
--    _ -> undefined

runNode ::
  forall hi.
  Logger ->
  NodeIn hi ->
  ExeTree hi ->
  IO QElementRun
runNode lgr hi xt =
  run hi xt
 where
  logRun' :: SuiteEvent -> (P.ApEventSink -> IO b) -> IO (Either L.FailPoint b)
  logRun' et action = logRun lgr xt.path et (action sink)

  logRun_ :: SuiteEvent -> (P.ApEventSink -> IO b) -> IO ()
  logRun_ et action = void $ logRun' et action

  logAbandonned_ :: SuiteEvent -> FailPoint -> IO ()
  logAbandonned_ = logAbandonned lgr xt.path

  logAbandonned' :: forall a. SuiteEvent -> FailPoint -> IO (Either FailPoint a)
  logAbandonned' se fp = logAbandonned_ se fp >> pure (Left fp)

  runSubNodes :: forall hi'. NodeIn hi' -> ChildQ (ExeTree hi') -> IO QElementRun
  runSubNodes hi'' = runChildQ Concurrent (runNode lgr hi'') canRunXTree

  hasRun :: Bool -> IO QElementRun
  hasRun = pure . QElementRun

  -- tree generation is restricted by typeclasses so unless the typeclass constraint implmentation is wrong
  -- execution trees with invalid structure (Thread or Once depending on Each, or Once depending on Thread)
  -- should never be generated.
  -- The only way these errors could be thrown is from unit testing code that generates an invalid testsuite
  -- via lower level constructors where no typeclass contraints apply
  invalidTree :: Text -> Text -> IO QElementRun
  invalidTree input cst = bug @Void . error $ input <> " >>> should not be passed to >>> " <> cst <> "\n" <> txt xt.path

  sink :: P.ApEventSink
  sink = lgr . L.ApEvent

  runTestsWithEachContext :: forall ti. IO (TestContext ti) -> TestSource ti -> IO QElementRun
  runTestsWithEachContext ctx =
    \case
      Queue childQ ->  
        runChildQ Sequential runInCtx (const $ pure Runnable) childQ
        where
          runInCtx t = do
            MkTestContext hkin aftr <- ctx
            runTest (pure hkin) aftr t
      PropertyTest _hi -> noImpPropertyError
   

  runTests :: forall ti. IO (Either FailPoint ti) -> IO () -> TestSource ti -> IO QElementRun
  runTests su td = \case
     Queue childQ -> runChildQ Sequential (runTest su td) (const $ pure Runnable) childQ
     PropertyTest _hi -> noImpPropertyError

  runTest :: forall ti. IO (Either FailPoint ti) -> IO () -> P.Test IO ti -> IO QElementRun
  runTest hi' after t = hi' >>= \i -> runTest' i after t

  runTest' :: forall ti. Either FailPoint ti -> IO () -> P.Test IO ti -> IO QElementRun
  runTest' hi' after t =
    do
      let path = mkTestPath t
      hi'
        & either
          (logAbandonned lgr path TE.Test)
          (void . logRun lgr path TE.Test . t.action sink)
      after
      pure $ QElementRun True

  mkTestPath :: forall a. P.Test IO a -> L.ExePath
  mkTestPath P.MkTest{id, title = ttl} = L.ExePath $ AE.TestPath{id, title = ttl} : coerce xt.path {- fixture path -}
  abandonSubs :: forall a. FailPoint -> ChildQ (ExeTree a) -> IO QElementRun
  abandonSubs fp = runSubNodes (Abandon fp)

  notRun :: IO QElementRun
  notRun = pure $ QElementRun False

  runOnceAfter :: forall a. ChildQ (ExeTree a) -> TVar AfterStatus -> NodeIn a -> AfterStatus -> IO () -> IO QElementRun
  runOnceAfter subNodes status nxtIn lockedStatus runHook =
    do
      run' <- atomically $ do
        s <- readTVar status
        qs <- readTVar subNodes.status
        when (s == AfterQPending) $
          writeTVar status AfterQRunning
        pure $ canRunAfterOnce s qs
      run'
        ? finally
          (runSubNodes nxtIn subNodes)
          ( do
              locked <- tryLockIO canLockAfterOnce status subNodes lockedStatus
              when locked $
                finally
                  runHook
                  (atomically $ writeTVar status AfterDone)
          )
        $ notRun

  noOp :: IO ()
  noOp = pure ()

  noOp' :: a -> IO ()
  noOp' = const noOp

  -- GOF strikes back
  singleton :: forall a. TMVar a -> IO a -> IO a
  singleton tCache ioa = do
    ma <- atomically $ tryReadTMVar tCache
    ma
      & maybe
        ( do
            a <- ioa
            atomically $ writeTMVar tCache a
            pure a
        )
        pure

  -- GOF strikes back
  singleton' :: forall a. TMVar a -> IO a -> a -> IO a
  singleton' tCache ioa = const $ singleton tCache ioa

  runThreadSetup :: forall i o. TMVar (Either FailPoint o) -> SuiteEvent -> (P.ApEventSink -> i -> IO o) -> Either FailPoint i -> IO (Either FailPoint o)
  runThreadSetup tCache evnt setup eti = do
    -- a singleton to avoid running empty subnodes (could happen if another thread finishes child list)
    -- no need for thread synchronisation as this happpens within a thread
    singleton tCache $ runSetup evnt setup eti

  runSetup :: forall i o. SuiteEvent -> (P.ApEventSink -> i -> IO o) -> Either FailPoint i -> IO (Either FailPoint o)
  runSetup evnt setup =
    either
      (logAbandonned' evnt)
      (logRun' evnt . flip setup)

  runThreadTeardown :: forall i. TMVar (Either FailPoint i) -> (P.ApEventSink -> i -> IO ()) -> IO ()
  runThreadTeardown = runPostThread TE.Teardown

  runThreadAfter :: TMVar (Either FailPoint ()) -> (P.ApEventSink -> IO ()) -> IO ()
  runThreadAfter tCache after = runPostThread TE.After tCache (\s _i -> after s)

  runPostThread :: forall i. HookPos -> TMVar (Either FailPoint i) -> (P.ApEventSink -> i -> IO ()) -> IO ()
  runPostThread hp tCache teardown = do
    -- unless ()
    mho <- atomically $ tryReadTMVar tCache
    mho -- if mho is Nothing it means hook was not run (empty subnodes)
      & maybe
        (pure ())
        ( either
            (logAbandonned_ (Hook Thread hp))
            (logRun_ (Hook Thread hp) . flip teardown)
        )

  onceRun :: Bool -> IO QElementRun -> IO QElementRun
  onceRun hookRun childQRun = childQRun >>= \qr -> pure $ QElementRun $ hookRun || qr.hasRun

  run :: NodeIn hi' -> ExeTree hi' -> IO QElementRun
  run =
    \cases
      -- For Once* we assume tree shaking has been executed prior to execution.
      -- There is no possibility of empty subnodes due to tree shaking, so these hooks will always
      -- need to be run

      -- abandon -> onceBefore
      Abandon{fp} ob@OnceBefore{subNodes} -> do
        (beforeLocked, canAbandonQ) <- atomically $ do
          ca <- canAbandon ob
          let locked = ca == All
          when locked $
            writeTVar ob.beforeStatus BeforeAbandoning
          pure (locked, ca /= None)
        when beforeLocked $
          logAbandonned_ (Hook Once Before) fp
        canAbandonQ
          ? onceRun beforeLocked (abandonSubs fp subNodes)
          $ hasRun beforeLocked

      --
      -- abandon -> onceAround
      Abandon{fp} oa@OnceAround{subNodes, status} -> do
        setUpLocked <- atomically $ do
          ca <- canAbandon oa
          let locked = ca == All
          when locked $
            writeTVar status AroundAbandoning
          pure locked
        when setUpLocked $
          logAbandonned_ (Hook Once Setup) fp
        finally
          (onceRun setUpLocked (abandonSubs fp subNodes))
          ( do
              locked <- tryLockIO canLockTeardown status subNodes TeardownRunning
              when locked $
                finally
                  (logAbandonned_ (Hook Once Teardown) fp)
                  (atomically $ writeTVar status AroundDone)
          )
      --
      -- abandon -> onceAfter
      abn@Abandon{fp} OnceAfter{subNodes', status'} ->
        runOnceAfter subNodes' status' abn AfterAbandoning (logAbandonned_ (Hook Once After) fp)
      --
      -- abandon -> threadBefore
      Abandon{fp} ThreadBefore{subNodes} -> do
        tCache <- newEmptyTMVarIO
        runSubNodes (mkThreadContext (ioLeft fp) (singleton' tCache abandon)) subNodes
       where
        abandon = logAbandonned' (Hook Thread Before) fp
      --
      -- abandon -> threadAround
      Abandon{fp} ThreadAround{subNodes, teardown} -> do
        tCache <- newEmptyTMVarIO
        hasRun' <- runSubNodes (mkThreadContext (ioLeft fp) (singleton' tCache abandon)) subNodes
        runThreadTeardown tCache teardown
        pure hasRun'
       where
        abandon = logAbandonned' (Hook Thread Setup) fp
      --
      -- abandon -> threadAfter
      Abandon{fp} ThreadAfter{subNodes'} -> do
        r <- abandonSubs fp subNodes'
        when r.hasRun $
          logAbandonned_ (Hook Thread After) fp
        pure r
      --
      -- abandon -> eachBefore
      Abandon{fp} EachBefore{subNodes} ->
        runSubNodes (mkEachContext (ioLeft fp) noOp nxtSetup noOp') subNodes
       where
        nxtSetup = const $ logAbandonned' (Hook Each Before) fp
      --
      -- abandon -> eachAround
      Abandon{fp} EachAround{subNodes} ->
        runSubNodes (mkEachContext (pure $ Left fp) noOp nxtSetup nxtTeardown) subNodes
       where
        nxtSetup = const $ logAbandonned' (Hook Each Setup) fp
        nxtTeardown = const $ logAbandonned_ (Hook Each Teardown) fp
      --
      -- abandon -> eachAfter
      Abandon{fp} EachAfter{subNodes'} ->
        runSubNodes (mkEachContext (ioLeft fp) noOp nxtSetup nxtAfter) subNodes'
       where
        nxtSetup = const $ ioLeft fp
        nxtAfter = const $ logAbandonned_ (Hook Each After) fp
      --
      -- abandon -> test
      Abandon{fp} Fixture{tests} ->
        runTests (pure $ Left fp) noOp tests
      --
      -- onceIn -> onceBefore
      OnceIn{hki} OnceBefore{before, beforeStatus, cache, subNodes} -> do
        setUpLocked <- tryLockIO canLockBefore beforeStatus subNodes BeforeRunning
        eho <-
          if setUpLocked
            then do
              eho <- logRun' (Hook Once Before) (`before` hki)
              atomically $ do
                writeTMVar cache eho
                writeTVar
                  beforeStatus
                  ( eho & \case
                      Left _ -> BeforeAbandoning
                      Right _ -> BeforeQRunning
                  )
              pure eho
            else atomically (readTMVar cache)
        -- TODO: add dbLog to prelude like: debugf' (const "OnceInd") "SUBNODES OF ONCE")
        runQ <- statusCheckIO canRunBeforeOnce beforeStatus subNodes
        if runQ
          then
            finally
              ( eho
                  & either
                    (`abandonSubs` subNodes)
                    (\ho -> runSubNodes (OnceIn ho) subNodes)
              )
              (atomically $ writeTVar beforeStatus BeforeDone)
          else
            hasRun False
      --
      -- onceIn -> onceAround
      OnceIn{hki} OnceAround{setup, teardown, status, cache, subNodes} -> do
        setUpLocked <- tryLockIO canLockSetup status subNodes SetupRunning
        eho <-
          if setUpLocked
            then do
              eho <- logRun' (Hook Once Setup) (`setup` hki)
              atomically $ do
                writeTMVar cache eho
                writeTVar status AroundQRunning
              pure eho
            else atomically (readTMVar cache)
        finally
          ( eho
              & either
                ( \fp -> do
                    (flip runSubNodes subNodes . Abandon) fp
                )
                ( \ho -> do
                    runSubNodes (OnceIn ho) subNodes
                )
          )
          ( do
              locked <- tryLockIO canLockTeardown status subNodes TeardownRunning
              when locked $
                finally
                  ( eho
                      & either
                        (logAbandonned_ (Hook Once Teardown))
                        (\hi' -> logRun_ (Hook Once Teardown) (`teardown` hi'))
                  )
                  (atomically $ writeTVar status AroundDone)
          )
      --
      -- onceIn -> onceAfter
      oi@OnceIn{} OnceAfter{status', subNodes', after} ->
        runOnceAfter subNodes' status' oi AfterRunning (logRun_ (Hook Once After) after)
      --
      -- onceIn -> threadBefore
      OnceIn{hki} ThreadBefore{before, subNodes} -> do
        tCache <- newEmptyTMVarIO
        let nxtSetup = runThreadSetup tCache (Hook Thread Before) before
        runSubNodes (mkThreadContext (ioRight hki) nxtSetup) subNodes
      --
      -- onceIn -> threadAround
      OnceIn{hki} ThreadAround{setup, teardown, subNodes} ->
        do
          tCache <- newEmptyTMVarIO
          let
            nxtSetup = runThreadSetup tCache (Hook Thread Setup) setup
            -- nxtTeardown = const $ runThreadTeardown tCache teardown -- BUG was running after each test
            -- runSubNodes (mkThreadContext (ioRight hki) noOp nxtSetup nxtTeardown) 
          r <- runSubNodes (mkThreadContext (ioRight hki) nxtSetup) subNodes 
          runThreadTeardown tCache teardown
          pure r
      --
      -- onceIn -> threadAfter
      oi@(OnceIn{}) ThreadAfter{after, subNodes'} ->
        do
          -- may not run if subnodes are empty
          run' <- runSubNodes oi subNodes'
          when run'.hasRun $
            logRun_ (Hook Thread After) after
          pure run'
      --
      -- onceIn -> eachBefore
      (OnceIn hki) EachBefore{before, subNodes} ->
        runSubNodes (mkEachContext (ioRight hki) noOp nxtSetup noOp') subNodes
       where
        nxtSetup = const $ logRun' (Hook Each Before) (`before` hki)
      --
      -- onceIn -> eachAround
      OnceIn{hki} EachAround{setup, teardown, subNodes} ->
        runSubNodes (mkEachContext (ioRight hki) noOp nxtSetup nxtTeardown) subNodes
       where
        nxtSetup = const $ logRun' (Hook Each Setup) (`setup` hki)
        nxtTeardown =
          either
            (logAbandonned_ (Hook Each Teardown))
            (\ho -> logRun_ (Hook Each Teardown) (`teardown` ho))
      --
      -- onceIn -> eachAfter
      OnceIn{hki} EachAfter{after, subNodes'} ->
        runSubNodes (mkEachContext (ioRight hki) noOp pure nxtTeardown) subNodes'
       where
        nxtTeardown =
          either
            (logAbandonned_ (Hook Each After))
            (\_ -> logRun_ (Hook Each After) after)
      --
      -- onceIn -> fixture
      OnceIn{hki} Fixture{tests} ->
        runTests (pure $ Right hki) noOp tests
      --
      -- threadContext -> threadbefore
      ThreadContext{threadContext} ThreadBefore{before, subNodes} ->
        do
          tCache <- newEmptyTMVarIO
          let nxtBefore = runThreadSetup tCache (Hook Thread Before) before
          runSubNodes (mkThreadContext threadContext nxtBefore) subNodes
      --
      -- threadContext -> threadAround
      ThreadContext{threadContext} ThreadAround{setup, teardown, subNodes} ->
        do
          tCache <- newEmptyTMVarIO
          let nxtBefore = runThreadSetup tCache (Hook Thread Setup) setup
          hasRun' <- runSubNodes (mkThreadContext threadContext nxtBefore) subNodes
          runThreadTeardown tCache teardown
          pure hasRun'

      --
      -- threadContext -> threadAfter
      ThreadContext{threadContext} ThreadAfter{subNodes', after} ->
        do
        tCache <- newEmptyTMVarIO
        let
          nxtBefore :: Either FailPoint hi' -> IO (Either FailPoint hi')
          nxtBefore hi' = do
            mt <- atomically $ isEmptyTMVar tCache 
            when mt do
             atomically $ writeTMVar tCache $ hi' $> ()
            pure hi'
        hasRun' <- runSubNodes (mkThreadContext threadContext nxtBefore) subNodes'
        runThreadAfter tCache after
        pure hasRun'

      --
      -- threadContext -> eachBefore
      -- TODO same as eachContext -> eachBefore refactor
      ThreadContext{threadContext} EachBefore{before, subNodes} ->
        runSubNodes (mkEachContext threadContext noOp nxtSetup noOp') subNodes
       where
        nxtSetup = runSetup (Hook Each Before) before
      -- TODO same as eachContext -> eachAround refactor
      ThreadContext{threadContext} EachAround{setup, teardown, subNodes} ->
        runSubNodes (mkEachContext threadContext noOp nxtSetup nxtTeardown) subNodes
       where
        nxtSetup = runSetup (Hook Each Setup) setup
        nxtTeardown =
          either
            (logAbandonned_ (Hook Each Teardown))
            (\i -> logRun_ (Hook Each Teardown) (`teardown` i))
      --
      -- threadContext -> eachAfter
      -- TODO same as eachContext -> eachAfter refactor
      ThreadContext{threadContext} EachAfter{subNodes', after} ->
        runSubNodes (mkEachContext threadContext noOp pure nxtAfter) subNodes'
       where
        nxtAfter =
          either
            (logAbandonned_ (Hook Each After))
            (\_i -> logRun_ (Hook Each After) after)
      -- threadContext -> fixture
      -- TODO same as eachContext -> fixture refactor
      ThreadContext{threadContext} Fixture{tests} -> runTests threadContext noOp tests
      -- --
      -- context -> eachBefore
      EachContext{testContext} EachBefore{before, subNodes} ->
        runSubNodes (composeEachContext testContext nxtSetup noOp') subNodes
       where
        nxtSetup = runSetup (Hook Each Before) before
      --
      -- context -> eachAround
      EachContext{testContext} EachAround{setup, teardown, subNodes} ->
        runSubNodes (composeEachContext testContext nxtSetup nxtTeardown) subNodes
       where
        nxtSetup = runSetup (Hook Each Setup) setup
        nxtTeardown =
          either
            (logAbandonned_ (Hook Each Teardown))
            (\i -> logRun_ (Hook Each Teardown) (`teardown` i))
      --
      --
      -- context -> eachAfter
      EachContext{testContext} EachAfter{subNodes', after} ->
        runSubNodes (composeEachContext testContext pure nxtAfter) subNodes'
       where
        nxtAfter =
          either
            (logAbandonned_ (Hook Each After))
            (\_i -> logRun_ (Hook Each After) after)
      --
      -- context -> fixtures
      EachContext{testContext} Fixture{tests} -> runTestsWithEachContext testContext tests
      -- ### all invalid combos ### --
      ThreadContext{} OnceBefore{} -> invalidTree "ThreadContext" "OnceBefore"
      ThreadContext{} OnceAround{} -> invalidTree "ThreadContext" "OnceAround"
      ThreadContext{} OnceAfter{} -> invalidTree "ThreadContext" "OnceAfter"
      EachContext{} OnceBefore{} -> invalidTree "EachContext" "OnceBefore"
      EachContext{} OnceAround{} -> invalidTree "EachContext" "OnceAround"
      EachContext{} OnceAfter{} -> invalidTree "EachContext" "OnceAfter"
      EachContext{} ThreadBefore{} -> invalidTree "EachContext" "ThreadBefore"
      EachContext{} ThreadAround{} -> invalidTree "EachContext" "ThreadAround"
      EachContext{} ThreadAfter{} -> invalidTree "EachContext" "ThreadAfter"

data NodeIn hi where
  Abandon :: {fp :: FailPoint} -> NodeIn hi
  OnceIn :: {hki :: hi} -> NodeIn hi
  ThreadContext :: {threadContext :: IO (Either FailPoint hi)} -> NodeIn hi
  EachContext ::
    { testContext :: IO (TestContext hi)
    } ->
    NodeIn hi

data TestContext hi = MkTestContext
  { -- hookIn :: IO (Either FailPoint hi),
    hookIn :: Either FailPoint hi
  , after :: IO ()
  }

mkTestContext :: forall hi ho. Either FailPoint hi -> IO () -> (Either FailPoint hi -> IO (Either FailPoint ho)) -> (Either FailPoint ho -> IO ()) -> IO (TestContext ho)
mkTestContext parentIn afterParent setupNxt teardownNxt =
  -- must be in IO so teardown has access to ho
  do
    eho <- setupNxt parentIn
    pure $ MkTestContext eho $ teardownNxt eho >> afterParent

mkTestContextM :: forall hi ho. IO (Either FailPoint hi) -> IO () -> (Either FailPoint hi -> IO (Either FailPoint ho)) -> (Either FailPoint ho -> IO ()) -> IO (TestContext ho)
mkTestContextM parentIn afterParent setupNxt teardownNxt =
  do
    hi <- parentIn
    mkTestContext hi afterParent setupNxt teardownNxt

composeTestContext :: forall hi ho. IO (TestContext hi) -> (Either FailPoint hi -> IO (Either FailPoint ho)) -> (Either FailPoint ho -> IO ()) -> IO (TestContext ho)
composeTestContext parentContext setupNxt teardownNxt =
  do
    MkTestContext{hookIn, after} <- parentContext
    mkTestContext hookIn after setupNxt teardownNxt

mkEachContext :: forall hi ho. IO (Either FailPoint hi) -> IO () -> (Either FailPoint hi -> IO (Either FailPoint ho)) -> (Either FailPoint ho -> IO ()) -> NodeIn ho
mkEachContext parentIn afterParent setupNxt teardownNxt =
  EachContext $ mkTestContextM parentIn afterParent setupNxt teardownNxt

composeEachContext :: forall hi ho. IO (TestContext hi) -> (Either FailPoint hi -> IO (Either FailPoint ho)) -> (Either FailPoint ho -> IO ()) -> NodeIn ho
composeEachContext parentContext setupNxt teardownNxt =
  EachContext $ composeTestContext parentContext setupNxt teardownNxt

mkThreadContext :: forall hi ho. IO (Either FailPoint hi) -> (Either FailPoint hi -> IO (Either FailPoint ho)) -> NodeIn ho
mkThreadContext parentIn setupNxt =
  ThreadContext $ parentIn >>= setupNxt

{-
newContext :: IO (TestContext hi) -> (Either FailPoint hi -> IO (Either FailPoint ho)) -> (Either FailPoint ho -> IO ()) -> IO (TestContext ho)
newContext ioCtx setupNxt teardownNxt = do
  MkTestContext{setup, teardown} <- ioCtx
  ( \hi ->
      MkTestContext
        { setup = setupNxt hi
        , teardown = \ho -> teardownNxt ho >> teardown hi
        }
    )
    <$> setup
-}

applyEach :: ((Either FailPoint hi -> IO ()) -> IO ()) -> IO () -> (Either FailPoint hi -> IO ()) -> IO ()
applyEach apply after test = apply test >> after

statusCheck :: (s -> CanRun -> Bool) -> TVar s -> ChildQ a -> STM Bool
statusCheck canRun hs cq = canRun <$> readTVar hs <*> readTVar cq.status

statusCheckIO :: (s -> CanRun -> Bool) -> TVar s -> ChildQ a -> IO Bool
statusCheckIO canRun hs cq = atomically $ statusCheck canRun hs cq

tryLock :: (s -> CanRun -> Bool) -> TVar s -> ChildQ a -> s -> STM Bool
tryLock canLock hs cq lockedStatus =
  do
    l <- statusCheck canLock hs cq
    when l $
      writeTVar hs lockedStatus
    pure l

tryLockIO :: (s -> CanRun -> Bool) -> TVar s -> ChildQ a -> s -> IO Bool
tryLockIO canLock hs cq lockedStatus = atomically $ tryLock canLock hs cq lockedStatus

logRun :: Logger -> L.ExePath -> SuiteEvent -> IO b -> IO (Either L.FailPoint b)
logRun lgr path evt action = do
  lgr $ L.Start evt path
  finally
    ( catchAll
        -- TODO :: test for strictness issues esp with failing thread hook
        -- eg returns handle and handle is closed
        (Right <$> action)
        (logReturnFailure lgr path evt)
    )
    (lgr $ L.End evt path)
