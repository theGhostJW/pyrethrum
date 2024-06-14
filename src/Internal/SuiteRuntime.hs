module Internal.SuiteRuntime where

import Core qualified as C
import DSL.Internal.ApEvent qualified as AE
import Internal.RunTimeLogging (FailPoint)
import Internal.RunTimeLogging qualified as L
import Internal.ThreadEvent hiding (Test)
import Internal.ThreadEvent qualified as TE
import Prepare qualified as P
import PyrethrumExtras (catchAll, debug', debug'_, debugf', toS, txt, uu, (?), debugf'_)
import Text.Show.Pretty (ppShow)
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
import Prelude qualified as P

{-
todo :: define defect properties with sum type type and typeclass which returns defect info

-}
-- TODO; move to pyrelude
ptxt :: (Show a) => a -> Text
ptxt = toS . ppShow

newtype ThreadCount = ThreadCount {maxThreads :: Int}
  deriving (Show)

execute :: (C.Config rc, C.Config tc) => ThreadCount -> L.LogControls L.ExePath AE.ApEvent -> C.ExeParams m [] rc tc -> IO ()
execute
  tc
  lc
  C.ExeParams
    { suite
    , interpreter
    , runConfig
    } = executeNodeList tc lc (P.prepare $ P.SuitePrepParams suite interpreter runConfig)

executeNodeList :: (Traversable c) => ThreadCount -> L.LogControls L.ExePath AE.ApEvent -> c (P.PreNode IO c ()) -> IO ()
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
                runChildQ Concurrent (runNode logger (OnceIn (pure ()))) canRunXTree nodes
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
    , tests :: ChildQ (P.Test IO hi)
    } ->
    ExeTree hi

mkXTree :: (Traversable m) => L.ExePath -> m (P.PreNode IO m hi) -> IO (ChildQ (ExeTree hi))
mkXTree xpth preNodes =
  do
    subTrees <- traverse mkNode preNodes
    mkChildQ subTrees
 where
  mkNode :: forall m hi. (Traversable m) => P.PreNode IO m hi -> IO (ExeTree hi)
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
          cq <- mkChildQ tests
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
                    debug'_ "CHILD CAN RUN" cr & \case
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

pureFailure :: L.ExePath -> SuiteEvent -> IO (Either L.FailPoint b)
pureFailure p = pure . Left . L.FailPoint p

logReturnFailure :: Logger -> L.ExePath -> SuiteEvent -> SomeException -> IO (Either L.FailPoint b)
logReturnFailure lgr p et e =
  lgr (L.mkFailure p et e) >> pureFailure p et

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
  ThreadAfter{subNodes'} -> qStatus subNodes'
  ThreadAround{subNodes} -> qStatus subNodes
  ThreadBefore{subNodes} -> qStatus subNodes
  --
  EachBefore{subNodes} -> qStatus subNodes
  EachAround{subNodes} -> qStatus subNodes
  EachAfter{subNodes'} -> qStatus subNodes'
  --
  Fixture{tests} -> qStatus tests
 where
  qStatus q = convert <$> readTVar q.status
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
  ThreadBefore{subNodes} -> qStatus subNodes
  ThreadAround{subNodes} -> qStatus subNodes
  ThreadAfter{subNodes'} -> qStatus subNodes'
  EachBefore{subNodes} -> qStatus subNodes
  EachAround{subNodes} -> qStatus subNodes
  EachAfter{subNodes'} -> qStatus subNodes'
  Fixture{tests} -> qStatus tests
 where
  qStatus q = readTVar q.status
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
  -- via lower level constructors where there are no typeclass contraints apply
  invalidTree :: Text -> Text -> IO QElementRun
  invalidTree input cst = bug @Void . error $ input <> " >>> should not be passed to >>> " <> cst <> "\n" <> txt xt.path

  sink :: P.ApEventSink
  sink = lgr . L.ApEvent

  runTests :: forall ti. IO (Either FailPoint ti) -> IO () -> ChildQ (P.Test IO ti) -> IO QElementRun
  runTests su td = runChildQ Sequential (runTest su td) (const $ pure Runnable)

  runTest :: forall ti. IO (Either FailPoint ti)  -> IO () -> P.Test IO ti -> IO QElementRun
  runTest hi' after t =
    do
      let path = mkTestPath t
      ti <- hi'
      ti
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

  noOp :: a -> IO ()
  noOp = const $ pure ()

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
  runThreadTeardown tCache teardown = do
    mho <- atomically $ tryReadTMVar tCache
    mho -- if mho is Nothing it means hook was not run (empty subnodes)
      & maybe
        (pure ())
        ( either
            (logAbandonned_ (Hook Thread Teardown))
            (logRun_ (Hook Thread Teardown) . flip teardown)
        )

  onceRun :: Bool -> IO QElementRun -> IO QElementRun
  onceRun hookRun childQRun = childQRun >>= \qr -> pure $ QElementRun $ hookRun || qr.hasRun

  nxtRunner = uu

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
        runSubNodes (nxtRunner (singleton tCache abandon) noOp) subNodes
       where
        abandon = logAbandonned' (Hook Thread Before) fp
      --
      -- abandon -> threadAround
      Abandon{fp} ThreadAround{subNodes, teardown} -> do
        tCache <- newEmptyTMVarIO
        let nxtTeardown _ignored = runThreadTeardown tCache teardown
        runSubNodes (nxtRunner (singleton tCache abandon) nxtTeardown) subNodes
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
        runSubNodes (nxtRunner nxtSetup noOp) subNodes
       where
        nxtSetup = logAbandonned' (Hook Each Before) fp
      --
      -- abandon -> eachAround
      Abandon{fp} EachAround{subNodes} ->
        runSubNodes (nxtRunner nxtSetup nxtTeardown) subNodes
       where
        nxtSetup = logAbandonned' (Hook Each Setup) fp
        nxtTeardown _ignore = logAbandonned_ (Hook Each Teardown) fp
      --
      -- abandon -> eachAfter
      Abandon{fp} EachAfter{subNodes'} ->
        runSubNodes (nxtRunner nxtSetup nxtTeardown) subNodes'
       where
        nxtSetup = pure $ Left fp
        nxtTeardown _ignore = logAbandonned_ (Hook Each After) fp

      --
      -- abandon -> test
      Abandon{fp} Fixture{tests} ->
        runTests (pure $ Left fp) noOp tests
      --
      -- onceIn -> onceBefore
      OnceIn{ioHi} OnceBefore{before, beforeStatus, cache, subNodes} -> do
        i <- ioHi
        setUpLocked <- tryLockIO canLockBefore beforeStatus subNodes BeforeRunning
        eho <-
          if setUpLocked
            then do
              eho <- logRun' (Hook Once Before) (`before` i)
              atomically $ do
                writeTMVar cache eho
                writeTVar beforeStatus
                  ( eho & \case
                      Left _ -> BeforeAbandoning
                      Right _ -> BeforeQRunning
                  )
              pure eho
            else atomically (readTMVar cache)
        -- TODO: add dbLog to prelude like: debugf' (const "OnceInd") "SUBNODES OF ONCE")
        runQ <- statusCheckIO canRunBeforeOnce beforeStatus subNodes
        if runQ then
          finally
            ( eho
                & either
                  (`abandonSubs` subNodes)
                  (\ho -> runSubNodes (OnceIn $ pure ho) subNodes)
            )
            (atomically $ writeTVar beforeStatus BeforeDone)
        else 
          hasRun False
      --
      -- onceIn -> onceAround
      OnceIn{ioHi} OnceAround{setup, teardown, status, cache, subNodes} -> do
        hki <- ioHi
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
                    runSubNodes (OnceIn $ pure ho) subNodes
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
      OnceIn{ioHi} ThreadBefore{before, subNodes} -> do
        oi <- ioHi
        tCache <- newEmptyTMVarIO
        let nxtSetup = runThreadSetup tCache (Hook Thread Before) before $ Right oi
        runSubNodes (nxtRunner nxtSetup noOp) subNodes
      --
      -- onceIn -> threadAround
      OnceIn{ioHi} ThreadAround{setup, teardown, subNodes} -> do
        oi <- ioHi
        tCache <- newEmptyTMVarIO
        let 
          nxtSetup = runThreadSetup tCache (Hook Thread Setup) setup $ Right oi
          nxtTeardown _ignored = runThreadTeardown tCache teardown
        runSubNodes (nxtRunner nxtSetup nxtTeardown) subNodes
      --
      -- onceIn -> threadAfter
      oi@(OnceIn{}) ThreadAfter{after, subNodes'} ->
        do
          -- may not run if subnodes are empty
          run' <- debugf'_ (const "") "RUN ThreadAfter SUBNODES" $ runSubNodes oi subNodes'
          when (run'.hasRun & debug'_ "HAS RUN - threadAfter") $
            logRun_ (Hook Thread After) after
          pure run'
      --
      -- onceIn -> eachBefore
      (OnceIn ioHi) EachBefore{before, subNodes} ->
        runSubNodes (nxtRunner nxtSetup noOp) subNodes
       where
        nxtSetup = ioHi >>= \ho -> logRun' (Hook Each Before) (`before` ho)
      --
      -- onceIn -> eachAround
      OnceIn{ioHi} EachAround{setup, teardown, subNodes} ->
        runSubNodes (nxtRunner nxtSetup nxtTeardown) subNodes
       where
        nxtSetup = ioHi >>= \ho -> logRun' (Hook Each Setup) (`setup` ho)
        nxtTeardown =
          either
            (logAbandonned_ (Hook Each Teardown))
            (\ho -> logRun_ (Hook Each Teardown) (`teardown` ho))
      --
      -- onceIn -> eachAfter
      OnceIn{ioHi} EachAfter{after, subNodes'} ->
        runSubNodes (nxtRunner nxtSetup nxtTeardown) subNodes'
       where
        nxtSetup = Right <$> ioHi
        nxtTeardown =
          either
            (logAbandonned_ (Hook Each After))
            (\_ -> logRun_ (Hook Each After) after)
      --
      -- onceIn -> fixture
      OnceIn{ioHi} Fixture{tests} ->
        runTests (Right <$> ioHi) noOp tests
      --
      -- onceIn -> threadbefore
      ThreadContext{} ThreadBefore{before, subNodes} -> uu
        -- do
        --   tCache <- newEmptyTMVarIO
        --   let mkBefore = runThreadSetup tCache (Hook Thread Before) before
        --   runSubNodes (EachContext $ newContext context mkBefore noOp) subNodes
      --
      -- context -> threadAround
      ThreadContext{} ThreadAround{setup, teardown, subNodes} -> uu
        -- do
        --   tCache <- newEmptyTMVarIO
        --   let 
        --     mkBefore = runThreadSetup tCache (Hook Thread Setup) setup
        --     nxtTeardown _ignore = runThreadTeardown tCache teardown
        --   runSubNodes (EachContext $ newContext context mkBefore nxtTeardown) subNodes
          
      --
      -- context -> threadAfter
      ThreadContext{} ThreadAfter{subNodes', after} -> uu
        -- do
        --   runSubNodes (EachContext $ newContext context pure nxtAfter) subNodes'
        --   where
        --     nxtAfter i = do
        --       qRunnable <- atomically $ canRunChildQ subNodes'
        --       unless qRunnable $
        --        i & either
        --         (logAbandonned_ (Hook Thread After))
        --         (\_i -> logRun_ (Hook Thread After) after)
      ThreadContext (EachBefore _ _ _) -> uu
      ThreadContext (EachAround _ _ _ _) -> uu
      ThreadContext (EachAfter _ _ _) -> uu
      ThreadContext (Fixture _ _ _) -> uu
      -- --
      -- context -> eachBefore
      EachContext {testContext} EachBefore{before, subNodes} ->
        runSubNodes (composeEachContext testContext nxtSetup noOp) subNodes
       where
        nxtSetup = runSetup (Hook Each Before) before
      --
      -- context -> eachAround
      EachContext  {testContext} EachAround{setup, teardown, subNodes} ->
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
      (EachContext(MkTestContext {hookIn, after = parentAfter})) EachAfter{subNodes', after} ->
        runSubNodes (mkEachContext hookIn parentAfter pure nxtAfter) subNodes'
       where
        nxtAfter =
          either
            (logAbandonned_ (Hook Each After))
            (\_i -> logRun_ (Hook Each After) after)
      --
      -- context -> fixtures
      (EachContext(MkTestContext hookIn after)) Fixture{tests} -> runTests hookIn after tests
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
  OnceIn :: {ioHi :: IO hi} -> NodeIn hi
  {-
    ThreadContext 
      - registerChildRun i -> IO i 
        - mkTreadContextBefore - needs Cach - dummy register
        - mkTreadContextAround - needs Cach - dummy register
        - mkTreadContextAfter TVar Bool -> IO ThreadContext
        --
        - nxtTreadContextBefore - needs Cach - dummy register
        - nxtTreadContextAround - needs Cach - dummy register
        - nxtTreadContextAfter TVar Bool -> ThreadContext -> IO ThreadContext (compose registerChildRun)

      setup :: IO (Either FailPoint hi)
    , teardown :: Either FailPoint hi -> IO () -- use MVar for teardown / registerChildRun for setup
    } -> NodeIn hi
    -- add registerChildRun to setup when ThreadIn -> Eachin
  -}
  ThreadContext :: {} -> NodeIn hi
  EachContext ::
    { 
      testContext :: IO (TestContext hi)
    } ->
    NodeIn hi

data TestContext hi = MkTestContext
  { 
    -- hookIn :: IO (Either FailPoint hi),
    hookIn :: Either FailPoint hi,
    after :: IO ()
  }

mkTestContext :: forall hi ho. Either FailPoint hi -> IO () -> (Either FailPoint hi ->  IO (Either FailPoint ho)) -> (Either FailPoint ho -> IO ()) ->  IO (TestContext ho)
mkTestContext parentIn afterParent setupNxt teardownNxt = 
  -- must be in IO so teardown has access to ho
  do 
   eho <- setupNxt parentIn
   pure $ MkTestContext eho $ teardownNxt eho >> afterParent

composeEachContext :: forall hi ho. IO (TestContext hi) -> (Either FailPoint hi ->  IO (Either FailPoint ho)) -> (Either FailPoint ho -> IO ()) -> NodeIn ho
composeEachContext parentContext setupNxt teardownNxt  = 
   EachContext ioTc 
   where 
    ioTc :: IO (TestContext ho) 
    ioTc = do
      MkTestContext parentIn afterParent <- parentContext
      mkTestContext parentIn afterParent setupNxt teardownNxt


mkEachContext :: forall hi ho. IO (Either FailPoint hi) -> IO () -> (Either FailPoint hi ->  IO (Either FailPoint ho)) -> (Either FailPoint ho -> IO ()) -> NodeIn ho
mkEachContext parentIn afterParent setupNxt teardownNxt = 
  EachContext $ do 
   hi <- parentIn
   mkTestContext hi afterParent setupNxt teardownNxt

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
