module Internal.SuiteRuntime where

import Core qualified as C
import DSL.Internal.ApEvent qualified as AE
import Internal.RunTimeLogging (FailPoint)
import Internal.RunTimeLogging qualified as L
import Internal.ThreadEvent qualified as F (Hz (..))
import Internal.ThreadEvent qualified as TE
import Prepare qualified as P
import PyrethrumExtras (catchAll, debug', txt, (?))
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

executeNodes :: (TE.ThreadEvent L.ExePath AE.ApEvent -> IO ()) -> ChildQ (ExeTree ()) -> ThreadCount -> IO ()
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

data NFrequency = Each | Thread
  deriving (Show, Eq)

data ExeTree hi where
  AfterOnce ::
    { path :: L.ExePath
    , status' :: TVar AfterStatus
    , subNodes' :: ChildQ (ExeTree hi)
    , after :: P.ApEventSink -> IO ()
    } ->
    ExeTree hi
  AroundOnce ::
    { path :: L.ExePath
    , setup :: P.ApEventSink -> hi -> IO ho
    , status :: TVar AroundStatus
    , cache :: TMVar (Either L.FailPoint ho)
    , subNodes :: ChildQ (ExeTree ho)
    , teardown :: Maybe (P.ApEventSink -> ho -> IO ())
    } ->
    ExeTree hi
  After ::
    { path :: L.ExePath
    , frequency :: NFrequency
    , subNodes' :: ChildQ (ExeTree hi)
    , after :: P.ApEventSink -> IO ()
    } ->
    ExeTree hi
  Around ::
    { path :: L.ExePath
    , frequency :: NFrequency
    , setup :: P.ApEventSink -> hi -> IO ho
    , subNodes :: ChildQ (ExeTree ho)
    , teardown :: Maybe (P.ApEventSink -> ho -> IO ())
    } ->
    ExeTree hi
  Test ::
    { path :: L.ExePath
    , title :: Text
    , tests :: ChildQ (P.TestItem IO hi)
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
          frequency & \case
            F.Once -> mkOnceHook subNodes action Nothing
            F.Thread -> mkNHook' Thread
            F.Each -> mkNHook' Each
         where
          mkNHook' = mkNHook subNodes action Nothing
      P.After{frequency, subNodes', after} ->
        do
          cq <- mkXTree path subNodes'
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
    path = L.ExePath $ pn.path : coerce xpth

    mkOnceHook :: forall hi' ho'. m (P.PreNode IO m ho') -> (P.ApEventSink -> hi' -> IO ho') -> Maybe (P.ApEventSink -> ho' -> IO ()) -> IO (ExeTree hi')
    mkOnceHook subNodes' setup teardown = do
      status <- newTVarIO SetupPending
      cache <- newEmptyTMVarIO
      subNodes <- mkXTree path subNodes'
      pure $
        AroundOnce
          { path
          , setup
          , status
          , cache
          , subNodes
          , teardown
          }

    mkNHook :: forall hi' ho'. m (P.PreNode IO m ho') -> (P.ApEventSink -> hi' -> IO ho') -> Maybe (P.ApEventSink -> ho' -> IO ()) -> NFrequency -> IO (ExeTree hi')
    mkNHook subNodes' setup teardown frequency = do
      subNodes <- mkXTree path subNodes'
      pure $
        Around
          { path
          , setup
          , frequency
          , subNodes
          , teardown
          }

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

runChildQ :: forall a. Concurrency -> (a -> IO ()) -> (a -> STM CanRun) -> ChildQ a -> IO QElementRun
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

logAbandonned :: Logger -> L.ExePath -> TE.SuiteEvent -> L.FailPoint -> IO ()
logAbandonned lgr p e a =
  lgr $
    L.ParentFailure
      { loc = p
      , suiteEvent = e
      , failLoc = a.path
      , failSuiteEvent = a.suiteEvent
      }

logReturnFailure :: Logger -> L.ExePath -> TE.SuiteEvent -> SomeException -> IO (Either L.FailPoint b)
logReturnFailure lgr p et e =
  do
    lgr (L.mkFailure p et e)
    pure . Left $ L.FailPoint p et

data CanAbandon = None | Partial | All
  deriving (Show, Eq)

canAbandon :: ExeTree hi -> STM CanAbandon
canAbandon = \case
  AfterOnce{status'} -> do
    s <- readTVar status'
    pure $
      s & \case
        AfterQPending -> All
        AfterQRunning -> Partial
        AfterRunning -> None
        AfterAbandoning -> None
        AfterDone -> None
  AroundOnce{status} -> do
    s <- readTVar status
    pure $
      s & \case
        SetupPending -> All
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
    Runnable -> All
    Saturated -> None
    Done -> None

canRunXTree :: ExeTree hi -> STM CanRun
canRunXTree = \case
  AfterOnce{subNodes', status'} ->
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
  AroundOnce{subNodes, status} -> do
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
  IO ()
runNode lgr hi xt =
  let
    logRun' :: TE.SuiteEvent -> (P.ApEventSink -> IO b) -> IO (Either L.FailPoint b)
    logRun' et action = logRun lgr xt.path et (action sink)

    logRun_ :: TE.SuiteEvent -> (P.ApEventSink -> IO b) -> IO ()
    logRun_ et action = void $ logRun' et action

    logAbandonned' = logAbandonned lgr xt.path
    -- logAbandonnedParent = logAbandonnedNew lgr

    runSubNodes :: forall hi'. NodeIn hi' -> ChildQ (ExeTree hi') -> IO QElementRun
    runSubNodes hi'' = runChildQ Concurrent (runNode lgr hi'') canRunXTree

    runSubNodes_ :: forall hi'. NodeIn hi' -> ChildQ (ExeTree hi') -> IO ()
    runSubNodes_ n = void . runSubNodes n

    -- tree generation is restricted by typeclasses so unless the typeclass constrint implmentation is wrong
    -- execution trees with invalid structure (Thread or Once depending on Each) should never be generated.
    invalidTree :: Text -> Text -> IO ()
    invalidTree input cst = bug @Void . error $ input <> " >>> should not be passed to >>> " <> cst <> "\n" <> txt xt.path

    sink = lgr . L.ApEvent
   in
    case xt of
      -- For AfterOnce and AroundOnce we assume tree shaking has been executed prior to execution.
      -- There is no possibility of empty subnodes due to tree shaking, so these hooks will always need to be run
      AfterOnce
        { status'
        , subNodes'
        , after
        } -> do
          run <- atomically $ do
            s <- readTVar status'
            qs <- readTVar subNodes'.status
            when (s == AfterQPending) $
              writeTVar status' AfterQRunning
            pure $ canRunAfterOnce s qs
          when run $
            finally
              (runSubNodes_ hi subNodes')
              ( let
                  runAfter mAbandon = do
                    let lockStatus = isJust mAbandon ? AfterAbandoning $ AfterRunning
                    locked <- atomically $ tryLock status' subNodes' canLockAfterOnce lockStatus
                    when locked $
                      finally
                        ( mAbandon
                            & maybe
                              (void $ logRun' (TE.Hook TE.Once TE.After) after)
                              (logAbandonned' (TE.Hook TE.Once TE.After))
                        )
                        (atomically $ writeTVar status' AfterDone)
                 in
                  do
                    case hi of
                      Abandon fp -> runAfter (Just fp)
                      EachIn _ -> invalidTree "EachIn" "AfterOnce"
                      ThreadIn _ -> invalidTree "ThreadIn" "AfterOnce"
                      OnceIn _ -> runAfter Nothing
              )
      ao@AroundOnce{setup, status, cache, subNodes, teardown} ->
        let
          leadHookPos = aroundLeadingHookPos teardown
         in
          case hi of
            Abandon fp -> do
              setUpLocked <- atomically $ do
                ca <- canAbandon ao
                let locked = ca == All
                when locked $
                  writeTVar status AroundAbandoning
                pure locked
              when setUpLocked $
                logAbandonned' (TE.Hook TE.Once leadHookPos) fp
              finally
                (runSubNodes_ (Abandon fp) subNodes)
                ( -- only AbandonOld teardown if setup has not started
                  when setUpLocked $ do
                    finally
                      (logAbandonned' (TE.Hook TE.Once TE.Teardown) fp)
                      (atomically $ writeTVar status AroundDone)
                )
            EachIn _ -> invalidTree "EachIn" "AroundOnce"
            ThreadIn _ -> invalidTree "ThreadIn" "AroundOnce"
            OnceIn ioHi ->
              do
                i <- ioHi
                setUpLocked <- atomically $ tryLock status subNodes canLockSetup SetupRunning
                eho <-
                  if setUpLocked
                    then do
                      eho <- logRun' (TE.Hook TE.Once leadHookPos) (`setup` i)
                      atomically $ writeTMVar cache eho
                      eho
                        & either
                          ( \fp -> do
                              atomically $ writeTVar status AroundAbandoning
                              finally
                                ( do
                                    runSubNodes_ (Abandon fp) subNodes
                                    when (isJust teardown) $
                                      logAbandonned' (TE.Hook TE.Once TE.Teardown) fp
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
                        ( whenJust teardown $
                            \td -> do
                              locked <- atomically $ tryLock status subNodes canLockTeardown TeardownRunning
                              when locked $
                                finally
                                  (logRun_ (TE.Hook TE.Once TE.Teardown) (`td` ho))
                                  (atomically $ writeTVar status AroundDone)
                        )
                  )
      ---
      After{frequency, subNodes', after} ->
        case frequency of
          Each ->
            runSubNodes_ (EachIn nxtApply) subNodes'
           where
            nxtApply nxtAction =
              case hi of
                Abandon fp -> nxtAction (Left fp) >> abandonAfter fp
                EachIn{apply} -> apply nxtAction >> runAfter
                OnceIn ioHi -> ioHi >>= \i -> nxtAction (Right i) >> runAfter
                ThreadIn ioHi -> ioHi >>= \i -> nxtAction i >> (i & either abandonAfter (const runAfter))
             where
              runAfter = logRun_ (TE.Hook TE.Each TE.After) after
              abandonAfter = logAbandonned' (TE.Hook TE.Each TE.After)
          Thread -> case hi of
            Abandon fp -> runSubNodesAfter $ Just fp
            EachIn _ -> invalidTree "EachIn" "After Thread"
            OnceIn _ -> runSubNodesAfter Nothing
            ThreadIn _ -> runSubNodesAfter Nothing
           where
            runSubNodesAfter abandonned =
              do
                run <- runSubNodes hi subNodes'
                when run.hasRun $
                  abandonned
                    & maybe
                      (logRun_ (TE.Hook TE.Thread TE.After) after)
                      (logAbandonned' (TE.Hook TE.Thread TE.After))
      ---
      Around{frequency, setup, subNodes, teardown = mteardown} ->
        let
          leadHookPos = aroundLeadingHookPos mteardown
         in
          case frequency of
            Each ->
              runSubNodes_ (EachIn nxtApply) subNodes
             where
              nxtApply nxtAction =
                case hi of
                  Abandon fp -> runAbandon fp
                  EachIn{apply} ->
                    apply $ either runAbandon runNxt
                  OnceIn ioHi -> ioHi >>= runNxt
                  ThreadIn ethIoHi -> ethIoHi >>= either runAbandon runNxt
               where
                runAbandon fp = do
                  logAbandonned' (TE.Hook TE.Each leadHookPos) fp
                  nxtAction . Left $ fp
                  whenJust mteardown $
                    const $
                      logAbandonned' (TE.Hook TE.Each TE.Teardown) fp

                runNxt :: hi -> IO ()
                runNxt hki =
                  do
                    eho <- logRun' (TE.Hook TE.Each leadHookPos) (`setup` hki)
                    nxtAction eho
                    whenJust mteardown $
                      \teardown' ->
                        eho
                          & either
                            (logAbandonned' $ TE.Hook TE.Each TE.Teardown)
                            (\ho -> logRun_ (TE.Hook TE.Each TE.Teardown) (`teardown'` ho))
            Thread ->
              let
                runThreadAround ioHo hoVar =
                  finally
                    (runSubNodes_ (ThreadIn ioHo) subNodes)
                    ( do
                        whenJust mteardown $
                          \td -> do
                            mho <- atomically $ tryReadTMVar hoVar
                            -- if mho is Nothing then setup was not run (empty subnodes)
                            whenJust mho $
                              either
                                (logAbandonned' (TE.Hook TE.Thread TE.Teardown))
                                (\ho -> logRun_ (TE.Hook TE.Thread TE.Teardown) (`td` ho))
                    )
               in
                case hi of
                  Abandon fp -> do
                    hoVar <- newEmptyTMVarIO
                    runThreadAround (hkOutSingleton hoVar) hoVar
                   where
                    hkOutSingleton hov = do
                      mho <- atomically $ tryReadTMVar hov
                      mho
                        & maybe
                          ( do
                              let ab = Left fp
                              atomically $ putTMVar hov ab
                              pure ab
                          )
                          pure
                  EachIn{} -> invalidTree "EachIn" "Around Thread"
                  OnceIn ioHi -> do
                    -- Action can't be run until its actually needed by a test.
                    -- There is a possibilty of the hook enclosing an empty or
                    -- saturated subNode list. plain old laziness might be enough
                    hoVar <- newEmptyTMVarIO
                    runThreadAround (hkOutSingleton hoVar) hoVar
                   where
                    hkOutSingleton hov = do
                      mho <- atomically $ tryReadTMVar hov
                      mho
                        & maybe
                          ( do
                              hi'' <- ioHi
                              ho <- logRun' (TE.Hook TE.Thread leadHookPos) (`setup` hi'')
                              atomically $ putTMVar hov ho
                              pure ho
                          )
                          pure
                  ThreadIn ioeHi -> do
                    hoVar <- newEmptyTMVarIO
                    let ioHo = hkOutSingleton hoVar
                    runThreadAround ioHo hoVar
                   where
                    hkOutSingleton hov = do
                      mho <- atomically $ tryReadTMVar hov
                      mho
                        & maybe
                          ( do
                              ethi <- ioeHi
                              ho <-
                                either
                                  (\fp -> logAbandonned' (TE.Hook TE.Thread leadHookPos) fp >> pure (Left fp))
                                  (\hi'' -> logRun' (TE.Hook TE.Thread leadHookPos) (`setup` hi''))
                                  ethi
                              atomically $ putTMVar hov ho
                              pure ho
                          )
                          pure

      ---
      Test{path, tests} ->
        case hi of
          Abandon fp -> runTests (`runTestItem` Left fp)
          EachIn{apply} -> runTests (apply . runTestItem)
          OnceIn ioHi -> ioHi >>= \hii -> runTests (`runTestItem` Right hii)
          ThreadIn ethIoHi -> ethIoHi >>= \ehi -> runTests (`runTestItem` ehi)
       where
        runTests :: (P.TestItem IO hi -> IO ()) -> IO ()
        runTests actn = void $ runChildQ Sequential actn (const $ pure Runnable) tests

        runTestItem :: P.TestItem IO hi -> Either FailPoint hi -> IO ()
        runTestItem
          tstItm =
            either
              (logAbandonned lgr (mkTestPath tstItm) TE.Test)
              (void . logRun lgr (mkTestPath tstItm) TE.Test . tstItm.action sink)

        mkTestPath :: P.TestItem IO hi -> L.ExePath
        mkTestPath P.TestItem{id, title = ttl} = L.ExePath $ AE.TestPath{id, title = ttl} : coerce path

-- user facing (logged) hook position varies depending on if there is a teardown
-- or not. teardown exists => Setup, teardown does not exist => Before
aroundLeadingHookPos :: Maybe a -> TE.HookPos
aroundLeadingHookPos {- Maybe teardown -} = maybe TE.Before (const TE.Setup)

data NodeIn hi
  = Abandon FailPoint
  | OnceIn (IO hi)
  | ThreadIn (IO (Either FailPoint hi))
  | EachIn
      {apply :: (Either FailPoint hi -> IO ()) -> IO ()}

tryLock :: TVar s -> ChildQ a -> (s -> CanRun -> Bool) -> s -> STM Bool
tryLock hs cq canLock lockStatus =
  do
    s <- readTVar hs
    qs <- readTVar cq.status
    let cl = canLock s qs
    when cl $
      writeTVar hs lockStatus
    pure cl

logRun :: Logger -> L.ExePath -> TE.SuiteEvent -> IO b -> IO (Either L.FailPoint b)
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