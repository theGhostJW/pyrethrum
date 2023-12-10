--  should be able to remove this in later versions of GHC
-- https://gitlab.haskell.org/ghc/ghc/-/issues/21443
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Internal.SuiteRuntime where

import qualified Internal.RunTimeLogging as L
import qualified Core as C
import qualified DSL.Internal.ApEvent as AE
import PyrethrumExtras (catchAll, txt, (?))
import UnliftIO (
  bracket,
  concurrently_,
  finally,
  forConcurrently_,
  newIORef,
  writeTMVar,
 )
import UnliftIO.Concurrent ( myThreadId )
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
import qualified Internal.ThreadEvent as TE
import Internal.RunTimeLogging (FailPoint)
import qualified Internal.ThreadEvent as F (Frequency (..))
import qualified Prepare as C
import qualified Prepare as P

newtype ThreadCount = ThreadCount Int
  deriving (Show)

executeNew :: (C.Config rc, C.Config tc) => ThreadCount -> L.LogControls m L.ExePath AE.ApEvent -> C.ExeParams [] rc tc effs -> IO ()
executeNew
  (ThreadCount maxThreads)
  L.LogControls
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
            xtree <- mkXTreeNew (L.ExePath []) nodeList
            executeNodesNew sink xtree maxThreads
        )
        stopWorker

executeNodesNew :: (TE.ThreadEvent L.ExePath AE.ApEvent -> IO ()) -> ChildQ (ExeTreeNew ()) -> Int -> IO ()
executeNodesNew sink nodes maxThreads =
  do
    rootLogger <- newLogger
    finally
      ( rootLogger L.StartExecution
          >> forConcurrently_
            thrdTokens
            ( const do
                logger <- newLogger
                runChildQ Concurrent (runNodeNew logger (OnceIn (pure ())) ) canRunXTree nodes
            )
      )
      (rootLogger L.EndExecution)
 where
  thrdTokens = replicate maxThreads True
  newLogger = L.mkLogger sink <$> UnliftIO.newIORef (-1) <*> myThreadId

data NFrequency = Each | Thread
  deriving (Show, Eq)

data ExeTreeNew hi where
  AfterOnce ::
    { path :: L.ExePath
    , status' :: TVar AfterStatus
    , subNodes' :: ChildQ (ExeTreeNew hi)
    , after :: P.ApEventSink -> IO ()
    } ->
    ExeTreeNew hi
  AroundOnce ::
    { path :: L.ExePath
    , setup :: P.ApEventSink -> hi -> IO ho
    , status :: TVar AroundStatus
    , cache :: TMVar (Either L.FailPoint ho)
    , subNodes :: ChildQ (ExeTreeNew ho)
    , teardown :: Maybe (P.ApEventSink -> ho -> IO ())
    } ->
    ExeTreeNew hi
  After ::
    { path :: L.ExePath
    , frequency :: NFrequency
    , subNodes' :: ChildQ (ExeTreeNew hi)
    , after :: P.ApEventSink -> IO ()
    } ->
    ExeTreeNew hi
  Around ::
    { path :: L.ExePath
    , frequency :: NFrequency
    , setup :: P.ApEventSink -> hi -> IO ho
    , subNodes :: ChildQ (ExeTreeNew ho)
    , teardown :: Maybe (P.ApEventSink -> ho -> IO ())
    } ->
    ExeTreeNew hi
  Test ::
    { path :: L.ExePath
    , title :: Text
    , tests :: ChildQ (P.TestItem IO hi)
    } ->
    ExeTreeNew hi

mkXTreeNew :: L.ExePath -> NonEmpty (P.PreNode IO NonEmpty hi) -> IO (ChildQ (ExeTreeNew hi))
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
    path = L.ExePath $ pn.path : xpth.unExePath

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
      subNodes <- mkXTreeNew path subNodes'
      pure
        $ Around
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

data ExeIn oi ti tsti = ExeIn
  { onceIn :: oi
  , threadIn :: ti
  , tstIn :: tsti
  }

type Logger = L.EngineEvent L.ExePath AE.ApEvent -> IO ()

logAbandonnedNew :: Logger -> L.ExePath -> TE.EventType -> L.FailPoint -> IO ()
logAbandonnedNew lgr p e a =
  lgr
    $ L.ParentFailure
      { loc = p
      , eventType = e
      , parentLoc = a.path
      , parentEventType = a.eventType
      }

logReturnFailureNew :: Logger -> L.ExePath -> TE.EventType -> SomeException -> IO (Either L.FailPoint b)
logReturnFailureNew lgr p et e =
  do
    lgr (L.mkFailure p et e)
    pure . Left $ L.FailPoint p et

data CanAbandon = None | Partial | All
  deriving (Show, Eq)

canAbandon :: ExeTreeNew hi -> STM CanAbandon
canAbandon = \case
  AfterOnce{status'} -> do
    s <- readTVar status'
    pure $ s & \case
      AfterQPending -> All
      AfterQRunning -> Partial
      AfterRunning -> None
      AfterAbandoning -> None
      AfterDone -> None
  AroundOnce{status} -> do
    s <- readTVar status
    pure $ s & \case
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

runNodeNew ::
  forall hi.
  Logger ->
  NodeIn hi ->
  ExeTreeNew hi ->
  IO ()
runNodeNew lgr hi xt =
  let
    logRun' :: TE.EventType -> (P.ApEventSink -> IO b) -> IO (Either L.FailPoint b)
    logRun' et action = logRun lgr xt.path et (action sink)

    logRun_ :: TE.EventType -> (P.ApEventSink -> IO b) -> IO ()
    logRun_ et action = void $ logRun' et action

    logAbandonned' = logAbandonnedNew lgr xt.path
    -- logAbandonnedParent = logAbandonnedNew lgr

    runSubNodes :: forall hi'. NodeIn hi' -> ChildQ (ExeTreeNew hi') -> IO QElementRun
    runSubNodes hi'' = runChildQ Concurrent (runNodeNew lgr hi'') canRunXTree

    runSubNodes_ :: forall hi'. NodeIn hi' -> ChildQ (ExeTreeNew hi') -> IO ()
    runSubNodes_ n = void . runSubNodes n

    -- tree generation is restricted by typeclasses so unless the typeclass constrint implmentation is wrong
    -- execution trees with invalid structure (Thread or Once depending on Each) should never be generated.
    shouldNeverHappen :: Text -> IO ()
    shouldNeverHappen cst = bug @Void . error $ "EachIn should not be passed to: " <> cst <> " " <> txt xt.path

    sink = lgr . L.ApEvent
   in
    case xt of
      -- as we know tree shaking has been executed prior to running we can assume we
      -- always need to execute once hooks if the status is correct. There is no
      -- possibility of empty subnodes due to tree shaking.
      AfterOnce
        { status'
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
              ( let
                  runAfter mAbandon = do
                    let lockStatus = isJust mAbandon ? AfterAbandoning $ AfterRunning
                    locked <- atomically $ tryLock status' subNodes' canLockAfterOnce lockStatus
                    when locked
                      $ finally
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
                      EachIn _ -> shouldNeverHappen "AfterOnce"
                      ThreadIn _ -> shouldNeverHappen "AfterOnce"
                      OnceIn _ -> runAfter Nothing
              )
      -- as we know tree shaking has been executed prior to running we can assume we
      -- always need to execute once hooks if the status is correct. There is no
      -- possibility of empty subnodes due to tree shaking.
      ao@AroundOnce{setup, status, cache, subNodes, teardown} ->
        case hi of
          Abandon fp -> do
            setUpLocked <- atomically $ do
              ca <- canAbandon ao
              let locked = ca == All
              when locked
                $ writeTVar status AroundAbandoning
              pure locked
            when setUpLocked
              $ logAbandonned' (TE.Hook TE.Once TE.Setup) fp
            finally
              (runSubNodes_ (Abandon fp) subNodes)
              ( -- only AbandonOld teardown if setup has not started
                when setUpLocked $ do
                  finally
                    (logAbandonned' (TE.Hook TE.Once TE.Teardown) fp)
                    (atomically $ writeTVar status AroundDone)
              )
          EachIn _ -> shouldNeverHappen "AroundOnce"
          ThreadIn _ -> shouldNeverHappen "AroundOnce"
          OnceIn ioHi ->
            do
              i <- ioHi
              setUpLocked <- atomically $ tryLock status subNodes canLockSetup SetupRunning
              eho <-
                if setUpLocked
                  then do
                    eho <- logRun' (TE.Hook TE.Once TE.Setup) (`setup` i)
                    atomically $ writeTMVar cache eho
                    eho
                      & either
                        ( \fp -> do
                            atomically $ writeTVar status AroundAbandoning
                            finally
                              ( do
                                  runSubNodes_ (Abandon fp) subNodes
                                  when (isJust teardown)
                                    $ logAbandonned' (TE.Hook TE.Once TE.Teardown) fp
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
            EachIn _ -> shouldNeverHappen "After Thread"
            OnceIn _ -> runSubNodesAfter Nothing
            ThreadIn _ -> runSubNodesAfter Nothing
           where
            runSubNodesAfter abandonned =
              do
                run <- runSubNodes hi subNodes'
                when run.hasRun
                  $ abandonned
                  & maybe
                    (logRun_ (TE.Hook TE.Thread TE.After) after)
                    (logAbandonned' (TE.Hook TE.Thread TE.After))
      ---
      Around{frequency, setup, subNodes, teardown = mteardown} ->
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
                logAbandonned' (TE.Hook TE.Each TE.Setup) fp
                nxtAction . Left $ fp
                whenJust mteardown
                  $ const
                  $ logAbandonned' (TE.Hook TE.Each TE.Teardown) fp
              runNxt hki =
                bracket
                  (logRun' (TE.Hook TE.Each TE.Setup) (`setup` hki))
                  nxtAction
                  ( \eho ->
                      whenJust mteardown
                        $ \teardown' ->
                          eho
                            & either
                              (logAbandonned' (TE.Hook TE.Each TE.Teardown))
                              (\ho -> logRun_ (TE.Hook TE.Each TE.Teardown) (`teardown'` ho))
                  )
          Thread ->
            let
              runThreadAround ioHo hoVar =
                finally
                  (runSubNodes_ (ThreadIn ioHo) subNodes)
                  ( do
                      whenJust mteardown
                        $ \td -> do
                          mho <- atomically $ tryReadTMVar hoVar
                          -- if mho is Nothing then setup was not run (empty subnodes)
                          whenJust mho
                            $ either
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

                EachIn{} -> shouldNeverHappen "Around Thread"
                OnceIn ioHi -> do
                  -- Action can't be run until its actually needed by a test.
                  -- There is a possibilty of the hook enclosing an empty or
                  -- saturated subNode list. plain old laziness might be enough
                  -- TODO: test this
                  hoVar <- newEmptyTMVarIO
                  runThreadAround (hkOutSingleton hoVar) hoVar
                 where
                  hkOutSingleton hov = do
                    mho <- atomically $ tryReadTMVar hov
                    mho
                      & maybe
                        ( do
                            hi'' <- ioHi
                            ho <- logRun' (TE.Hook TE.Thread TE.Setup) (`setup` hi'')
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
                                (\fp -> logAbandonned' (TE.Hook TE.Thread TE.Setup) fp >> pure (Left fp))
                                (\hi'' -> logRun' (TE.Hook TE.Thread TE.Setup) (`setup` hi''))
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
              (logAbandonnedNew lgr (mkTestPath tstItm) TE.Test)
              (void . logRun lgr (mkTestPath tstItm) TE.Test . tstItm.action sink)

        mkTestPath :: P.TestItem IO hi -> L.ExePath
        mkTestPath P.TestItem{id, title = ttl} = L.ExePath $ AE.TestPath{id, title = ttl} : path.unExePath

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
    when cl
      $ writeTVar hs lockStatus
    pure cl


logRun :: Logger -> L.ExePath -> TE.EventType -> IO b -> IO (Either L.FailPoint b)
logRun lgr path evt action = do
  lgr $ L.Start evt path
  finally
    ( catchAll
        -- TODO :: test for strictness issues esp with failing thread hook
        -- eg returns handle and handle is closed
        (Right <$> action)
        (logReturnFailureNew lgr path evt)
    )
    (lgr $ L.End evt path)