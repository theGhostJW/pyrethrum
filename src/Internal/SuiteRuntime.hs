module Internal.SuiteRuntime where

import Core qualified as C
import DSL.Internal.ApEvent qualified as AE
import Internal.RunTimeLogging (FailPoint)
import Internal.RunTimeLogging qualified as L
import Internal.ThreadEvent hiding (Test)
import Internal.ThreadEvent qualified as TE 
import PyrethrumExtras (catchAll, debugf', toS, txt, (?))
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
import qualified Prepare as P

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
  Test ::
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
          pure $ Test{path, title = c.title, tests = cq}
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

logAbandonned :: Logger -> L.ExePath -> SuiteEvent -> L.FailPoint -> IO ()
logAbandonned lgr p e a =
  lgr $
    L.ParentFailure
      { loc = p
      , suiteEvent = e
      , failLoc = a.path
      , failSuiteEvent = a.suiteEvent
      }

logReturnFailure :: Logger -> L.ExePath -> SuiteEvent -> SomeException -> IO (Either L.FailPoint b)
logReturnFailure lgr p et e =
  do
    lgr (L.mkFailure p et e)
    pure . Left $ L.FailPoint p et

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
  Test{tests} -> qStatus tests
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
  Test{tests} -> qStatus tests
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
  BeforeAbandoning -> False

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
  forall hi ho.
  Logger ->
  NodeIn hi ->
  ExeTree ho ->
  IO ()
runNode lgr hi xt =
  runNode' hi xt
 where
  logRun' :: SuiteEvent -> (P.ApEventSink -> IO b) -> IO (Either L.FailPoint b)
  logRun' et action = logRun lgr xt.path et (action sink)

  logRun_ :: SuiteEvent -> (P.ApEventSink -> IO b) -> IO ()
  logRun_ et action = void $ logRun' et action

  logAbandonned' = logAbandonned lgr xt.path
  -- logAbandonnedParent = logAbandonnedNew lgr

  runSubNodes :: forall hi' ho'. NodeIn hi' -> ChildQ (ExeTree ho') -> IO QElementRun
  runSubNodes hi'' = runChildQ Concurrent (runNode lgr hi'') canRunXTree

  runSubNodes_ :: forall hi' ho'. NodeIn hi' -> ChildQ (ExeTree ho') -> IO ()
  runSubNodes_ n = void . runSubNodes n


  -- tree generation is restricted by typeclasses so unless the typeclass constraint implmentation is wrong
  -- execution trees with invalid structure (Thread or Once depending on Each, or Once depending on Thread)
  -- should never be generated.
  -- The only way these errors could be thrown is from unit testing code that generates an invalid testsuite
  -- via lower level constructors where there are no typeclass contraints apply
  invalidTree :: Text -> Text -> IO ()
  invalidTree input cst = bug @Void . error $ input <> " >>> should not be passed to >>> " <> cst <> "\n" <> txt xt.path

  sink :: P.ApEventSink
  sink = lgr . L.ApEvent

  runNode' :: forall hi' ho'. NodeIn hi' -> ExeTree ho' -> IO ()
  runNode' i t =
     run  i t
    where
    abandonSubs :: forall a. ChildQ (ExeTree a) -> IO ()
    abandonSubs = runSubNodes_ i

    runTests :: (P.Test IO hi -> IO ()) -> ChildQ (P.Test IO hi) -> IO ()
    runTests actn tests = void $ runChildQ Sequential actn (const $ pure Runnable) tests

    mkTestPath :: P.Test IO hi -> L.ExePath -> L.ExePath
    mkTestPath P.TestItem{id, title = ttl} fxPath = L.ExePath $ AE.TestPath{id, title = ttl} : coerce fxPath

    
    run :: NodeIn hi' -> ExeTree ho' -> IO ()
    run =
     \cases
      -- For Once* we assume tree shaking has been executed prior to execution.
      -- There is no possibility of empty subnodes due to tree shaking, so these hooks will always
      -- need to be run

      -- abandon -> onceBefore
      (Abandon fp) ob@OnceBefore{subNodes} -> do
        (beforeLocked, canAbandonQ) <- atomically $ do
          ca <- canAbandon ob
          let locked = ca == All
          when locked $
            writeTVar ob.beforeStatus BeforeAbandoning
          pure (locked, ca /= None)
        when beforeLocked $
          logAbandonned' (Hook Once Before) fp
        when canAbandonQ $
          abandonSubs subNodes

      -- abandon -> onceAround
      (Abandon fp) oa@OnceAround{subNodes, status} -> do
        setUpLocked <- atomically $ do
          ca <- canAbandon oa
          let locked = ca == All
          when locked $
            writeTVar status AroundAbandoning
          pure locked
        when setUpLocked $
          logAbandonned' (Hook Once Setup) fp
        finally
          (abandonSubs subNodes)
          ( 
            do
              locked <- atomically $ tryLock status subNodes canLockTeardown TeardownRunning
              when locked $
                finally
                  (logAbandonned' (Hook Once Teardown) fp)
                  (atomically $ writeTVar status AroundDone)
          )

      -- abandon -> onceAfter
      (Abandon fp) OnceAfter{subNodes', status'} -> do
        run <- atomically $ do
          s <- readTVar status'
          qs <- readTVar subNodes'.status
          when (s == AfterQPending) $
            writeTVar status' AfterQRunning
          pure $ canRunAfterOnce s qs
        when run $
          finally
            (abandonSubs subNodes')
            ( do
                locked <- atomically $ tryLock status' subNodes' canLockAfterOnce AfterAbandoning
                when locked $
                  finally
                    (logAbandonned' (Hook Once After) fp)
                    (atomically $ writeTVar status' AfterDone)
            )

      -- abandon -> threadBefore
      (Abandon fp) ThreadBefore{subNodes} -> 
        logAbandonned' (Hook Thread Before) fp >> abandonSubs subNodes

      -- abandon -> threadAround
      (Abandon fp) ThreadAround{subNodes} ->
        do
          logAbandonned' (Hook Thread Setup) fp
          abandonSubs subNodes
          logAbandonned' (Hook Thread Teardown) fp 

      -- abandon -> threadAfter
      (Abandon fp) ThreadAfter{subNodes'} ->
        abandonSubs subNodes' >> logAbandonned' (Hook Thread After) fp 

      -- abandon -> eachBefore
      (Abandon fp) EachBefore{subNodes} -> 
        logAbandonned' (Hook Each Before) fp >> abandonSubs subNodes

      -- abandon -> eachAround
      (Abandon fp) EachAround{subNodes} ->
        do
          logAbandonned' (Hook Each Setup) fp
          abandonSubs subNodes
          logAbandonned' (Hook Each Teardown) fp

      -- abandon -> eachAfter
      (Abandon fp) EachAfter{subNodes'} ->
          abandonSubs subNodes' >> logAbandonned' (Hook Each After) fp

      -- abandon -> test
      -- (Abandon fp) Test{path, tests} -> 
      --   let 
      --     abandonTest test = logAbandonned lgr (mkTestPath test path) TE.Test fp
      --   in
      --     runTests abnadonTest tests

       
       
--       do
--         hoVar <- newEmptyTMVarIO
--         runThreadAround (hkOutSingleton hoVar) hoVar
--        where
--         hkOutSingleton hov = do
--           mho <- atomically $ tryReadTMVar hov
--           mho
--             & maybe
--               ( do
--                   let ab = Left fp
--                   atomically $ putTMVar hov ab
--                   pure ab
--               )
--               pure
        -- uu
      -- (Abandon _) (ThreadAfter _ _ _)

      -- ### all invalid combos ### --
      EachIn{} OnceBefore{} -> invalidTree "EachIn" "OnceBefore"
      EachIn{} OnceAround{} -> invalidTree "EachIn" "OnceAround"
      EachIn{} OnceAfter{} -> invalidTree "EachIn" "OnceAfter"
      EachIn{} ThreadBefore{} -> invalidTree "EachIn" "ThreadBefore"
      EachIn{} ThreadAround{} -> invalidTree "EachIn" "ThreadAround"
      EachIn{} ThreadAfter{} -> invalidTree "EachIn" "ThreadAfter"
      ThreadIn{} OnceBefore{} -> invalidTree "ThreadIn" "OnceBefore"
      ThreadIn{} OnceAround{} -> invalidTree "ThreadIn" "OnceAround"
      ThreadIn{} OnceAfter{} -> invalidTree "ThreadIn" "OnceAfter"
  

--       EachIn _ _ -> invalidTree "EachIn" "AroundOnce"
--       ThreadIn _ -> invalidTree "ThreadIn" "AroundOnce"
--       OnceIn ioHi ->
--         do
--           i <- ioHi
--           setUpLocked <- atomically $ tryLock status subNodes canLockBefore SetupRunning
--           eho <-
--             if setUpLocked
--               then do
--                 eho <- logRun' (Hook Once Before) (`setup` i)
--                 atomically $ writeTMVar cache eho
--                 eho
--                   & either
--                     ( \fp -> do
--                         atomically $ writeTVar status BeforeAbandoning
--                         finally
--                           (runSubNodes_ (Abandon fp) subNodes)
--                           (atomically $ writeTVar status BeforeDone)
--                     )
--                     (const $ atomically $ writeTVar status BeforeQRunning)
--                 pure eho
--               else atomically (readTMVar cache)
--           whenRight_
--             eho
--             ( finally
--                 (\ho -> runSubNodes_ (OnceIn $ pure ho) subNodes)
--                 (atomically $ writeTVar status BeforeDone)
--             )
-- --
-- ao@OnceAround{setup, status, cache, subNodes, teardown} ->
--   case hi of

--     EachIn _ _ -> invalidTree "EachIn" "AroundOnce"
--     ThreadIn _ -> invalidTree "ThreadIn" "AroundOnce"
--     OnceIn ioHi ->
--       do
--         i <- ioHi
--         setUpLocked <- atomically $ tryLock status subNodes canLockSetup SetupRunning
--         eho <-
--           if setUpLocked
--             then do
--               eho <- logRun' (Hook Once leadHookPos) (`setup` i)
--               atomically $ writeTMVar cache eho
--               eho
--                 & either
--                   ( \fp -> do
--                       atomically $ writeTVar status AroundAbandoning
--                       finally
--                         ( do
--                             runSubNodes_ (Abandon fp) subNodes
--                             when (isJust teardown) $
--                               logAbandonned' (Hook Once Teardown) fp
--                         )
--                         (atomically $ writeTVar status AroundDone)
--                   )
--                   (const $ atomically $ writeTVar status AroundQRunning)
--               pure eho
--             else atomically (readTMVar cache)
--         whenRight_
--           eho
--           ( \ho ->
--               finally
--                 (runSubNodes_ (OnceIn $ pure ho) subNodes)
--                 ( \td -> do
--                     locked <- atomically $ tryLock status subNodes canLockTeardown TeardownRunning
--                     when locked $
--                       finally
--                         (logRun_ (Hook Once Teardown) (`td` ho))
--                         (atomically $ writeTVar status AroundDone)
--                 )
--           )
-- --
-- OnceAfter{status', subNodes', after} -> do
--   run <- atomically $ do
--     s <- readTVar status'
--     qs <- readTVar subNodes'.status
--     when (s == AfterQPending) $
--       writeTVar status' AfterQRunning
--     pure $ canRunAfterOnce s qs
--   when run $
--     finally
--       (runSubNodes_ hi subNodes')
--       ( let
--           runAfter mAbandon = do
--             let lockStatus = isJust mAbandon ? AfterAbandoning $ AfterRunning
--             locked <- atomically $ tryLock status' subNodes' canLockAfterOnce lockStatus
--             when locked $
--               finally
--                 ( mAbandon
--                     & maybe
--                       (void $ logRun' (Hook Once After) after)
--                       (logAbandonned' (Hook Once After))
--                 )
--                 (atomically $ writeTVar status' AfterDone)
--          in
--           do
--             case hi of
--               Abandon fp -> runAfter (Just fp)
--               EachIn _ _ -> invalidTree "EachIn" "AfterOnce"
--               ThreadIn _ -> invalidTree "ThreadIn" "AfterOnce"
--               OnceIn _ -> runAfter Nothing
--       )
-- -- ##### Thread* #####
-- -- NOTE: If we hit errors due to hooks firing on empty subtrees
-- -- look back to prior to restructure
-- -- https://github.com/theGhostJW/pyrethrum/commit/1f7f47dddd7434c0af509296f81f7d92acac3628

-- ThreadBefore{before, subNodes} ->
--   case hi of
--     abandon@(Abandon fp )-> do
--       logAbandonned' (Hook Thread Before) fp
--       runSubNodes_ abandon subnodes
--     EachIn _ _ -> invalidTree "EachIn" "ThreadBefore"
--     OnceIn ioHi -> runBefore $ Right ioHi
--     ThreadIn ioHi -> runBefore ioHi
--  where
--   runBefore ioEthHkIn = do
--     ethi <- ioEthHkIn
--     ehto <-
--       ethi
--         & either
--           (logAbandonned' (Hook Thread Before))
--           (\hi' -> logRun' (Hook Thread Before) (`before` hi'))
--     runSubNodes_ (ThreadIn $ either (pure . Left) (fmap Right) ehto) subnodes
-- --
-- ThreadAround{setup, subNodes, teardown} ->
--     case hi of
--       Abandon fp -> do
--         hoVar <- newEmptyTMVarIO
--         runThreadAround (hkOutSingleton hoVar) hoVar
--        where
--         hkOutSingleton hov = do
--           mho <- atomically $ tryReadTMVar hov
--           mho
--             & maybe
--               ( do
--                   let ab = Left fp
--                   atomically $ putTMVar hov ab
--                   pure ab
--               )
--               pure
--       EachIn{} -> invalidTree "EachIn" "Around Thread"
--       OnceIn ioHi -> do
--         -- Action can't be run until its actually needed by a test.
--         -- There is a possibilty of the hook enclosing an empty or
--         -- saturated subNode list. plain old laziness might be enough
--         hoVar <- newEmptyTMVarIO
--         runThreadAround (hkOutSingleton hoVar) hoVar
--        where
--         hkOutSingleton hov = do
--           mho <- atomically $ tryReadTMVar hov
--           mho
--             & maybe
--               ( do
--                   hi'' <- ioHi
--                   ho <- logRun' (Hook Thread leadHookPos) (`setup` hi'')
--                   atomically $ putTMVar hov ho
--                   pure ho
--               )
--               pure
--       ThreadIn ioeHi -> do
--         hoVar <- newEmptyTMVarIO
--         let ioHo = hkOutSingleton hoVar
--         runThreadAround ioHo hoVar
--        where
--         hkOutSingleton hov = do
--           mho <- atomically $ tryReadTMVar hov
--           mho
--             & maybe
--               ( do
--                   ethi <- ioeHi
--                   ho <-
--                     either
--                       (\fp -> logAbandonned' (Hook Thread leadHookPos) fp >> pure (Left fp))
--                       (\hi'' -> logRun' (Hook Thread leadHookPos) (`setup` hi''))
--                       ethi
--                   atomically $ putTMVar hov ho
--                   pure ho
--               )
--               pure
-- ---
-- After{frequency, subNodes', after} ->
--   case frequency of
--     Each ->
--       runSubNodes_ (EachIn nxtApply nxtAfter) subNodes'
--      where
--       runAfter = logRun_ (Hook Each After) after
--       abandonAfter = logAbandonned' (Hook Each After)
--       nxtAfter =
--         case hi of
--           Abandon fp -> abandonAfter fp
--           EachIn{after = hiAfter} -> runAfter >> hiAfter
--           OnceIn{} -> runAfter
--           ThreadIn ioHi -> ioHi >>= either abandonAfter (const runAfter)
--       nxtApply nxtAction =
--         case hi of
--           Abandon fp -> nxtAction (Left fp)
--           EachIn{apply} -> apply nxtAction
--           {- Defect Here
--            Template:
--             EachAfter 0
--               [
--                 EachAfter 0.0
--                  [
--                    Fixture 0.0.0
--                  ]

--               ]

--            Action Construction:
--             - EachAfter 0
--                -- apply => ((()-> ta)) >> EA.0
--             - EachAfter 0.0
--               -- apply => ((() -> ta)) >> EA.0) >> EA.0.0
--             - Test 0.0.0.0
--               -- apply => ((() -> Test 0.0.0.0)) >> EA.0) >> EA.0.0
--             - execution will be
--               1. Test 0.0.0.0
--               2. EA.0
--               3. EA.0.0
--             !! => Should be:
--               1. Test 0.0.0.0
--               2. EA.0.0
--               3. EA.0

--            Action Construction is Wrong
--           -}
--           OnceIn ioHi -> ioHi >>= \i -> nxtAction (Right i)
--           ThreadIn ioHi -> ioHi >>= \i -> nxtAction i
--     Thread -> case hi of
--       Abandon fp -> runSubNodesAfter $ Just fp
--       EachIn _ _ -> invalidTree "EachIn" "After Thread"
--       OnceIn _ -> runSubNodesAfter Nothing
--       ThreadIn _ -> runSubNodesAfter Nothing
--      where
--       runSubNodesAfter abandonned =
--         do
--           run <- runSubNodes hi subNodes'
--           when run.hasRun $
--             abandonned
--               & maybe
--                 (logRun_ (Hook Thread After) after)
--                 (logAbandonned' (Hook Thread After))
-- Around{frequency, setup, subNodes, teardown = mteardown} ->
--   let
--     leadHookPos = aroundLeadingHookPos mteardown
--    in
--     case frequency of
--       Each ->
--         runSubNodes_ (EachIn nxtApply emptyAfter) subNodes
--        where
--         emptyAfter = pure ()
--         nxtApply nxtAction =
--           case hi of
--             Abandon fp -> runAbandon fp
--             EachIn{apply} ->
--               apply $ either runAbandon runNxt
--             OnceIn ioHi -> ioHi >>= runNxt
--             ThreadIn ethIoHi -> ethIoHi >>= either runAbandon runNxt
--          where
--           runAbandon fp = do
--             logAbandonned' (Hook Each leadHookPos) fp
--             nxtAction . Left $ fp
--             whenJust mteardown $
--               const $
--                 logAbandonned' (Hook Each Teardown) fp

--           runNxt :: hi -> IO ()
--           runNxt hki =
--             do
--               eho <- logRun' (Hook Each leadHookPos) (`setup` hki)
--               nxtAction eho
--               whenJust mteardown $
--                 \teardown' ->
--                   eho
--                     & either
--                       (logAbandonned' $ Hook Each Teardown)
--                       (\ho -> logRun_ (Hook Each Teardown) (`teardown'` ho))
--       Thread ->
--         let
--           runThreadAround ioHo hoVar =
--             finally
--               (runSubNodes_ (ThreadIn ioHo) subNodes)
--               ( do
--                   whenJust mteardown $
--                     \td -> do
--                       mho <- atomically $ tryReadTMVar hoVar
--                       -- if mho is Nothing then setup was not run (empty subnodes)
--                       whenJust mho $
--                         either
--                           (logAbandonned' (Hook Thread Teardown))
--                           (\ho -> logRun_ (Hook Thread Teardown) (`td` ho))
--               )
--          in
--           case hi of
--             Abandon fp -> do
--               hoVar <- newEmptyTMVarIO
--               runThreadAround (hkOutSingleton hoVar) hoVar
--              where
--               hkOutSingleton hov = do
--                 mho <- atomically $ tryReadTMVar hov
--                 mho
--                   & maybe
--                     ( do
--                         let ab = Left fp
--                         atomically $ putTMVar hov ab
--                         pure ab
--                     )
--                     pure
--             EachIn{} -> invalidTree "EachIn" "Around Thread"
--             OnceIn ioHi -> do
--               -- Action can't be run until its actually needed by a test.
--               -- There is a possibilty of the hook enclosing an empty or
--               -- saturated subNode list. plain old laziness might be enough
--               hoVar <- newEmptyTMVarIO
--               runThreadAround (hkOutSingleton hoVar) hoVar
--              where
--               hkOutSingleton hov = do
--                 mho <- atomically $ tryReadTMVar hov
--                 mho
--                   & maybe
--                     ( do
--                         hi'' <- ioHi
--                         ho <- logRun' (Hook Thread leadHookPos) (`setup` hi'')
--                         atomically $ putTMVar hov ho
--                         pure ho
--                     )
--                     pure
--             ThreadIn ioeHi -> do
--               hoVar <- newEmptyTMVarIO
--               let ioHo = hkOutSingleton hoVar
--               runThreadAround ioHo hoVar
--              where
--               hkOutSingleton hov = do
--                 mho <- atomically $ tryReadTMVar hov
--                 mho
--                   & maybe
--                     ( do
--                         ethi <- ioeHi
--                         ho <-
--                           either
--                             (\fp -> logAbandonned' (Hook Thread leadHookPos) fp >> pure (Left fp))
--                             (\hi'' -> logRun' (Hook Thread leadHookPos) (`setup` hi''))
--                             ethi
--                         atomically $ putTMVar hov ho
--                         pure ho
--                     )
--                     pure

-- ---
-- Test{path, tests} ->
--   case hi of
--     Abandon fp -> runTests (`runTestItem` Left fp)
--     EachIn{apply, after} -> runTests (\i -> (apply $ runTestItem i) >> after)
--     OnceIn ioHi -> ioHi >>= \hii -> runTests (`runTestItem` Right hii)
--     ThreadIn ethIoHi -> ethIoHi >>= \ehi -> runTests (`runTestItem` ehi)
--  where
--   runTests :: (P.TestItem IO hi -> IO ()) -> IO ()
--   runTests actn = void $ runChildQ Sequential actn (const $ pure Runnable) tests

--   runTestItem :: P.TestItem IO hi -> Either FailPoint hi -> IO ()
--   runTestItem
--     tstItm =
--       either
--         (logAbandonned lgr (mkTestPath tstItm) Test)
--         (void . logRun lgr (mkTestPath tstItm) Test . tstItm.action sink)

--   mkTestPath :: P.TestItem IO hi -> L.ExePath
--   mkTestPath P.TestItem{id, title = ttl} = L.ExePath $ AE.TestPath{id, title = ttl} : coerce path

data NodeIn hi
  = Abandon FailPoint
  | OnceIn (IO hi)
  | ThreadIn (IO (Either FailPoint hi))
  | EachIn
      { apply :: (Either FailPoint hi -> IO ()) -> IO ()
      , after :: IO ()
      }

applyEach :: ((Either FailPoint hi -> IO ()) -> IO ()) -> IO () -> (Either FailPoint hi -> IO ()) -> IO ()
applyEach apply after test = apply test >> after

tryLock :: TVar s -> ChildQ a -> (s -> CanRun -> Bool) -> s -> STM Bool
tryLock hs cq canLock lockedStatus =
  do
    s <- readTVar hs
    qs <- readTVar cq.status
    let cl = canLock s qs
    when cl $
      writeTVar hs lockedStatus
    pure cl

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