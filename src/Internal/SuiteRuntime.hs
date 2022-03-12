-- {-# LANGUAGE NoStrictData #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE NoStrictData #-}
{-# LANGUAGE MagicHash #-}

module Internal.SuiteRuntime where

import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty))
--     MonadUnliftIO,
--     STM,
--     TMVar,
--     TQueue,
--     TVar,
--     atomically,

--     isEmptyTQueue,
--     newEmptyTMVarIO,
--     newTQueueIO,
--     newTVarIO,
--     putTMVar,
--     readTMVar,
--     readTQueue,
--     readTVar,
--     readTVarIO,

--     writeTQueue,
--     writeTVar, newTMVarIO, isEmptyTMVar,

import GHC.Exts
import qualified Internal.PreNode as PN
  ( CompletionStatus (Fault, Normal),
    HookStatus (..),
    PreNode (..),
    PreNodeRoot (..),
  )
import Pyrelude hiding (threadStatus, parent, ThreadRunning, ThreadStatus, atomically, bracket, forkFinally, newMVar, newTVarIO, readTVarIO, withMVar)
import Pyrelude.IO (hPutStrLn, putStrLn)
import UnliftIO
  ( Exception (displayException),
    bracket,
    catchAny,
    newMVar,
    newTMVar,
    pureTry,
    putMVar,
    tryAny,
  )
import UnliftIO.Concurrent as C (ThreadId, forkFinally, forkIO, takeMVar, threadDelay, withMVar)
import UnliftIO.STM
  ( STM,
    TMVar,
    TQueue,
    TVar,
    atomically,
    isEmptyTMVar,
    isEmptyTQueue,
    modifyTVar,
    newEmptyTMVar,
    newEmptyTMVarIO,
    newTMVarIO,
    newTQueueIO,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTQueue,
    readTVar,
    readTVarIO,
    takeTMVar,
    tryReadTQueue,
    writeTQueue,
    writeTVar,
  )
import qualified Prelude as PRL

data FixtureStatus
  = Pending
  | Starting
  | Active
  | Done PN.CompletionStatus
  | BeingKilled
  deriving (Show)

isComplete :: PN.HookStatus -> Bool
isComplete = \case
  PN.Complete _ -> True
  _ -> False

data HookExe i o where
  HookExe ::
    { hookParent :: HookExe i0 i,
      hookStatus :: IO (TVar PN.HookStatus),
      hook :: i -> IO o,
      hookResult :: IO (TMVar o),
      hookRelease :: Int -> o -> IO ()
    } ->
    HookExe i o

data NodeRoot o = NodeRoot
  { rootStatus :: IO (TVar PN.HookStatus),
    children :: [Node () o]
  }

data Node i o where
  Hook ::
    { hookParent :: Either i (Node i0 i),
      hookStatus :: IO (TVar PN.HookStatus),
      hook :: i -> IO o,
      hookResult :: IO (TMVar o),
      hookChildren :: [Node o o2],
      hookRelease :: Int -> o -> IO ()
    } ->
    Node i o
  Fixture ::
    { logStart :: IO (),
      fixParent :: Either i (Node i0 i),
      iterations :: [i -> IO ()],
      logEnd :: IO ()
    } ->
    Node i ()

data ThreadStatus
  = ThreadInitialising Int
  | ThreadRunning
  | ThreadDone
  deriving (Eq, Show)

data RunningThread = RunningThread
  { thread :: IO ThreadId,
    status :: TVar ThreadStatus
  }

data LoadedFixture = LoadedFixture
  { logStart :: IO (),
    fixStatus :: TVar FixtureStatus,
    iterations :: TVar [IO ()],
    activeThreads :: TVar [RunningThread],
    logEnd :: IO ()
  }

data IndexedFixture = IndexedFixture
  { index :: Int,
    fixture :: LoadedFixture
  }

{- TODO
  ~ gather fixtures
  ~ simple
  ~ fixture address
  ~ log start and end fixture (generate form test suite)
  ~ exceptions
  ~ killing
  ~ test with null iterations
-}

linkParents :: PN.PreNodeRoot o -> NodeRoot o
linkParents PN.PreNodeRoot {children} =
  NodeRoot
    { rootStatus = newTVarIO PN.Intitialising,
      children = linkParents' (Left ()) <$> children
    }

linkParents' :: Either o (Node i o) -> PN.PreNode o o' -> Node o o'
linkParents' parent = \case
  PN.Hook {hookStatus, hook, hookChildren, hookRelease} ->
    let h =
          Internal.SuiteRuntime.Hook
            { hookParent = parent,
              hookStatus = newTVarIO PN.Intitialising,
              hook = hook,
              hookResult = newEmptyTMVarIO,
              hookChildren = linkParents' (Right h) <$> hookChildren,
              hookRelease = hookRelease
            }
     in h
  PN.Fixture {logStart, iterations, logEnd} ->
    Internal.SuiteRuntime.Fixture
      { logStart = logStart,
        fixParent = parent,
        iterations = iterations,
        logEnd = logEnd
      }

isUninitialised :: PN.HookStatus -> Bool
isUninitialised = \case
  PN.Unintialised -> True
  PN.Intitialising -> False
  PN.Running -> False
  PN.Complete cs -> False
  PN.BeingMurdered -> False

tryLock :: TVar PN.HookStatus -> STM Bool
tryLock status =
  readTVar status
    >>= bool
      (pure False)
      (writeTVar status PN.Intitialising >> pure True)
      . isUninitialised

-- executeHook only to be run when want executeHook has set
-- staus to initalising should only ever run once per branch
-- TODO - test exception on output of branch parent and in resource aquisition
executeHook :: (Text -> IO ()) -> Node i o -> IO ()
executeHook db =
  \case
    Fixture {} -> pure ()
    Hook
      { hookParent,
        hookStatus,
        hook,
        hookResult,
        hookChildren,
        hookRelease
      } -> do
        input <- db "CALL PARENT LOCK EXECUTe hook" >> lockExecuteHook db hookParent
        bracket
          (hook input)
          (\o -> db "START HOOK RELEASE" >> hookRelease 1 o >> db "END HOOK RELEASE" {- TODO implemnt pass through timeout for release -})
          \hookOut -> do
            hkVal <- hookResult
            status <- hookStatus
            atomically $ do
              putTMVar hkVal hookOut
              -- Initialising -> Running
              writeTVar status PN.Running

lockExecuteHook :: (Text -> IO ()) -> Either o (Node i o) -> IO o
lockExecuteHook db parent =
  eitherf
    parent
    pure
    ( \case
        Fixture {} -> pure ()
        branch@Hook
          { hookParent,
            hookStatus,
            hook,
            hookResult,
            hookChildren,
            hookRelease
          } -> do
            bs <- hookStatus
            -- set status to initialising if not already running (tryLock)
            wantLaunch <- atomically $ tryLock bs
            db $ "HOOK LOCK >>> " <> txt wantLaunch
            when
              wantLaunch
              --  this writes hook result to the TVar
              (void $ executeHook db branch) -- forkIO
            hc' <- hookResult
            atomically $ readTMVar hc'
    )

loadFixture :: (Text -> IO ()) -> Either o (Node i o) -> [o -> IO ()] -> IO () -> IO () -> IO LoadedFixture
loadFixture db parent iterations logStart logEnd = do
  let loadIteration itr = do
        input <- lockExecuteHook db parent
        itr input

  fixStatus <- newTVarIO Pending
  iterations' <- newTVarIO (loadIteration <$> iterations)
  activeThreads' <- newTVarIO []
  pure $
    LoadedFixture
      { logStart = logStart,
        fixStatus = fixStatus,
        iterations = iterations',
        activeThreads = activeThreads',
        logEnd = logEnd
      }

--

mkFixtures :: (Text -> IO ()) -> Node i o -> [IO LoadedFixture]
mkFixtures db = \case
  Hook {hookChildren} -> hookChildren >>= mkFixtures db
  Fixture logStart parent iterations logEnd -> [loadFixture db parent iterations logStart logEnd]

data Executor = Executor
  { maxThreads :: Int,
    threadsInUse :: TVar Int,
    fixturesPending :: TQueue IndexedFixture,
    fixturesStarted :: TQueue IndexedFixture
  }

data Fork
  = Fork LoadedFixture
  | Pend
  | RunComplete

data ThreadStats = ThreadStats
  { maxThreads :: Int,
    inUse :: Int
  }
  deriving (Show)

data NoFixture
  = EmptyQueues
  | NoFixturesReady
  | NoThreadsAvailable ThreadStats
  deriving (Show)

data NoCandidate
  = EmptyQueue
  | CantUseAnyMoreThreads
  | InvalidFixtureInPendingList
  | Finished

data IterationRun = IterationRun
  { parentFixture :: LoadedFixture,
    iteration :: IO ()
  }

updateStatusReturnCompleted :: (Text -> IO ()) -> LoadedFixture -> STM Bool
updateStatusReturnCompleted
  db
  LoadedFixture
    { fixStatus,
      iterations,
      activeThreads
    } =
    let completionBlocked :: FixtureStatus -> Bool
        completionBlocked = \case
          Pending -> False
          Starting -> True
          Active -> False
          Done _ -> False
          BeingKilled -> True
        isDone :: FixtureStatus -> Bool
        isDone = \case
          Pending -> False
          Starting -> False
          Active -> False
          Done _ -> True
          BeingKilled -> False
     in do
          i <- readTVar iterations
          unsafeIOToSTM . db $ "EMPTY ITERATIONS: " <> txt (null i)
          a <- readTVar activeThreads
          unsafeIOToSTM . db $ "EMPTY ACTIVE THREADS: " <> txt (null a)
          s <- readTVar fixStatus
          unsafeIOToSTM . db $ "FIXTURE STATUS: " <> txt s
          let done = null i && null a && not (completionBlocked s)
              doneAlready = isDone s
              completed = done && not doneAlready
              newStatus = completed ? Done PN.Normal $ s
          writeTVar fixStatus newStatus
          pure completed

canForkThread :: FixtureStatus -> Bool
canForkThread = \case
  Pending -> True
  Starting -> False
  Active -> True
  Done cs -> False
  BeingKilled -> False

takeIteration :: LoadedFixture -> STM (Maybe IterationRun)
takeIteration fixture@LoadedFixture {iterations, activeThreads, fixStatus} = do
  itrs <- readTVar iterations
  activ <- readTVar activeThreads
  status <- readTVar fixStatus
  not (canForkThread status)
    ? pure Nothing
    $ case itrs of
      [] -> pure Nothing
      x : xs -> do
        writeTVar iterations xs
        pure (Just $ IterationRun fixture x)

nextActiveFixtureRemoveDone :: TQueue IndexedFixture -> STM (Maybe LoadedFixture)
nextActiveFixtureRemoveDone activeQ =
  let getNxt :: Maybe Int -> STM (Maybe LoadedFixture)
      getNxt mInitilIndex = do
        mfx <- tryReadTQueue activeQ
        maybef
          mfx
          (pure Nothing)
          \ifx@IndexedFixture {index = index', fixture = fixture@LoadedFixture {fixStatus}} ->
            let nxtInitial :: Maybe Int
                nxtInitial = mInitilIndex <|> Just index'

                reQu :: STM ()
                reQu = writeTQueue activeQ ifx

                reQuGetNxt :: STM (Maybe LoadedFixture)
                reQuGetNxt = reQu >> getNxt nxtInitial

                reQReturnThisFixture :: STM (Maybe LoadedFixture)
                reQReturnThisFixture = reQu $> Just fixture
             in -- if we are back where we started we are done
                mInitilIndex == Just index'
                  ? pure Nothing
                  $ do
                    status <- readTVar fixStatus
                    case status of
                      Pending -> reQReturnThisFixture
                      Starting -> reQuGetNxt
                      Active -> reQReturnThisFixture
                      -- done fixtures are not added to the back of the q
                      Done cs -> getNxt nxtInitial
                      -- just put at end of q and continue eventually this fixture
                      -- will become done and get kicked out of the queue above
                      BeingKilled -> reQuGetNxt
   in getNxt Nothing

-- returns the next fixture and puts on end of active fixture queue
nextFixture :: TQueue IndexedFixture -> TQueue IndexedFixture -> STM (Either NoFixture LoadedFixture)
nextFixture pendingQ activeQ =
  let notEmpty q = not <$> isEmptyTQueue q
   in do
        hasPending <- notEmpty pendingQ
        hasActive <- notEmpty activeQ
        if
            | hasPending ->
              do
                ifx <- readTQueue pendingQ
                writeTQueue activeQ ifx
                pure . Right $ fixture ifx
            | hasActive ->
              maybe (Left NoFixturesReady) Right
                <$> nextActiveFixtureRemoveDone activeQ
            | otherwise ->
              pure $ Left EmptyQueues

releaseThread :: TVar Int -> STM ()
releaseThread = flip modifyTVar (\i -> i - 1)

reserveThread :: Executor -> (Text -> IO ()) -> STM (Either ThreadStats ThreadStats)
reserveThread
  exe@Executor
    { maxThreads,
      threadsInUse
    }
  db = do
    used <- readTVar threadsInUse
    let reserved = used < maxThreads
        newUsed = reserved ? used + 1 $ used
        stats = ThreadStats {maxThreads = maxThreads, inUse = newUsed}
    --
    when reserved do
      unsafeIOToSTM (db "THREAD RESERVED")
      writeTVar threadsInUse newUsed
    --
    unless reserved do
      unsafeIOToSTM (db "NO THREADS AVAILABLE NONE RESERVED")

    pure $ (reserved ? Right $ Left) stats

isPending :: FixtureStatus -> Bool
isPending = \case
  Pending -> True
  Starting -> False
  Active -> False
  Done cs -> False
  BeingKilled -> False

isStarting :: FixtureStatus -> Bool
isStarting = \case
  Pending -> False
  Starting -> True
  Active -> False
  Done cs -> False
  BeingKilled -> False

setToStartedIfPending :: TVar FixtureStatus -> STM Bool
setToStartedIfPending fixStatus =
  do
    s <- readTVar fixStatus
    let ip = isPending s
    writeTVar fixStatus $ ip ? Starting $ s
    pure ip

setActiveIfStarting :: TVar FixtureStatus -> STM Bool
setActiveIfStarting fixStatus =
  do
    s <- readTVar fixStatus
    let is = isStarting s
    writeTVar fixStatus $ is ? Active $ s
    pure is

removeFinishedThreads :: [RunningThread] -> STM [RunningThread]
removeFinishedThreads rthds =
  let removeFinishedThreads' :: [RunningThread] -> [RunningThread] -> STM [RunningThread]
      removeFinishedThreads' accum = \case
        [] -> pure accum
        rt@RunningThread {status} : rts -> do
          s <- readTVar status
          s == ThreadRunning
            ? removeFinishedThreads' (rt : accum) rts
            $ removeFinishedThreads' accum rts
   in removeFinishedThreads' [] rthds

logError :: Text -> IO ()
logError t = print $ "Something went wrong with the runtime: " <> t

runFixture :: TVar ThreadStatus -> LoadedFixture -> (Text -> IO ()) -> IO ()
runFixture
  threadStatus
  fx@LoadedFixture
    { logStart,
      fixStatus,
      iterations,
      activeThreads,
      logEnd
    }
  db =
    let runIterations :: IO ()
        runIterations = do
          wantLogStart <- atomically $ setToStartedIfPending fixStatus
          db $ "RF - WantLogStart " <> txt wantLogStart
          if wantLogStart
            then do
              db "RF - Top WantLogStart"
              elst <- tryAny logStart
              eitherf
                elst
                ( \e -> do
                    db "RF - LogStartleft"
                    atomically $ writeTVar fixStatus $ Done (PN.Fault "Failed logging fixture start" e)
                    -- if we get here things have really screwed up ye old terminal as a last resort
                    logError $ "Failed logging fixture start\n" <> toS (displayException e)

                    catchAny
                      (db "RF - Loging Log End" >> logEnd)
                      (\e' -> logError $ "Failed logging fixture end\n" <> toS (displayException e'))
                )
                ( const $ do
                    db "RF - Rerun Iterations"
                    atomically $ setActiveIfStarting fixStatus
                    runIterations
                )
            else do
              mi <- atomically $ takeIteration fx
              maybe
                (db "RF - No Iterations Left" >> pure ())
                (\IterationRun {iteration} -> db "RF - Run IT" >> iteration >> runIterations)
                mi
     in do
          db "RF - BODY"
          db $ "RF - BODY THREAD POINTER IS: " <> unsafeAddr threadStatus
          s <- readTVarIO threadStatus
          db $ "RF - BODY THREAD STATUS: " <> txt s
          case s of
            ThreadInitialising _ -> C.threadDelay 1_000 >> runFixture threadStatus fx db
            ThreadRunning -> runIterations
            ThreadDone -> pure ()

-- Any is a type to which any type can be safely unsafeCoerced to.

-- A datatype that has the same layout as Word and so can be casted to it.
newtype Ptr' a = Ptr' a

-- Any is a type to which any type can be safely unsafeCoerced to.
aToWord# :: GHC.Exts.Any -> Word#
aToWord# a = let !mb = Ptr' a in case unsafeCoerce# mb :: Word of W# addr -> addr

unsafeAddr :: a -> Text
unsafeAddr a = txt $ I# (word2Int# (aToWord# (unsafeCoerce# a)))

forkFixtureThread :: TVar Int -> LoadedFixture -> (Text -> IO ()) -> IO ()
forkFixtureThread
  threadsInUse
  fx@LoadedFixture {activeThreads, logEnd}
  db = do
    db "III forkFixtureThread started"
    thrdStatus <- newTVarIO (ThreadInitialising 999)
    let tfx = forkFinally
          (db "III FIXTURE START" >> runFixture thrdStatus fx db)
          -- finally clean up
          \_ -> do
            fxDone <- atomically $ do
              -- set this threadstatus to done
              writeTVar thrdStatus ThreadDone
              -- remove all finished threads from active threads list
              ats <- readTVar activeThreads
              newAts <- removeFinishedThreads ats
              writeTVar activeThreads newAts
              -- decrement global threads in use
              upre <- readTVar threadsInUse
              unsafeIOToSTM (db "THREAD DONE")
              unsafeIOToSTM (db $ "BEFORE DECREMENT THREAD: " <> txt upre)
              releaseThread threadsInUse
              u <- readTVar threadsInUse
              unsafeIOToSTM (db $ "AFTER DECREMENT THREAD: " <> txt u)
              updateStatusReturnCompleted db fx
            db $ "FIXTURE DONE: " <> txt fxDone
            when
              fxDone
              logEnd

    db "III ABove Atomically"
    atomically $ do
      unsafeIOToSTM (db "III Read thread status")
      unsafeIOToSTM (db "III Adding thread status")
      modifyTVar activeThreads (RunningThread tfx thrdStatus :)
      sinitial <- readTVar thrdStatus
      unsafeIOToSTM (db $ "III Thread BEFORE RUNNING SET: " <> txt sinitial)
      writeTVar thrdStatus ThreadRunning
      s <- readTVar thrdStatus
      s' <- readTVar thrdStatus
      unsafeIOToSTM (db $ "III Thread RUNNING SET I: " <> txt s)
      unsafeIOToSTM (db $ "III Thread RUNNING SET: " <> txt s')
      unsafeIOToSTM (db $ "MAIN FORK THREAD POINTER IS: " <> unsafeAddr thrdStatus)
      unsafeIOToSTM (db $ "MAIN FORK THREAD POINTER II IS: " <> unsafeAddr thrdStatus)
    id' <- tfx
    db $ "III DONE forkFixtureThread: " <> txt id'

execute' :: Executor -> (Text -> IO ()) -> IO ()
execute'
  exe@Executor
    { maxThreads,
      threadsInUse,
      fixturesPending,
      fixturesStarted
    }
  db = do
    eNxtFx <-
      atomically $ do
        eStats <- reserveThread exe db
        eitherf
          eStats
          (pure . Left . NoThreadsAvailable)
          (const $ unsafeIOToSTM (db "III Execute Nxt Fixture") >> nextFixture fixturesPending fixturesStarted)

    let recurse = execute' exe db
        waitRecurse = C.threadDelay 10_000 >> recurse
        threadRelease = db "THREAD RELEASE" >> atomically (releaseThread threadsInUse)

    eitherf
      eNxtFx
      ( \case
          -- both the pending and active que of fixtures
          -- are empty so we are done
          -- thread was reserved so release thread
          -- has no effect because app is about to end but may
          -- later if multi-process runs are implemented and thread release implementation
          -- is changed
          EmptyQueues -> threadRelease >> db "EmptyQ - EXECUTION DONE" >> pure ()
          -- all the fixtures are not in a state to run any more threads
          -- eg being killed. We expect they may become available later of be finished
          -- and removed from the active que leading to empty ques we wait and try again
          -- thread was reserved so release thread
          NoFixturesReady -> threadRelease >> db "NoFixturesReady" >> waitRecurse
          -- all threads in use wait try again
          -- no threads reserved so none need to be released
          nt@NoThreadsAvailable {} -> db (txtPretty nt) >> waitRecurse
      )
      (\f -> forkFixtureThread threadsInUse f db >> recurse)

qFixture :: TQueue IndexedFixture -> (Int, LoadedFixture) -> STM ()
qFixture q (idx, fixture) = writeTQueue q $ IndexedFixture idx fixture

execute :: Int -> NodeRoot o -> IO ()
execute maxThreads NodeRoot{rootStatus, children} = do
  hSetBuffering stdout NoBuffering
  -- h <- openFile "C:\\Pyrethrum\\log.log" WriteMode
  -- hSetBuffering h NoBuffering

  lock <- newMVar ()

  let wantDebug = True
      db :: Text -> IO ()
      db msg = wantDebug ? withMVar lock (const $ putStrLn (toS msg)) $ pure ()

  -- let db = printer
  -- putStrLn $ toS s
  -- hPutStrLn h s

  fxs <- sequence $ children >>= mkFixtures db 
  let idxFxs = zip [0 ..] fxs
  -- create queue
  pendingQ <- newTQueueIO
  runningQ <- newTQueueIO
  -- load all fixtures to pending queue
  atomically $ traverse_ (qFixture pendingQ) idxFxs
  initialThreadsInUse <- newTVarIO 0
  -- finally
  execute' (Executor maxThreads initialThreadsInUse pendingQ runningQ) db

-- (hClose h)
