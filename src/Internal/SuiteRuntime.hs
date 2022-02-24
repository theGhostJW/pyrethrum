-- {-# LANGUAGE NoStrictData #-}

module Internal.SuiteRuntime where

import Data.Function (const, ($), (&), (.))
import Data.Sequence (Seq (Empty))
import Internal.PreNode
  ( CompletionStatus (Fault, Normal),
    HookStatus (..), PreNode,
  )
import qualified Internal.PreNode as PN
import Pyrelude (Alternative ((<|>)), Identity, ListLike, SomeException, Text, bool, eitherf, finally, fromMaybe, isJust, mapLeft, maybef, newTVar, threadDelay, throw, toS, traverse_, unless, unlessJust, uu, void, when, ($>), (?))
import UnliftIO
  ( Exception (displayException),
    MonadUnliftIO,
    STM,
    TMVar,
    TQueue,
    TVar,
    atomically,
    bracket,
    catchAny,
    isEmptyTQueue,
    newTQueueIO,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTQueue,
    readTVar,
    readTVarIO,
    tryAny,
    writeTQueue,
    writeTVar,
  )
import UnliftIO.Concurrent as C (ThreadId, forkFinally, forkIO, threadDelay)
import UnliftIO.STM
  ( STM,
    TMVar,
    TQueue,
    TVar,
    atomically,
    isEmptyTQueue,
    modifyTVar,
    newTQueueIO,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTQueue,
    readTVar,
    readTVarIO,
    tryReadTQueue,
    writeTQueue,
    writeTVar,
  )
import Prelude

data FixtureStatus
  = Pending
  | Starting
  | Active
  | Done CompletionStatus
  | BeingKilled
  deriving (Show)

isComplete :: HookStatus -> Bool
isComplete = \case
  Complete _ -> True
  _ -> False

data HookExe i o where 
  HookExe ::  { 
     hookParent :: HookExe i0 i,
      hookStatus :: IO (TVar HookStatus),
      hook :: i -> IO o,
      hookResult :: IO (TMVar o),
      hookRelease :: Int -> o -> IO ()
    }
    -> HookExe i o 

data Node i o where
  Root ::
    { rootStatus :: IO (TVar hookStatus),
      rootChildren :: [Node () o]
    } ->
    Node () ()
  Hook ::
    { hookParent :: Node i0 i,
      hookStatus :: IO (TVar HookStatus),
      hook :: i -> IO o,
      hookResult :: IO (TMVar o),
      hookChildren :: [Node o o2],
      hookRelease :: Int -> o -> IO ()
    } ->
    Node i o
  Fixture ::
    MonadUnliftIO m =>
    { logStart :: IO (),
      fixParent :: Node i0 i,
      iterations :: [i -> IO ()],
      logEnd :: IO ()
    } ->
    Node i ()

data ThreadStatus
  = ThreadInitialising
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

linkParents :: Node i o -> PreNode pi i -> Node i o
linkParents parent = \case
  root@PN.Root rootChildren -> 
    let
        status = newTVarIO Intitialising 
        unusedParent = Root status []
    in
      Root status (linkParents root <$> rootChildren)
  Hook { hookParent, hookStatus, hook, hookResult, hookChildren, hookRelease } ->
    Hook parent hookStatus hook hookResult hookChildren hookRelease
  Fixture { logStart,
      fixParent,
      iterations,
      logEnd
    } -> Fixture

isUninitialised :: HookStatus -> Bool
isUninitialised = \case
  Unintialised -> True
  Intitialising -> False
  Running -> False
  Complete cs -> False
  BeingMurdered -> False

tryLock :: TVar HookStatus -> STM Bool
tryLock status =
  readTVar status
    >>= bool
      (pure False)
      (writeTVar status Intitialising >> pure True)
      . isUninitialised

-- executeHook only to be run when want executeHook has set
-- staus to initalising should only ever run once per branch
-- TODO - test exception on output of branch parent and in resource aquisition
executeHook :: Node i o -> IO ()
executeHook =
  \case
    Root {} -> pure ()
    Fixture {} -> pure ()
    Hook
      { hookParent,
        hookStatus,
        hook,
        hookResult,
        hookChildren,
        hookRelease
      } -> do
        input <- lockExecuteHook hookParent
        bracket
          (hook input)
          (hookRelease 1 {- TODO implemnt pass through timeout for release -})
          \hookOut -> do
            hkVal <- hookResult
            status <- hookStatus
            atomically $ do
              putTMVar hkVal hookOut
              -- Initialising -> Running
              writeTVar status Running

lockExecuteHook :: Node i o -> IO o
lockExecuteHook = \case
  Root {} -> pure ()
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
      when
        wantLaunch
        --  this writes hook result to the TVar
        (void $ executeHook branch) -- forkIO
      hc' <- hookResult
      atomically $ readTMVar hc'

loadFixture :: Node i o -> [o -> IO ()] -> IO () -> IO () -> IO LoadedFixture
loadFixture parent iterations logStart logEnd = do
  let loadIteration itr = do
        input <- lockExecuteHook parent
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

mkFixtures :: Node i o -> [IO LoadedFixture]
mkFixtures = \case
  Root {rootChildren} -> rootChildren >>= mkFixtures
  Hook {hookChildren} -> hookChildren >>= mkFixtures
  Fixture logStart parent iterations logEnd -> [loadFixture parent iterations logStart logEnd]

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

data NoFixture
  = EmptyQueues
  | NoFixturesReady
  | NoThreadsAvailable

data NoCandidate
  = EmptyQueue
  | CantUseAnyMoreThreads
  | InvalidFixtureInPendingList
  | Finished

data IterationRun = IterationRun
  { parentFixture :: LoadedFixture,
    iteration :: IO ()
  }

updateStatusReturnCompleted :: LoadedFixture -> STM Bool
updateStatusReturnCompleted
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
          a <- readTVar activeThreads
          s <- readTVar fixStatus
          let done = null i && null a && not (completionBlocked s)
              doneAlready = isDone s
              completed = done && not doneAlready
              newStatus = completed ? Done Normal $ s
          writeTVar iterations i
          writeTVar activeThreads a
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
  canForkThread status
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
          \ifx@IndexedFixture {index, fixture = fixture@LoadedFixture {fixStatus}} ->
            let nxtInitial :: Maybe Int
                nxtInitial = mInitilIndex <|> Just index

                reQu :: STM ()
                reQu = writeTQueue activeQ ifx

                reQuGetNxt :: STM (Maybe LoadedFixture)
                reQuGetNxt = reQu >> getNxt nxtInitial

                reQReturnThisFixture :: STM (Maybe LoadedFixture)
                reQReturnThisFixture = reQu $> Just fixture
             in -- if we are back where we started we are done
                mInitilIndex == Just index
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

reserveThread :: Executor -> STM Bool
reserveThread
  Executor
    { maxThreads,
      threadsInUse
    } = do
    used <- readTVar threadsInUse
    let reserved = used < maxThreads
    writeTVar threadsInUse $ reserved ? used + 1 $ used
    pure reserved

isPending :: FixtureStatus -> Bool
isPending = \case
  Pending -> True
  Starting -> False
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

runFixture :: STM (TVar ThreadStatus) -> LoadedFixture -> IO ()
runFixture
  threadStatus
  fx@LoadedFixture
    { logStart,
      fixStatus,
      iterations,
      activeThreads,
      logEnd
    } =
    let runIterations :: IO ()
        runIterations = do
          wantLogStart <- atomically $ setToStartedIfPending fixStatus
          if wantLogStart
            then do
              elst <- tryAny logStart
              eitherf
                elst
                ( \e -> do
                    atomically $ writeTVar fixStatus $ Done (Fault "Failed logging fixture start" e)
                    -- if we get here things have really screwed up ye old terminal as a last resort
                    logError $ "Failed logging fixture start\n" <> toS (displayException e)

                    catchAny
                      logEnd
                      (\e' -> logError $ "Failed logging fixture end\n" <> toS (displayException e'))
                )
                (const runIterations)
            else do
              mi <- atomically $ takeIteration fx
              maybe
                (pure ())
                (\IterationRun {iteration} -> iteration >> runIterations)
                mi
     in do
          s' <- atomically threadStatus
          s <- readTVarIO s'
          case s of
            ThreadInitialising -> C.threadDelay 1_000 >> runFixture threadStatus fx
            ThreadRunning -> runIterations
            ThreadDone -> pure ()

forkFixtureThread :: TVar Int -> LoadedFixture -> IO ()
forkFixtureThread
  threadsInUse
  fx@LoadedFixture {activeThreads} = do
    let thrdStatus = newTVar ThreadInitialising
        tfx = forkFinally
          (runFixture thrdStatus fx)
          -- finally clean up
          \_ -> atomically $ do
            -- set this threadstatus to done
            ts <- thrdStatus
            writeTVar ts ThreadDone
            -- remove all finished threads from active threads list
            ats <- readTVar activeThreads
            newAts <- removeFinishedThreads ats
            writeTVar activeThreads newAts
            -- decrement global threads in use
            modifyTVar threadsInUse (1 -)

    atomically $ do
      ts <- thrdStatus
      modifyTVar activeThreads (RunningThread tfx ts :)
      writeTVar ts ThreadRunning

execute' :: Executor -> IO ()
execute'
  exe@Executor
    { maxThreads,
      threadsInUse,
      fixturesPending,
      fixturesStarted
    } = do
    eNxtFx <-
      atomically $
        reserveThread exe
          >>= bool
            (pure (Left NoThreadsAvailable))
            (nextFixture fixturesPending fixturesStarted)

    let recurse = execute' exe
        waitRecurse = C.threadDelay 10_000 >> recurse

    eitherf
      eNxtFx
      ( \case
          -- both the pending and active que of fixtures
          -- are empty so we are done
          EmptyQueues -> pure ()
          -- all the fixtures are not in a state to run any more threads
          -- eg being killed. We expect they may become available later of be finished
          -- and removed from the active que leading to empty ques we wait and try again
          NoFixturesReady -> waitRecurse
          -- all threads in use wait try again
          NoThreadsAvailable -> waitRecurse
      )
      (\f -> forkFixtureThread threadsInUse f >> recurse)

qFixture :: TQueue IndexedFixture -> (Int, LoadedFixture) -> STM ()
qFixture q (idx, fixture) = writeTQueue q $ IndexedFixture idx fixture

execute :: Int -> Node i o -> IO ()
execute maxThreads root = do
  fxs <- sequence $ mkFixtures root
  let idxFxs = zip [0 ..] fxs
  -- create queue
  pendingQ <- newTQueueIO
  runningQ <- newTQueueIO
  -- load all fixtures to pending queue
  atomically $ traverse_ (qFixture pendingQ) idxFxs
  initialThreadsInUse <- newTVarIO 0
  execute' $ Executor maxThreads initialThreadsInUse pendingQ runningQ
