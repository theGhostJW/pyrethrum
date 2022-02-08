{-# LANGUAGE NoStrictData #-}

module Internal.ExeNodeLazy where

import Data.Function
import Data.Sequence (Seq (Empty))
import Polysemy
import Pyrelude (Alternative ((<|>)), ListLike, Text, bool, eitherf, fromMaybe, isJust, mapLeft, maybef, throw, traverse_, unless, unlessJust, uu, void, when, ($>), (?))
import UnliftIO
  ( MonadUnliftIO,
    STM,
    TMVar,
    TQueue,
    TVar,
    atomically,
    bracket,
    isEmptyTQueue,
    newTQueueIO,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTQueue,
    readTVar,
    readTVarIO,
    writeTQueue,
    writeTVar,
  )
import UnliftIO.Concurrent as C (ThreadId, forkIO, threadDelay)
import UnliftIO.STM
  ( STM,
    TMVar,
    TQueue,
    TVar,
    atomically,
    isEmptyTQueue,
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

data CompletionStatus = Normal | Fault | Murdered deriving (Eq, Show)

data FixtureStatus
  = Pending
  | Active
  | Done CompletionStatus
  | BeingKilled
  deriving (Eq, Show)

data BranchStatus
  = Unintialised
  | Intitialising
  | Running
  | Complete CompletionStatus
  | BeingMurdered
  deriving (Eq, Show)

isComplete :: BranchStatus -> Bool
isComplete = \case
  Complete _ -> True
  _ -> False

data Node i o where
  Root ::
    { rootStatus :: IO (TMVar BranchStatus),
      rootChildren :: [Node () o]
    } ->
    Node () ()
  Hook ::
    { branchParent :: Node i0 i,
      branchStatus :: IO (TVar BranchStatus),
      hook :: i -> IO o,
      hookResult :: IO (TMVar o),
      hookChildren :: [Node o o2],
      branchRelease :: Int -> o -> IO ()
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

data LoadedFixture = LoadedFixture
  { logStart :: IO (),
    fixStatus :: TVar FixtureStatus,
    iterations :: TVar [IO ()],
    activeThreads :: TVar [IO ThreadId],
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

setReadyForLaunch :: TVar BranchStatus -> STM Bool
setReadyForLaunch status = do
  s <- readTVar status
  s == Unintialised
    ? do
      writeTVar status Intitialising
      pure True
    $ pure False

-- launch only to be run when want launch has set
-- staus to initalising should only ever run once per branch
-- TODO - test exception on output of branch parent and in resource aquisition
launch :: Node i o -> IO ()
launch =
  \case
    Root {} -> error "Not Implemented - Change types later"
    Fixture {} -> error "Not Implemented - Change types later"
    Hook
      { branchParent,
        branchStatus,
        hook,
        hookResult,
        hookChildren,
        branchRelease
      } -> do
        input <- outputWithLaunch branchParent
        bracket
          (hook input)
          (branchRelease 1 {- TODO implemnt pass through timeout for release -})
          -- we need to loop here to stop the branch releasing before the
          -- fixture has run to completion
          \hookOut -> do
            hkVal <- hookResult
            status <- branchStatus
            atomically $ do
              putTMVar hkVal hookOut
              writeTVar status Running
            let recheck = do
                  s <- readTVarIO status
                  isComplete s
                    ? pure ()
                    $ C.threadDelay 2_000_000 >> recheck
            recheck

outputWithLaunch :: Node i o -> IO o
outputWithLaunch = \case
  Root {} -> pure ()
  Fixture {} -> pure ()
  branch@Hook
    { branchParent,
      branchStatus,
      hook,
      hookResult,
      hookChildren,
      branchRelease
    } -> do
      bs <- branchStatus
      wantLaunch <- atomically $ setReadyForLaunch bs
      when
        wantLaunch
        (void $ forkIO $ launch branch)
      hc' <- hookResult
      atomically $ readTMVar hc'

loadFixture :: Node i o -> [o -> IO ()] -> IO () -> IO () -> IO LoadedFixture
loadFixture parent iterations logStart logEnd = do
  input <- outputWithLaunch parent
  fixStatus <- newTVarIO Pending
  iterations' <- newTVarIO ((input &) <$> iterations)
  pure $
    LoadedFixture
      { logStart = logStart,
        fixStatus = fixStatus,
        iterations = iterations',
        activeThreads = pure [],
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

updateStatus :: LoadedFixture -> IO Bool
updateStatus
  LoadedFixture
    { fixStatus,
      iterations,
      activeThreads
    } =
    let canComplete :: FixtureStatus -> Bool
        canComplete = \case
          Pending -> True
          Active -> True
          Done cs -> True
          BeingKilled -> False
        doneAlready :: FixtureStatus -> Bool
        doneAlready = \case
          Pending -> False
          Active -> False
          Done cs -> True
          BeingKilled -> False
     in atomically $ do
          i <- readTVar iterations
          a <- readTVar activeThreads
          s <- readTVar fixStatus
          let isDone = null i && null a && canComplete s
              newStatus = isDone && not (doneAlready s) ? Done Normal $ s
          writeTVar iterations i
          writeTVar activeThreads a
          writeTVar fixStatus newStatus
          pure isDone

forkIteration :: IterationRun -> IO ()
forkIteration IterationRun { fixture = LoadedFixture {fixStatus,
    iterations,
    activeThreads,
    logEnd }, iteration } = uu
      do
       let
         updateStatus :: ThreadId -> IO Bool
         updateStatus = atomically $ do
            activeThreads

takeIteration :: LoadedFixture -> STM (Maybe IterationRun)
takeIteration fixture = do
  status <- readTVar $ fixStatus fixture
  let itrsVar = (iterations :: LoadedFixture -> TVar [IO ()]) fixture
  itrs <- readTVar itrsVar
  notElem status [Active, Pending]
    ? pure Nothing
    $ case itrs of
      [] -> pure Nothing
      x : xs -> do
        writeTVar itrsVar xs
        pure (Just $ IterationRun fixture x)

nextReadyFixtureInQ :: Maybe Int -> TQueue IndexedFixture -> STM (Maybe LoadedFixture)
nextReadyFixtureInQ mInitilIndex activeQ = do
  mfx <- tryReadTQueue activeQ
  maybef
    mfx
    (pure Nothing)
    \ifx@IndexedFixture {index, fixture} ->
      let nxtInitial = mInitilIndex <|> Just index
          reQu = writeTQueue activeQ ifx
          reQReturnThisFixture = reQu $> Just fixture
          tryNextFixture = nextReadyFixtureInQ nxtInitial activeQ
       in -- if we are back where we started we are done
          mInitilIndex == Just index
            ? pure Nothing
            $ do
              status <- readTVar $ fixStatus fixture
              case status of
                Pending -> reQReturnThisFixture
                Active -> reQReturnThisFixture
                -- done fixtures are not added to the back of the q
                Done cs -> tryNextFixture
                BeingKilled -> reQu >> tryNextFixture

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
              do
                mbNxt <- nextReadyFixtureInQ Nothing activeQ
                pure $
                  maybef
                    mbNxt
                    (Left NoFixturesReady)
                    Right
            | otherwise ->
              pure $ Left EmptyQueues

threadsAvailable :: Executor -> STM Bool
threadsAvailable
  Executor
    { maxThreads,
      threadsInUse
    } = do
    used <- readTVar threadsInUse
    pure $ used < maxThreads

execute' :: Executor -> IO ()
execute'
  exe@Executor
    { maxThreads,
      threadsInUse,
      fixturesPending,
      fixturesStarted
    } = do
    ethNxt <- atomically $ do
      haveThrds <- threadsAvailable exe
      haveThrds
        ? nextFixture fixturesPending fixturesStarted
        $ pure (Left NoThreadsAvailable)

    let waitReexecute :: IO ()
        waitReexecute = C.threadDelay 10_000 >> execute' exe

    eitherf
      nxtfx
      ( \case
          -- both the pending and active que of fixtures
          -- are empty so we are done
          EmptyQueues -> pure ()
          -- all the fixtures are not in a state to run any more threads
          -- eg being killed. We expect they may become available later of be finished
          -- and removed from the active que leading to empty ques we wait and try again
          NoFixturesReady -> waitReexecute
          -- all threads in use wait try again
          NoThreadsAvailable -> waitReexecute
      )
      ( \fx ->
          do
            mbi <- atomically $ takeIteration fx
            maybef
              mbi
              waitReexecute
              ( \ir ->
                  uu
              )
      )

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
