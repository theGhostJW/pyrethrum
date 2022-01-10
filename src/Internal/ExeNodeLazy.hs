{-# LANGUAGE NoStrictData #-}

module Internal.ExeNodeLazy where

import Data.Function
import Data.Sequence (Seq (Empty))
import Polysemy
import Pyrelude (Listy (unsafeHead, unsafeTail), Text, bool, fromMaybe, isJust, maybef, throw, traverse_, unless, unlessJust, uu, void, when, (?))
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.STM
import Prelude

data CompletionStatus = Normal | Fault | Murdered deriving (Eq, Show)

data FixtureStatus
  = Pending
  | Active
  | Done CompletionStatus
  | BeingKilled
  deriving (Eq, Show)

isDone :: FixtureStatus -> Bool
isDone = \case
  Pending -> False
  Active -> False
  Done _ -> True
  BeingKilled -> False

data BranchStatus
  = Unintitalised
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
    activeThreads :: IO [IO ThreadId],
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
  s == Unintitalised
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
                    $ threadDelay 2_000_000 >> recheck
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

-- fixtureActive :: LoadedFixture -> STM Bool
-- fixtureActive fx =
--   (==) Active <$> readTVar (fixStatus fx)

-- firstActive :: [IO LoadedFixture] -> IO (Maybe LoadedFixture)
-- firstActive = \case
--   [] -> pure Nothing
--   x : xs -> do
--     lf <- x
--     active <- fixtureActive lf
--     active
--       ? pure (Just lf)
--       $ firstActive xs

data NoCandidate = EmptyQueue | CantUseAnyMoreThreads

data IterationRun = IterationRun IndexedFixture (IO ())

nextActiveReady :: Maybe Int -> TQueue IndexedFixture -> STM (Maybe IterationRun)
nextActiveReady mInitilIndex activeQ = do
  fx' <- readTQueue activeQ
  let thisIdx = index fx'
      fx = fixture fx'
      exausted = maybef mInitilIndex False $ (==) thisIdx
      nxtInitial = maybef mInitilIndex (pure thisIdx) pure
  exausted
    ? pure Nothing
    $ do
      status <- readTVar $ fixStatus fx
      let itrsVar = (iterations :: LoadedFixture -> TVar [IO ()]) fx
      itrs <- readTVar itrsVar
      -- write fixtures back to end of queue unless complete
      unless (isDone status) $
        writeTQueue activeQ fx'

      -- when (status == Pending)
      --   throw "Framework Error - this should not happen - no fixtures should be pending by the time this is run"

      bool
        (nextActiveReady nxtInitial activeQ)
        ( do
            let nxtItrs = unsafeTail itrs
                nxtIO = unsafeHead itrs

            writeTVar itrsVar nxtItrs
            pure (Just $ IterationRun fx' nxtIO)
        )
        (status == Active && not (null itrs))

pruneQueReturnNextReady :: TQueue IndexedFixture -> TQueue IndexedFixture -> STM (Either NoCandidate IterationRun)
pruneQueReturnNextReady pendingQ activeQ =
  let notEmpty q = not <$> isEmptyTQueue q
   in do
        hasPending <- notEmpty pendingQ
        hasActive <- notEmpty activeQ
        if
            | hasPending ->
              do
                fx <- readTQueue pendingQ
                writeTQueue activeQ fx
                pure $ Right fx
            | hasActive ->
              do
                mbNxt <- nextActiveReady Nothing activeQ
                pure $
                  maybef
                    mbNxt
                    (Left CantUseAnyMoreThreads)
                    Right
            | otherwise -> pure $ Left EmptyQueue

-- nextFork' :: Executor -> IO (Maybe LoadedFixture)
-- nextFork' ep = do
--   atomically $ do
--     let fxs = fixtures ep
--     used <- readTVar $ threadsInUse ep
--     let wantFork = used < maxThreads ep
--     if wantFork
--       then do
--         fx <- readTQueue fxs
--         let status = fixStatus fx
--         pure Nothing
--       else pure Nothing

execute' :: Executor -> IO ()
execute'
  exe@Executor
    { maxThreads,
      threadsInUse,
      fixturesPending,
      fixturesStarted
    } = uu

qFixture :: TQueue IndexedFixture -> (Int, LoadedFixture) -> STM ()
qFixture q (idx, fixture) = writeTQueue q $ IndexedFixture idx fixture

execute :: Int -> Node i o -> IO ()
execute maxThreads root = do
  fxs <- sequence $ mkFixtures root
  let idxFxs = zip [0 ..] fxs
  -- create queue
  pendingQ <- newTQueueIO
  runningQ <- newTQueueIO
  -- load queue
  atomically $ traverse_ (qFixture pendingQ) idxFxs
  initialThreadsInUse <- newTVarIO 0
  execute' $ Executor maxThreads initialThreadsInUse pendingQ runningQ
