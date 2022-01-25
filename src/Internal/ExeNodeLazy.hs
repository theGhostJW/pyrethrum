{-# LANGUAGE NoStrictData #-}

module Internal.ExeNodeLazy where

import Data.Function
import Data.Sequence (Seq (Empty))
import Polysemy
import Pyrelude (ListLike (unsafeHead, unsafeTail), Text, bool, fromMaybe, isJust, maybef, throw, traverse_, unless, unlessJust, uu, void, when, (?), Alternative ((<|>)))
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

data NoCandidate = EmptyQueue | CantUseAnyMoreThreads | InvalidFixtureInPendingList

data IterationRun = IterationRun IndexedFixture (IO ())

takeIteration :: IndexedFixture -> STM (Maybe IterationRun)
takeIteration ifx = do
  let fx = fixture ifx
  status <- readTVar $ fixStatus fx
  let itrsVar = (iterations :: LoadedFixture -> TVar [IO ()]) fx
  itrs <- readTVar itrsVar
  status `elem` [Active, Pending] || null itrs
    ? pure Nothing
    $ do
      let nxtItrs = unsafeTail itrs
          nxtIO = unsafeHead itrs
      writeTVar itrsVar nxtItrs
      pure (Just $ IterationRun ifx nxtIO)

nextActiveReady :: Maybe Int -> TQueue IndexedFixture -> STM (Maybe IterationRun)
nextActiveReady mInitilIndex activeQ = do
  ifx <- readTQueue activeQ
  let fx = fixture ifx
      thisIdx = index ifx
      exausted = maybef mInitilIndex False $ (==) thisIdx
      nxtInitial = mInitilIndex <|> Just thisIdx 
  exausted
    ? pure Nothing
    $ do
      status <- readTVar $ fixStatus fx
      -- write fixtures back to end of queue unless complete
      unless (isDone status) $
        writeTQueue activeQ ifx
      takeIteration ifx

pruneQueReturnNextReady :: TQueue IndexedFixture -> TQueue IndexedFixture -> STM (Either NoCandidate IterationRun)
pruneQueReturnNextReady pendingQ activeQ =
  let notEmpty q = not <$> isEmptyTQueue q
   in do
        hasPending <- notEmpty pendingQ
        hasActive <- notEmpty activeQ
        if
            | hasPending ->
              do
                ifx <- readTQueue pendingQ
                writeTQueue activeQ ifx
                mNxtIt <- takeIteration ifx
                maybef
                  mNxtIt
                  ( -- this can only happen if a non-pending fixture has somehow got into the pending queue
                    -- or a pending fixture has empty iteratations
                    pure $ Left InvalidFixtureInPendingList
                  )
                  (pure . Right)
            | hasActive ->
              do
                mbNxt <- nextActiveReady Nothing activeQ
                pure $
                  maybef
                    mbNxt
                    (Left CantUseAnyMoreThreads)
                    Right
            | otherwise -> pure $ Left EmptyQueue

wantFork :: Executor -> STM Bool
wantFork Executor { 
      maxThreads,
      threadsInUse
     } = do
    used <- readTVar $ threadsInUse
    pure $ used < maxThreads

execute' :: Executor -> IO ()
execute'
  exe@Executor
    { maxThreads,
      threadsInUse,
      fixturesPending,
      fixturesStarted
    } = uu
      -- do 
      --   ethNxt <- atomically $ pruneQueReturnNextReady fixturesPending fixturesStarted
      --   eitherf ethNxt
         

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
