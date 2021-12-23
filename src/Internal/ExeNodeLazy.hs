{-# LANGUAGE NoStrictData #-}

module Internal.ExeNodeLazy where

import Polysemy
import Pyrelude (Text, fromMaybe, throw, uu, void, when, (?))
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.STM
import Prelude
import Data.Function

data CompletionStatus = Normal | Fault | Murdered deriving (Eq, Show)

data FixtureStatus
  = Pending
  | Active
  | Done CompletionStatus
  | BeingKilled
  deriving (Eq, Show)

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
  Branch ::
    { branchParent :: Node i0 i,
      branchStatus :: IO (TVar BranchStatus),
      hook :: i -> IO o,
      hookCache :: IO (TMVar o),
      branchChildren :: [Node o o2],
      branchRelease :: Int -> o -> IO ()
    } ->
    Node i o
  Fixture ::
    MonadUnliftIO m =>
    { fixParent :: Node i0 i,
      iterations :: [i -> IO ()]
    } ->
    Node i ()

data LoadedFixture = LoadedFixture
  { 
    started :: IO (TVar Bool),
    fixStatus :: IO (TVar FixtureStatus),
    iterations :: IO (TVar [IO ()]),
    activeThreads :: IO [IO ThreadId]
  }

{- TODO
  ~ gather fixtures
  ~ simple
  ~ fixture address
  ~ log start and end fixture (generate form test suite)
  ~ exceptions
  ~ killing

-}

wantLaunch :: TVar BranchStatus -> STM Bool
wantLaunch status = do
  s <- readTVar status
  s == Unintitalised
    ? do
      writeTVar status Intitialising
      pure True
    $ pure False

-- launch only to be run when want launce has set
-- staus to initalising should only ever run once
-- TODO - test exception on output of branch parent and in resource aquisition
launch :: Node i o -> IO ()
launch =
  \case
    Root {} -> error "Not Implemented - Change types later"
    Fixture {} -> error "Not Implemented - Change types later"
    Branch
      { branchParent,
        branchStatus,
        hook,
        hookCache,
        branchChildren,
        branchRelease
      } -> do
        input <- outputWithLaunch branchParent
        bracket
          (hook input)
          (branchRelease 1 {- TODO implemnt pass through timeout for release -})
          --  we need to loop here to stop the branch releasing before the fixture has run
          \hookOut -> do
            hkVal <- hookCache
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
  branch@Branch
    { branchParent,
      branchStatus,
      hook,
      hookCache,
      branchChildren,
      branchRelease
    } -> do
      bs <- branchStatus
      wLaunch <- atomically $ wantLaunch bs
      when
        wLaunch
        (void $ forkIO $ launch branch)
      hc <- hookCache
      atomically $ readTMVar hc

-- runFixture :: Node i o -> IO (TVar Status) -> IO (TMVar [o -> IO ()]) -> IO [m ThreadId] -> IO ()
-- runFixture parent status iterations threads = do
--   input <- outputWithLaunch parent

loadFixture :: Node i o -> [o -> IO ()] -> IO LoadedFixture
loadFixture parent iterations = do
  input <- outputWithLaunch parent
  pure $ LoadedFixture { 
    started = newTVarIO False,
    fixStatus = newTVarIO Pending,
    iterations = newTVarIO ((input &) <$> iterations),
    activeThreads = pure []
  }
--

fixtures :: Node i o -> [IO LoadedFixture]
fixtures = \case
  Root {rootChildren} -> rootChildren >>= fixtures
  Branch {branchChildren} -> branchChildren >>= fixtures
  Fixture parent iterations -> [loadFixture parent iterations]

-- mkRunFixture :: [Sem effs ()] -> RunFixture effs
-- mkRunFixture i = RunFixture i True