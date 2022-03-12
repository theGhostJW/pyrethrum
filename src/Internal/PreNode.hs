{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Internal.PreNode where

import Pyrelude (IO, Int, Show, SomeException, TVar, Text)
import UnliftIO (MonadUnliftIO)

data CompletionStatus = Normal | Fault Text SomeException | Murdered deriving (Show)

data PreNodeRoot o =
  PreNodeRoot 
    { children :: [PreNode () o]
    }

data PreNode i o where
  Hook ::
    { hookAddress :: Text, -- used in testing
      hookStatus :: IO (TVar HookStatus),
      hook :: i -> IO o,
      hookChildren :: [PreNode o o2],
      hookRelease :: Int -> o -> IO ()
    } ->
    PreNode i o
  Fixture ::
    { fixtureAddress :: Text, -- used in testing
      logStart :: IO (),
      iterations :: [i -> IO ()],
      logEnd :: IO ()
    } ->
    PreNode i ()

data HookStatus
  = Unintialised
  | Intitialising
  | Running
  | Complete CompletionStatus
  | BeingMurdered
  deriving (Show)