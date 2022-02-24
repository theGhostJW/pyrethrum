

module Internal.PreNode where

import Pyrelude ( Show, Int, IO, TVar, SomeException, Text )
import UnliftIO ( MonadUnliftIO )

data CompletionStatus = Normal | Fault Text SomeException | Murdered deriving (Show)

data PreNode i o where
  Root ::
    { 
      rootChildren :: [PreNode () o]
    } ->
    PreNode () ()
  Hook ::
    { 
      hookStatus :: IO (TVar HookStatus),
      hook :: i -> IO o,
      hookChildren :: [PreNode o o2],
      hookRelease :: Int -> o -> IO ()
    } ->
    PreNode i o
  Fixture ::
    MonadUnliftIO m =>
    { logStart :: IO (),
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
  