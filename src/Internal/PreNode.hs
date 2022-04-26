module Internal.PreNode where

import Control.DeepSeq (NFData)
import Language.Haskell.TH (ExpQ)
import Pyrelude (Bool (False, True), Either, Eq, Generic, IO, Int, ListLike (any, filter, null, all), Show, SomeException, TVar, Text, not, ($), (&&))
import UnliftIO (MonadUnliftIO, STM, TMVar)

data CompletionStatus
  = Normal
  | Fault Text SomeException
  | Murdered Text
  deriving (Show)

newtype PreNodeRoot o = PreNodeRoot { children :: IO [PreNode () o] }

data FixtureStatus
  = Pending
  | Starting
  | Active
  | Done CompletionStatus
  | BeingKilled
  deriving (Show)

data PreNode i o where
  AnyHook ::
    { hookAddress :: Text, -- used in testing
      hookStatus :: TVar HookStatus,
      hook :: i -> IO o,
      hookChildren :: [PreNode o o2],
      hookResult :: TMVar (Either SomeException o),
      hookRelease :: Int -> o -> IO ()
    } ->
    PreNode i o
  Fixture ::
    { fixtureAddress :: Text, -- used in testing
      fixtureStatus :: TVar FixtureStatus,
      logStart :: IO (),
      iterations :: [i -> IO ()],
      logEnd :: IO ()
    } ->
    PreNode i ()

nodeEmpty :: PreNode a b -> Bool
nodeEmpty = \case
  AnyHook {hookChildren} -> all nodeEmpty hookChildren
  Fixture {iterations} -> null iterations

data HookStatus
  = Unintialised
  | Intitialising
  | Running
  | Complete CompletionStatus
  | BeingMurdered
  | Finalising
  | Finalised CompletionStatus
  deriving (Show)

cleaningUp :: HookStatus -> Bool
cleaningUp = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> False
  BeingMurdered -> True
  Finalising -> True
  Finalised _ -> False

finalised :: HookStatus -> Bool
finalised = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> False
  BeingMurdered -> False
  Finalising -> False
  Finalised _ -> True

complete :: HookStatus -> Bool
complete = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> True
  BeingMurdered -> False
  Finalising -> False
  Finalised _ -> False

normalCompletion :: HookStatus -> Bool
normalCompletion = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> case cs of
    Normal -> True
    Fault {} -> False
    Murdered _ -> False
  Finalising -> False
  BeingMurdered -> False
  Finalised _ -> False
