
module Internal.PreNode where

import Pyrelude (IO, Int, Show, SomeException, TVar, Text, Eq, Bool (False, True), (&&), not)
import UnliftIO (MonadUnliftIO, STM)
import Language.Haskell.TH (ExpQ)

data CompletionStatus = Normal | Fault Text SomeException | Murdered deriving (Show)

newtype PreNodeRoot o =
  PreNodeRoot 
    { children :: [PreNode () o]
    }

data PreNode i o where
  Hook ::
    { hookAddress :: Text, -- used in testing
      hookStatus :: TVar HookStatus,
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
  | Finalising
  | Finalised
  deriving (Show)

cleaningUp :: HookStatus -> Bool 
cleaningUp = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> False
  BeingMurdered -> True
  Finalising -> True
  Finalised -> False

finalised :: HookStatus -> Bool 
finalised = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> False
  BeingMurdered -> False
  Finalising -> False
  Finalised -> True

complete :: HookStatus -> Bool 
complete = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> True
  BeingMurdered -> False
  Finalising -> False
  Finalised -> False

normalCompletion :: HookStatus -> Bool 
normalCompletion = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> case cs of
    Normal -> True
    Fault {} -> False
    Murdered -> False
  Finalising -> False
  BeingMurdered -> False
  Finalised -> False


