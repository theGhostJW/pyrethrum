module Internal.PreNode where

import Control.DeepSeq (NFData)
import Language.Haskell.TH (ExpQ)
import Pyrelude (Bool (False, True), Either, Eq, Generic, IO, Int, ListLike (any, filter, null, all), Show, SomeException, TVar, Text, not, ($), (&&), Ord)
import UnliftIO (MonadUnliftIO, STM, TMVar)

data CompletionStatus
  = Normal
  | Fault Text SomeException
  | Murdered Text
  deriving (Show)

newtype PreNodeRoot = 
  PreNodeRoot { rootNode :: IO (PreNode () () () ()) }

data FixtureStatus
  = Pending
  | Starting
  | Active
  | Done CompletionStatus
  | BeingKilled
  deriving (Show)

newtype Loc = Loc { unLoc :: Text} deriving (Show, Eq, Ord)

data PreNode si so ti to where
  Branch :: {
    subElms :: [PreNode si so ti to]
   } ->
   PreNode si () ti () 
  AnyHook ::
    { 
      hookStatus :: TVar HookStatus,
      hook :: Loc -> si -> IO so,
      hookChild :: PreNode so so2 to to2,
      hookResult :: TMVar (Either SomeException so),
      hookRelease :: Loc -> so -> IO ()
    } ->
    PreNode si so ti to 
  ThreadHook ::
    { 
      threadHook :: Loc -> ti -> IO to,
      threadHookChild :: PreNode so so2 to to2,
      threadHookRelease :: Loc -> to -> IO ()
    } ->
    PreNode si so ti to 
  Fixture ::
    { 
      logStart :: Loc -> IO (),
      iterations :: [Loc -> si -> ti -> IO ()],
      logEnd :: Loc -> IO ()
    } ->
    PreNode si () ti ()

nodeEmpty :: PreNode a b c d -> Bool
nodeEmpty = \case
  AnyHook {hookChild} -> nodeEmpty hookChild
  ThreadHook {threadHookChild} -> nodeEmpty threadHookChild
  Branch {subElms} -> all nodeEmpty subElms
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
