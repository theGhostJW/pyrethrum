module Internal.PreNode where

import Control.DeepSeq (NFData)
import Language.Haskell.TH (ExpQ)
import Pyrelude (Bool (False, True), Either, Eq, Generic, IO, Int, ListLike (any, filter, null, all), Show, SomeException, TVar, Text, not, ($), (&&), Ord, Maybe)
import UnliftIO (MonadUnliftIO, STM, TMVar)

newtype PreNodeRoot = 
  PreNodeRoot { rootNode :: IO (PreNode () () () ()) }

newtype Loc = Loc { unLoc :: Text} deriving (Show, Eq, Ord)

data PreNode si so ti to where
  Branch :: {
    bTag :: Maybe Text,
    subElms :: [PreNode si so ti to]
   } ->
   PreNode si () ti () 
  AnyHook ::
    { 
      hookTag :: Maybe Text,
      hook :: Loc -> si -> IO so,
      hookChild :: PreNode so cso ti to,
      hookResult :: TMVar (Either SomeException so),
      hookRelease :: Loc -> so -> IO ()
    } ->
    PreNode si so ti to 
  ThreadHook ::
    { 
      threadTag :: Maybe Text,
      threadHook :: Loc -> si -> ti -> IO to,
      threadHookChild :: PreNode si so to cto,
      threadHookRelease :: Loc -> to -> IO ()
    } ->
    PreNode si so ti to 
  Fixture ::
    { 
      fxTag :: Maybe Text,
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


