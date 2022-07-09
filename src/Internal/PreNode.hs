module Internal.PreNode where

import Control.DeepSeq (NFData)
import Language.Haskell.TH (ExpQ)
import Pyrelude (Bool (False, True), Either, Eq, Generic, IO, Int, ListLike (any, filter, null, all), Show, SomeException, TVar, Text, not, ($), (&&), Ord, Maybe)
import UnliftIO (MonadUnliftIO, STM, TMVar)
import Data.Map.Strict as M

newtype PreNodeRoot = 
  PreNodeRoot { rootNode :: IO (PreNode () () () () () ()) }

newtype Loc = Loc { unLoc :: Text} deriving (Show, Eq, Ord)

data PreNode oi oo ti to ii io where
  Branch :: {
    bTag :: Maybe Text,
    subElms :: [PreNode oi oo ti to ii io]
   } ->
   PreNode oi () ti () ii ()
  OnceHook ::
    { 
      hookTag :: Maybe Text,
      hook :: Loc -> oi -> IO oo,
      hookChild :: PreNode oo coo ti to ii io,
      hookResult :: TMVar (Either oomeException oo),
      hookRelease :: Loc -> oo -> IO ()
    } ->
    PreNode oi oo ti to ii io  
  ThreadHook ::
    { 
      threadTag :: Maybe Text,
      threadHook :: Loc -> oi -> ti -> IO to,
      threadHookChild :: PreNode oi oo to cto ii io,
      threadHookRelease :: Loc -> to -> IO ()
    } ->
    PreNode oi oo ti to ii io
  TestHook ::
    { 
      threadTag :: Maybe Text,
      testHook :: Loc -> oi -> ti -> ii -> IO io,
      testHookChild :: PreNode oi oo ti to io cii,
      testHookRelease :: Loc -> io -> IO ()
    } ->
    PreNode oi oo ti to ii io
  Fixture ::
    { 
      fxTag :: Maybe Text,
      logStart :: Loc -> IO (),
      iterations :: Map Text (oi -> ti -> ii -> IO ()),
      logEnd :: Loc -> IO ()
    } ->
    PreNode oi () ti () ii ()


nodeEmpty :: PreNode a b c d e f -> Bool
nodeEmpty = \case
  OnceHook {hookChild} -> nodeEmpty hookChild
  ThreadHook {threadHookChild} -> nodeEmpty threadHookChild
  Branch {subElms} -> all nodeEmpty subElms
  Fixture {iterations} -> M.null iterations


