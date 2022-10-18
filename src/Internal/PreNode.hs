module Internal.PreNode where

import Control.DeepSeq (NFData)
import Internal.RunTimeLogging (Loc)
import Language.Haskell.TH (ExpQ)
import Pyrelude (Bool (False, True), Either, Eq, Generic, IO, Int, ListLike (all, any, filter, null), Maybe, Ord, Show, SomeException, TVar, Text, not, ($), (&&))
import UnliftIO (MonadUnliftIO, STM, TMVar)

type PreNodeRoot = PreNode () () () ()

data Test si ti ii = Test
  { tstId :: Text,
    tst :: si -> ti -> ii -> IO ()
  }

data PreNode oi oo ti to  where
  Branch ::
    { bTag :: Maybe Text,
      subElms :: [PreNode oi oo ti to ]
    } ->
    PreNode oi () ti () 
  OnceHook ::
    { hookTag :: Maybe Text,
      hook :: Loc -> oi -> IO oo,
      hookChild :: PreNode oo coo ti to ,
      hookRelease :: Loc -> oo -> IO ()
    } ->
    PreNode oi oo ti to 
  ThreadHook ::
    { threadTag :: Maybe Text,
      threadHook :: Loc -> oi -> ti -> IO to,
      threadHookChild :: PreNode oi oo to cto ,
      threadHookRelease :: Loc -> to -> IO ()
    } ->
    PreNode oi oo ti to 
  Fixture :: { 
      onceFxHook :: Loc -> oi -> IO oo,
      onceFxHookRelease :: Loc -> oo -> IO (),
      threadFxHook :: Loc -> oo -> ti -> IO to,
      threadFxHookRelease :: Loc -> to -> IO (),
      testHook :: Loc -> oo -> to -> IO io,
      testHookRelease :: Loc -> io -> IO (),
      fxTag :: Maybe Text,
      iterations :: [Test oo to io]
    } ->
    PreNode oi () ti () 

nodeEmpty :: PreNode a b c d  -> Bool
nodeEmpty = \case
  OnceHook {hookChild} -> nodeEmpty hookChild
  ThreadHook {threadHookChild} -> nodeEmpty threadHookChild
  Branch {subElms} -> all nodeEmpty subElms
  Fixture {iterations} -> null iterations
