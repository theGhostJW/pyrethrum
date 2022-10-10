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
      hookResult :: TMVar (Either SomeException oo),
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
  Fixture ::
    { fxTag :: Maybe Text,
      fxHook :: Loc -> oi -> ti -> IO to,
      fxHookRelease :: Loc -> to -> IO (),
      testHook :: Loc -> oi -> ti -> IO io,
      testHookRelease :: Loc -> io -> IO (),
      iterations :: [Test oi ti ii]
    } ->
    PreNode oi () ti () 

nodeEmpty :: PreNode a b c d  -> Bool
nodeEmpty = \case
  OnceHook {hookChild} -> nodeEmpty hookChild
  ThreadHook {threadHookChild} -> nodeEmpty threadHookChild
  Branch {subElms} -> all nodeEmpty subElms
  Fixture {iterations} -> null iterations
