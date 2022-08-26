module Internal.PreNode where

import Control.DeepSeq (NFData)
import Internal.RunTimeLogging (Loc)
import Language.Haskell.TH (ExpQ)
import Pyrelude (Bool (False, True), Either, Eq, Generic, IO, Int, ListLike (all, any, filter, null), Maybe, Ord, Show, SomeException, TVar, Text, not, ($), (&&))
import UnliftIO (MonadUnliftIO, STM, TMVar)

type PreNodeRoot = PreNode () () () () () ()

data Test si ti ii = Test
  { tstId :: Text,
    tst :: si -> ti -> ii -> IO ()
  }

data PreNode oi oo ti to ii io where
  Branch ::
    { bTag :: Maybe Text,
      subElms :: [PreNode oi oo ti to ii io]
    } ->
    PreNode oi () ti () ii ()
  OnceHook ::
    { hookTag :: Maybe Text,
      hook :: Loc -> oi -> IO oo,
      hookChild :: PreNode oo coo ti to ii io,
      hookResult :: TMVar (Either SomeException oo),
      hookRelease :: Loc -> oo -> IO ()
    } ->
    PreNode oi oo ti to ii io
  ThreadHook ::
    { threadTag :: Maybe Text,
      threadHook :: Loc -> oi -> ti -> IO to,
      threadHookChild :: PreNode oi oo to cto ii io,
      threadHookRelease :: Loc -> to -> IO ()
    } ->
    PreNode oi oo ti to ii io
  TestHook ::
    { testTag :: Maybe Text,
      testHook :: Loc -> oi -> ti -> ii -> IO io,
      testHookChild :: PreNode oi oo ti to io cii,
      testHookRelease :: Loc -> io -> IO ()
    } ->
    PreNode oi oo ti to ii io
  Fixture ::
    { fxTag :: Maybe Text,
      iterations :: [Test oi ti ii]
    } ->
    PreNode oi () ti () ii ()

nodeEmpty :: PreNode a b c d e f -> Bool
nodeEmpty = \case
  OnceHook {hookChild} -> nodeEmpty hookChild
  ThreadHook {threadHookChild} -> nodeEmpty threadHookChild
  TestHook {testHookChild} -> nodeEmpty testHookChild
  Branch {subElms} -> all nodeEmpty subElms
  Fixture {iterations} -> null iterations
