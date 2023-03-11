module Internal.PreNode where

import Control.DeepSeq (NFData)
import Internal.RunTimeLogging (Loc, ApLogger)
import Language.Haskell.TH (ExpQ)
import UnliftIO (MonadUnliftIO, STM, TMVar)

type PreNodeRoot = PreNode () () () ()

data Test si ti ii = Test
  { tstId :: Text,
    tst :: ApLogger -> si -> ti -> ii -> IO ()
  }

data PreNode oi oo ti to  where
  Branch ::
    { bTag :: Maybe Text,
      subElms :: [PreNode oi oo ti to ]
    } ->
    PreNode oi () ti () 
  OnceHook ::
    { hookTag :: Maybe Text,
      hook :: Loc -> ApLogger -> oi -> IO oo,
      hookChild :: PreNode oo coo ti to ,
      hookRelease :: Loc -> ApLogger -> oo -> IO ()
    } ->
    PreNode oi oo ti to 
  ThreadHook ::
    { threadTag :: Maybe Text,
      threadHook :: Loc -> ApLogger -> oi -> ti -> IO to,
      threadHookChild :: PreNode oi oo to cto ,
      threadHookRelease :: Loc -> ApLogger -> to -> IO ()
    } ->
    PreNode oi oo ti to 
  Fixture :: { 
      onceFxHook :: Loc -> ApLogger -> oi -> IO oo,
      onceFxHookRelease :: Loc -> ApLogger -> oo -> IO (),
      threadFxHook :: Loc -> ApLogger -> oo -> ti -> IO to,
      threadFxHookRelease :: Loc -> ApLogger -> to -> IO (),
      testHook :: Loc -> ApLogger -> oo -> to -> IO io,
      testHookRelease :: Loc -> ApLogger -> io -> IO (),
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
