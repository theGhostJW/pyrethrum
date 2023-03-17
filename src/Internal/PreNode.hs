module Internal.PreNode where

import Control.DeepSeq (NFData)
import Internal.RunTimeLogging (Loc, ApLogger)
import Language.Haskell.TH (ExpQ)
import UnliftIO (MonadUnliftIO, STM, TMVar)
import PyrethrumExtras ( txt, uu, (?) )
import qualified Text.Extra as T

type PreNodeRoot = PreNode () () () ()

data Test si ti ii = Test
  { tstId :: Text,
    tst :: ApLogger -> si -> ti -> ii -> IO ()
  }

-- this would be genrated by the TH
data Fixture oi ti ii oo to io where 
  Fixture ::
    {id :: Text,
     onceHook :: ApLogger -> oi -> IO oo,
     onceHookRelease :: ApLogger -> oo -> IO (),
     threadHook :: ApLogger -> oo -> ti -> IO to,
     threadHookRelease :: ApLogger -> to -> IO (),
     testHook :: ApLogger -> oo -> to -> ii ->  IO io,
     testHookRelease :: ApLogger -> io -> IO (),
     tests :: [Test oo to io]
   } -> 
   Fixture oi ti ii oo to io

f1 :: Fixture Int Text Bool Text Int Bool
f1 = Fixture {
  id = "fixture",
  onceHook = \l int -> pure $ txt int,
  threadHook = \l otxt tiTxt -> pure $ T.length otxt + T.length tiTxt,
  testHook = \l tx int b -> b ? pure b $ error tx,
  
  testHookRelease = \l b -> b ? pure () $ error "its false",
  onceHookRelease = \l txt' -> putText txt',
  threadHookRelease = \l i -> print . txt $ i + 1,
  tests = []
}
data PreNode oi oo ti to where
  Branch ::
    { title :: Maybe Text,
      subElms :: [PreNode oi oo ti to ]
    } ->
    PreNode oi oo ti to 
  OnceHook ::
    { title :: Maybe Text,
      hook :: Loc -> ApLogger -> oi -> IO oo,
      hookChild :: PreNode oo coo ti to,
      hookRelease :: Loc -> ApLogger -> oo -> IO ()
    } ->
    PreNode oi oo ti to 
  ThreadHook ::
    { title :: Maybe Text,
      threadHook :: Loc -> ApLogger -> oi -> ti -> IO to,
      threadHookChild :: PreNode oi oo to cto ,
      threadHookRelease :: Loc -> ApLogger -> to -> IO ()
    } ->
    PreNode oi oo ti to 
  Fixtures :: { 
      onceFxHook :: Loc -> ApLogger -> oi -> IO oo,
      threadFxHook :: Loc -> ApLogger -> oo -> ti -> IO to,
      testHook :: Loc -> ApLogger -> oo -> to -> IO io,
      onceFxHookRelease :: Loc -> ApLogger -> oo -> IO (),
      threadFxHookRelease :: Loc -> ApLogger -> to -> IO (),
      testHookRelease :: Loc -> ApLogger -> io -> IO (),
      title :: Maybe Text,
      iterations :: [Test oo to io]
    } ->
    PreNode oi oo ti to 


data OnceHook oi oo where 
  OnceNone :: () -> OnceHook oi oo
  OnceBefore :: Loc -> ApLogger -> oi -> IO oo -> OnceHook oi oo 
  OnceAround :: {
    hook :: Loc -> ApLogger -> oi -> IO oo,
    release :: Loc -> ApLogger -> oo -> IO ()
  } -> OnceHook oi oo
data ThreadHook oi ti to where 
  ThreadNone :: () -> ThreadHook oi ti ti
  ThreadBefore :: Loc -> ApLogger -> oi -> IO oo -> ThreadHook oi ti to
  ThreadAround :: Loc -> ApLogger -> oi -> IO oo -> ThreadHook oi ti to


    -- todo: 
    -- change name of tag
    -- get rid of maybe on tag
    -- change iterations name
    -- change iterations test to fixtures
    -- ?? add maybe to all hooks?????
      -- how release without a hook
      -- data Hook hook release where 
            -- None |  -- passes through types
            -- Before a | 
            -- After b | 
            -- Around a b
    -- should we have branches

nodeEmpty :: PreNode a b c d  -> Bool
nodeEmpty = \case
  OnceHook {hookChild} -> nodeEmpty hookChild
  ThreadHook {threadHookChild} -> nodeEmpty threadHookChild
  Branch {subElms} -> all nodeEmpty subElms
  Fixtures {iterations} -> null iterations



