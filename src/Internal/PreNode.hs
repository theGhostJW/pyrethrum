module Internal.PreNode where

import Control.DeepSeq (NFData)
import Internal.RunTimeLogging (Loc, ApLogger)
import Language.Haskell.TH (ExpQ)
import UnliftIO (MonadUnliftIO, STM, TMVar)
import PyrethrumExtras ( txt, uu, (?) )
import qualified Text.Extra as T

type PreNodeRoot = PreNode () ()

data Test si ti ii = Test
  { tstId :: Text,
    tst :: ApLogger -> si -> ti -> ii -> IO ()
  }

-- this would be genrated by the TH
data Fixture oi ti ii where 
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
   Fixture oi ti ii

f1 :: Fixture Int Text Bool
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
data PreNode oi ti where
  Branch ::
    { title :: Text,
      subElms :: [PreNode oi ti]
    } ->
    PreNode oi ti
  OnceHook ::
    { title :: Text,
      hook :: Loc -> ApLogger -> oi -> IO oo,
      onceSubNodes :: [PreNode oo ti],
      hookRelease :: Loc -> ApLogger -> oo -> IO ()
    } ->
    PreNode oi ti 
  ThreadHook ::
    { title :: Text,
      threadHook :: Loc -> ApLogger -> oi -> ti -> IO tn,
      threadHookChild :: PreNode oi tn,
      threadHookRelease :: Loc -> ApLogger -> tn -> IO ()
    } ->
    PreNode oi ti
  Fixtures :: { 
      onceFxHook :: Loc -> ApLogger -> oi -> IO oo,
      threadFxHook :: Loc -> ApLogger -> oo -> ti -> IO tn,
      testHook :: Loc -> ApLogger -> oo -> tn -> IO io,
      onceFxHookRelease :: Loc -> ApLogger -> oo -> IO (),
      threadFxHookRelease :: Loc -> ApLogger -> tn -> IO (),
      testHookRelease :: Loc -> ApLogger -> io -> IO (),
      title :: Text,
      fixtures :: [Test oo tn io]
    } ->
    PreNode oi ti 


data OnceHook oi oo where 
  OnceNone :: () -> OnceHook oi oi
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
    -- simplify GADTs :: Done
    -- change name of tag :: Done
    -- get rid of maybe on tag :: Done
    -- change iterations name :: Done
    -- get rid of branches
    -- remove once / threadHooks from Fixtures
    -- how release without a hook
      -- data Hook hook release where 
            -- None |  -- passes through types
            -- Before a | 
            -- After b | 
            -- Around a b
    -- root becomes ones hook with OnceNone as hook
    -- change fixtures from test to fixtures
    -- consider collapsing threadHook and onceHook types

nodeEmpty :: PreNode oi ti  -> Bool
nodeEmpty = \case
  OnceHook {onceSubNodes} -> all nodeEmpty onceSubNodes
  ThreadHook {threadHookChild} -> nodeEmpty threadHookChild
  Branch {subElms} -> all nodeEmpty subElms
  Fixtures {fixtures} -> null fixtures



