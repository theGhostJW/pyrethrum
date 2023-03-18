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
  OnceHook ::
    { title :: Text,
      hook :: OnceHook oi oo,
      onceSubNodes :: [PreNode oo ti]
    } ->
    PreNode oi ti 
  ThreadHook ::
    { title :: Text,
      threadHook :: Loc -> ApLogger -> oi -> ti -> IO tn,
      threadSubNodes :: [PreNode oi tn],
      threadHookRelease :: Loc -> ApLogger -> tn -> IO ()
    } ->
    PreNode oi ti
  Fixtures :: { 
      testHook :: Loc -> ApLogger -> oi -> ti -> IO io,
      testHookRelease :: Loc -> ApLogger -> io -> IO (),
      title :: Text,
      fixtures :: [Test oi ti io]
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
  ThreadBefore :: Loc -> ApLogger -> oi -> ti -> IO to -> ThreadHook oi ti to
  ThreadAround :: {
    hook :: Loc -> ApLogger -> oi -> ti -> IO to -> ThreadHook oi ti to,
    release :: Loc -> ApLogger -> to -> IO ()
  } -> ThreadHook oi ti to

data TestHook oi ti tsto where 
  TestNone :: () -> TestHook i ti ()
  TestBefore :: Loc -> ApLogger -> oi -> ti -> IO tsto -> TestHook i ti tsto
  TestAround :: {
    hook :: Loc -> ApLogger -> oi -> ti -> IO tsto -> TestHook oi ti tsto,
    release :: Loc -> ApLogger -> tsto -> IO ()
   } -> TestHook oi ti tsto


-- data ThreadHook oi ti to where 
--   ThreadNone :: () -> ThreadHook oi ti ti
--   ThreadBefore :: Loc -> ApLogger -> oi -> IO oo -> ThreadHook oi ti to
--   ThreadAround :: Loc -> ApLogger -> oi -> IO oo -> ThreadHook oi ti to


    -- todo: 
    -- simplify GADTs :: Done
    -- change name of tag :: Done
    -- get rid of maybe on tag :: Done
    -- change iterations name :: Done
    -- get rid of branches :: Done
    -- remove once / threadHooks from Fixtures :: Done
    -- how release without a hook
      -- data Hook hook release where 
            -- None |  -- passes through types
            -- Before a | 
            -- After b | 
            -- Around a b
    -- change fixtures from test to fixtures
    -- collapse threadHook and onceHook types
    -- rewrite executeNode
      -- get rid of getStatus 
        -- stm bool on once hook executing 
        -- \case direct functions for done / can run
    -- reinstate tests
    -- root becomes ones hook with OnceNone as hook

nodeEmpty :: PreNode oi ti  -> Bool
nodeEmpty = \case
  OnceHook {onceSubNodes} -> all nodeEmpty onceSubNodes
  ThreadHook {threadSubNodes} -> all nodeEmpty threadSubNodes
  Fixtures {fixtures} -> null fixtures



