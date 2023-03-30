module Internal.PreNode where

import Control.DeepSeq (NFData)
import Internal.RunTimeLogging (ApLogger, Loc)
import Language.Haskell.TH (ExpQ)
import PyrethrumExtras (txt, uu, (?))
import qualified Text.Extra as T
import UnliftIO (MonadUnliftIO, STM, TMVar)

type PreNodeRoot = PreNode () ()

data Test si ti ii = Test
  { tstId :: Text
  , tst :: ApLogger -> si -> ti -> ii -> IO ()
  }

-- this would be genrated by the TH
data Fixture oi ti ii where
  Fixture ::
    { id :: Text
    , onceHook :: ApLogger -> oi -> IO oo
    , onceHookRelease :: ApLogger -> oo -> IO ()
    , threadHook :: ApLogger -> oo -> ti -> IO to
    , threadHookRelease :: ApLogger -> to -> IO ()
    , testHook :: ApLogger -> oo -> to -> ii -> IO io
    , testHookRelease :: ApLogger -> io -> IO ()
    , tests :: [Test oo to io]
    } ->
    Fixture oi ti ii

f1 :: Fixture Int Text Bool
f1 =
  Fixture
    { id = "fixture"
    , onceHook = \l int -> pure $ txt int
    , threadHook = \l otxt tiTxt -> pure $ T.length otxt + T.length tiTxt
    , testHook = \l tx int b -> b ? pure b $ error tx
    , testHookRelease = \l b -> b ? pure () $ error "its false"
    , onceHookRelease = \l txt' -> putText txt'
    , threadHookRelease = \l i -> print . txt $ i + 1
    , tests = []
    }
data PreNode oi ti where
  OnceHook ::
    { title :: Text
    , onceHook :: OnceHook oi oo
    , threadHooko :: ThreadHook oo ti to
    , onceSubNodes :: [PreNode oo to]
    } ->
    PreNode oi ti
  -- ThreadHook ::
  --   { title :: Text
  --   , threadHook :: ThreadHook oi ti to
  --   , threadSubNodes :: [PreNode oi to]
  --   } ->
  --   PreNode oi ti
  Fixtures ::
    { title :: Text
    , testHook :: TestHook oi ti tsto
    , fixtures :: [Test oi ti tsto]
    } ->
    PreNode oi ti

data OnceHook oi oo where
  OnceNone :: OnceHook oi oi
  OnceBefore ::
    { hook :: Loc -> ApLogger -> oi -> IO oo
    } ->
    OnceHook oi oo
  OnceAfter ::
    { releaseOnly :: Loc -> ApLogger -> oi -> IO ()
    } ->
    OnceHook oi oi
  OnceAround ::
    { hook :: Loc -> ApLogger -> oi -> IO oo
    , release :: Loc -> ApLogger -> oo -> IO ()
    } ->
    OnceHook oi oo

data ThreadHook oi ti to where
  ThreadNone :: ThreadHook oi ti ti
  ThreadBefore ::
    { hook :: Loc -> ApLogger -> oi -> ti -> IO to
    } ->
    ThreadHook oi ti to
  ThreadAfter ::
    { releaseOnly :: Loc -> ApLogger -> ti -> IO ()
    } ->
    ThreadHook oi ti ti
  ThreadAround ::
    { hook :: Loc -> ApLogger -> oi -> ti -> IO to
    , release :: Loc -> ApLogger -> to -> IO ()
    } ->
    ThreadHook oi ti to

data TestHook oi ti tsto where
  TestNone :: TestHook oi ti ()
  TestBefore ::
    { hook :: Loc -> ApLogger -> oi -> ti -> IO tsto
    } ->
    TestHook oi ti tsto
  TestAfter ::
    { releaseOnly :: Loc -> ApLogger -> IO ()
    } ->
    TestHook oi ti ()
  TestAround ::
    { hook :: Loc -> ApLogger -> oi -> ti -> IO tsto
    , release :: Loc -> ApLogger -> tsto -> IO ()
    } ->
    TestHook oi ti tsto

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
-- collapse threadHook and onceHook types
-- loc should not  include event type it should be node address
-- change fixtures from test to fixtures
-- rewrite executeNode
-- reimplement uu
-- get rid of getStatus ??
-- stm bool on once hook executing
-- \case direct functions for done / can run
-- reinstate tests
-- root becomes ones hook with OnceNone as hook

nodeEmpty :: PreNode oi ti -> Bool
nodeEmpty = \case
  OnceHook{onceSubNodes} -> all nodeEmpty onceSubNodes
  -- ThreadHook{threadSubNodes} -> all nodeEmpty threadSubNodes
  Fixtures{fixtures} -> null fixtures



