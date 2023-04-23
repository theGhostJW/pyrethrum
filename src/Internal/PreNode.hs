module Internal.PreNode where

import Control.DeepSeq (NFData)
import Internal.RunTimeLogging (MessageLogger, Loc)
import Language.Haskell.TH (ExpQ)
import PyrethrumExtras (txt, uu, (?))
import qualified Text.Extra as T
import UnliftIO (MonadUnliftIO, STM, TMVar)

type PreNodeRoot = PreNode () ()

data Test si ti ii = Test
  { id :: Text
  , test :: Context -> si -> ti -> ii -> IO ()
  }

data Context = Context
  { loc :: Loc
  , logger :: MessageLogger
  }

-- this would be genrated by the TH
data Fixture oi ti tsti where
  Fixture ::
    { id :: Text
    , maxThreads :: Maybe Int
    , onceHook :: OnceHook oi oo
    , threadHook :: ThreadHook oo ti to
    , testHook :: TestHook oo to tsti tsto
    , tests :: [Test oo to tsto]
    } ->
    Fixture oi ti tsti

-- f1 :: Fixture Int Text Bool
-- f1 =
--   Fixture
--     { id = "fixture"
--     , onceHook = \l int -> pure $ txt int
--     , threadHook = \l otxt tiTxt -> pure $ T.length otxt + T.length tiTxt
--     , testHook = \l tx int b -> b ? pure b $ error tx
--     , testHookRelease = \l b -> b ? pure () $ error "its false"
--     , onceHookRelease = \l txt' -> putText txt'
--     , threadHookRelease = \l i -> print . txt $ i + 1
--     , tests = []
--     }
data PreNode oi ti where
  Group ::
    { title :: Text
    , maxThreads :: Maybe Int
    , onceHook :: OnceHook oi oo
    , threadHook :: ThreadHook oo ti to
    , subNodes :: [PreNode oo to]
    } ->
    PreNode oi ti
  Fixtures ::
    { title :: Text
    , maxThreads :: Maybe Int
    , testHook :: TestHook oi ti () tsto
    , fixtures :: [Fixture oi ti tsto]
    } ->
    PreNode oi ti

data OnceHook oi oo where
  OnceNone :: OnceHook oi oi
  OnceBefore ::
    { hook :: Context -> oi -> IO oo
    } ->
    OnceHook oi oo
  OnceAfter ::
    { releaseOnly :: Context -> oi -> IO ()
    } ->
    OnceHook oi oi
  OnceAround ::
    { hook :: Context -> oi -> IO oo
    , release :: Context -> oo -> IO ()
    } ->
    OnceHook oi oo

data ThreadHook oi ti to where
  ThreadNone :: ThreadHook oi ti ti
  ThreadBefore ::
    { hook :: Context -> oi -> ti -> IO to
    } ->
    ThreadHook oi ti to
  ThreadAfter ::
    { releaseOnly :: Context -> ti -> IO ()
    } ->
    ThreadHook oi ti ti
  ThreadAround ::
    { hook :: Context -> oi -> ti -> IO to
    , release :: Context -> to -> IO ()
    } ->
    ThreadHook oi ti to

data TestHook oi ti tsti tsto where
  TestNone :: TestHook oi ti tsti tsti
  TestBefore ::
    { hook :: Context -> oi -> ti -> tsti -> IO tsto
    } ->
    TestHook oi ti tsti tsto
  TestAfter ::
    { releaseOnly :: Context -> tsti -> IO ()
    } ->
    TestHook oi ti tsti tsti
  TestAround ::
    { hook :: Context -> oi -> ti -> tsti -> IO tsto
    , release :: Context -> tsto -> IO ()
    } ->
    TestHook oi ti tsti tsto

-- data ThreadHook oi ti to where
--   ThreadNone :: () -> ThreadHook oi ti ti
--   ThreadBefore :: Context-> oi -> IO oo -> ThreadHook oi ti to
--   ThreadAround :: Context-> oi -> IO oo -> ThreadHook oi ti to

-- todo:
-- simplify GADTs :: Done
-- change name of tag :: Done
-- get rid of maybe on tag :: Done
-- change iterations name :: Done
-- get rid of branches :: Done
-- remove once / threadHooks from Fixtures :: Done
-- collapse threadHook and onceHook types :: Done
-- change fixtures from test to fixtures :: Done
-- rewrite executeNode:
  -- XTest
  -- XFixture
  -- XFixtures
  -- XGroup
-- check references to status - replace / remove
-- reimplement uu
-- tree shake / validate - prenodes (unique ids)
-- basic tests
-- FIX :: 
-- need to use subElmIdx to create a unique loc
-- loc should not  include event type it should be node address
-- get rid of getStatus ??
-- stm bool on once hook executing
-- \case direct functions for done / can run
-- reinstate tests
-- root becomes ones hook with OnceNone as hook

nodeEmpty :: PreNode oi ti -> Bool
nodeEmpty = \case
  Group{subNodes} -> all nodeEmpty subNodes
  Fixtures{fixtures} -> null fixtures
