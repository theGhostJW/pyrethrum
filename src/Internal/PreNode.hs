module Internal.PreNode (
  Context (..),
  Fixture (..),
  OnceHook (..),
  PreNode (..),
  PreNodeRoot,
  Test (..),
  TestHook (..),
  ThreadHook (..),
  nodeEmpty,
) where

import Internal.RunTimeLogging (Loc, MessageLogger)

type PreNodeRoot = PreNode () ()

data Test si ti ii = Test
  { id :: Text
  , test :: Context -> si -> ti -> ii -> IO ()
  }

data Context = Context
  { loc :: Loc
  , logger :: MessageLogger
  }

data Fixture oi ti tsti where
  Fixture ::
    { title :: Text
    , maxThreads :: Maybe Int
    , onceHook :: OnceHook oi oo
    , threadHook :: ThreadHook oo ti to
    , testHook :: TestHook oo to tsti tsto
    , tests :: NonEmpty (Test oo to tsto)
    } ->
    Fixture oi ti tsti

data PreNode oi ti where
  Group ::
    { title :: Text
    , threadLimit :: Maybe Int
    , onceHook :: OnceHook oi oo
    , threadHook :: ThreadHook oo ti to
    , subNodes :: NonEmpty (PreNode oo to)
    } ->
    PreNode oi ti
  Fixtures ::
    { title :: Text
    , threadLimit :: Maybe Int
    , testHook :: TestHook oi ti () tsto
    , fixtures :: NonEmpty (Fixture oi ti tsto)
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

nodeEmpty :: PreNode oi ti -> Bool
nodeEmpty = \case
  Group{subNodes} -> all nodeEmpty subNodes
  Fixtures{fixtures} -> null fixtures
