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

type PreNodeRoot a = PreNode a () ()

data Test a si ti ii = Test
  { id :: Text
  , test :: Context a -> si -> ti -> ii -> IO ()
  }

data Context a = Context
  { loc :: Loc
  , logger :: MessageLogger a
  }

data Fixture a oi ti tsti where
  Fixture ::
    { title :: Text
    , maxThreads :: Maybe Int
    , onceHook :: OnceHook a oi oo
    , threadHook :: ThreadHook a oo ti to
    , testHook :: TestHook a oo to tsti tsto
    , tests :: NonEmpty (Test a oo to tsto)
    } ->
    Fixture a oi ti tsti

data PreNode a oi ti where
  Group ::
    { title :: Text
    , threadLimit :: Maybe Int
    , onceHook :: OnceHook a oi oo
    , threadHook :: ThreadHook a oo ti to
    , subNodes :: NonEmpty (PreNode a oo to)
    } ->
    PreNode a oi ti
  Fixtures ::
    { title :: Text
    , threadLimit :: Maybe Int
    , testHook :: TestHook a oi ti () tsto
    , fixtures :: NonEmpty (Fixture a oi ti tsto)
    } ->
    PreNode a oi ti

data OnceHook a oi oo where
  OnceNone :: OnceHook a oi oi
  OnceBefore ::
    { hook :: Context a -> oi -> IO oo
    } ->
    OnceHook a oi oo
  OnceAfter ::
    { releaseOnly :: Context a -> oi -> IO ()
    } ->
    OnceHook a oi oi
  OnceAround ::
    { hook :: Context a -> oi -> IO oo
    , release :: Context a -> oo -> IO ()
    } ->
    OnceHook a oi oo

data ThreadHook a oi ti to where
  ThreadNone :: ThreadHook a oi ti ti
  ThreadBefore ::
    { hook :: Context a -> oi -> ti -> IO to
    } ->
    ThreadHook a oi ti to
  ThreadAfter ::
    { releaseOnly :: Context a -> ti -> IO ()
    } ->
    ThreadHook a oi ti ti
  ThreadAround ::
    { hook :: Context a -> oi -> ti -> IO to
    , release :: Context a -> to -> IO ()
    } ->
    ThreadHook a oi ti to

data TestHook a oi ti tsti tsto where
  TestNone :: TestHook a oi ti tsti tsti
  TestBefore ::
    { hook :: Context a -> oi -> ti -> tsti -> IO tsto
    } ->
    TestHook a oi ti tsti tsto
  TestAfter ::
    { releaseOnly :: Context a -> tsti -> IO ()
    } ->
    TestHook a oi ti tsti tsti
  TestAround ::
    { hook :: Context a -> oi -> ti -> tsti -> IO tsto
    , release :: Context a -> tsto -> IO ()
    } ->
    TestHook a oi ti tsti tsto

nodeEmpty :: PreNode a oi ti -> Bool
nodeEmpty = \case
  Group{subNodes} -> all nodeEmpty subNodes
  Fixtures{fixtures} -> null fixtures
