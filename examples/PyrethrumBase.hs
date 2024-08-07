module PyrethrumBase (
  Action,
  Depth (..),
  Fixture (..),
  Hook (..),
  LogEffs,
  Node (..),
  RunConfig (..),
  C.DataSource(..),
  Suite,
  TestConfig (..),
  testConfig,
  HasLog,
  mkTestRun
) where

import Core qualified as C
import DSL.FileSystemEffect (FSException, FileSystem)
import DSL.Internal.NodeEvent (NodeEvent)
import DSL.Internal.NodeEvent qualified as AE
import DSL.Out (Out)
import Effectful (Eff, IOE, type (:>))
import Effectful.Error.Static as E (Error)
import PyrethrumConfigTypes as CG
    ( Depth(..), RunConfig(..), TestConfig(..), testConfig ) 

--  these will probably be split off and go into core later
type Action = Eff ApEffs
type HasLog es = Out NodeEvent :> es
type LogEffs a = forall es. (Out NodeEvent :> es) => Eff es a
type ApEffs = '[FileSystem, Out NodeEvent, E.Error FSException, IOE]

-- type ApConstraints es = (FileSystem :> es, Out NodeEvent :> es, Error FSException :> es, IOE :> es)
-- type AppEffs a = forall es. (FileSystem :> es, Out NodeEvent :> es, Error FSException :> es, IOE :> es) => Eff es a

-- TODO: research StrictSTM

data Hook hz when input output where
  BeforeHook ::
    (C.Frequency hz) =>
    { action :: RunConfig -> Action o
    } ->
    Hook hz C.Before () o
  BeforeHook' ::
    (C.Frequency phz, C.Frequency hz, C.CanDependOn hz phz) =>
    { depends :: Hook phz pw pi i
    , action' :: RunConfig -> i -> Action o
    } ->
    Hook hz C.Before i o
  AfterHook ::
    (C.Frequency hz) =>
    { afterAction :: RunConfig -> Action ()
    } ->
    Hook hz C.After () ()
  AfterHook' ::
    (C.Frequency phz, C.Frequency hz, C.CanDependOn hz phz) =>
    { afterDepends :: Hook phz pw pi i
    , afterAction' :: RunConfig -> Action ()
    } ->
    Hook hz C.After i i
  AroundHook ::
    (C.Frequency hz) =>
    { setup :: RunConfig -> Action o
    , teardown :: RunConfig -> o -> Action ()
    } ->
    Hook hz C.Around () o
  AroundHook' ::
    (C.Frequency phz, C.Frequency hz, C.CanDependOn hz phz) =>
    { aroundDepends :: Hook phz pw pi i
    , setup' :: RunConfig -> i -> Action o
    , teardown' :: RunConfig -> o -> Action ()
    } ->
    Hook hz C.Around i o

data Fixture hi where
  Full ::
    (C.Item i ds, Show as) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action as
    , parse :: as -> Either C.ParseException ds
    , items :: RunConfig -> C.DataSource i
    } ->
    Fixture ()
  Full' ::
    (C.Item i ds, Show as, C.Frequency hz) =>
    { config' :: TestConfig
    , depends :: Hook hz pw pi a
    , action' :: RunConfig -> a -> i -> Action as
    , parse' :: as -> Either C.ParseException ds
    , items' :: RunConfig -> C.DataSource i
    } ->
    Fixture a
  Direct ::
    forall i ds.
    (C.Item i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action ds
    , items :: RunConfig -> C.DataSource i
    } ->
    Fixture ()
  Direct' ::
    (C.Item i ds, C.Frequency hz) =>
    { config' :: TestConfig
    , depends :: Hook hz pw pi a
    , action' :: RunConfig -> a -> i -> Action ds
    , items' :: RunConfig -> C.DataSource i
    } ->
    Fixture a

type Suite = [Node ()]
data Node i where
  Hook ::
    (C.Frequency hz) =>
    { path :: AE.Path
    , hook :: Hook hz when i o
    , subNodes :: [Node o]
    } ->
    Node i
  Fixture ::
    { path :: AE.Path
    , fixture :: Fixture i
    } ->
    Node i

mkFixture :: Fixture hi -> C.Fixture Action RunConfig TestConfig hi
mkFixture = \case
  Full{..} -> C.Full{..}
  Direct{..} -> C.Direct{..}
  Full'{..} -> C.Full' config' (mkHook depends) action' parse' items'
  Direct'{..} -> C.Direct'{depends = mkHook depends, ..}

mkTestRun :: Suite -> [C.Node Action RunConfig TestConfig ()]
mkTestRun tr = mkNode <$> tr

mkHook :: Hook hz pw i o -> C.Hook (Eff ApEffs) RunConfig hz i o
mkHook = \case
  BeforeHook{..} -> C.Before{..}
  BeforeHook'{..} -> C.Before' (mkHook depends) action'
  AfterHook{..} -> C.After{..}
  AfterHook'{..} -> C.After'{afterDepends = mkHook afterDepends, ..}
  AroundHook{..} -> C.Around{..}
  AroundHook'
    { aroundDepends
    , setup'
    , teardown'
    } ->
      C.Around' (mkHook aroundDepends) setup' teardown'

mkNode :: Node i -> C.Node Action RunConfig TestConfig i
mkNode = \case
  Hook{..} ->
    C.Hook
      { hook = mkHook hook
      , subNodes = mkNode <$> subNodes
      , ..
      }
  Fixture{..} -> C.Fixture{fixture = mkFixture fixture, ..}
