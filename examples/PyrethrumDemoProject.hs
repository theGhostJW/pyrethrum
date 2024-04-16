module PyrethrumDemoProject where

import Core qualified as C
import DSL.FileSystemEffect (FSException, FileSystem)
import DSL.Internal.ApEvent (ApEvent)
import DSL.Internal.ApEvent qualified as AE
import DSL.Out (Out)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Effectful (Eff, IOE, type (:>))
import Effectful.Error.Static as E (Error)

type Action = Eff ApEffs
type HasLog es = Out ApEvent :> es
type LogEffs a = forall es. (Out ApEvent :> es) => Eff es a
type ApConstraints es = (FileSystem :> es, Out ApEvent :> es, Error FSException :> es, IOE :> es)
type ApEffs = '[FileSystem, Out ApEvent, E.Error FSException, IOE]
type AppEffs a = forall es. (FileSystem :> es, Out ApEvent :> es, Error FSException :> es, IOE :> es) => Eff es a

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum, Bounded)
$(deriveJSON defaultOptions ''Environment)

data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
$(deriveJSON defaultOptions ''Country)

data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)
$(deriveJSON defaultOptions ''Depth)

data RunConfig = RunConfig
  { title :: Text
  , environment :: Environment
  , maxThreads :: Int
  , country :: Country
  , depth :: Depth
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''RunConfig)

instance C.Config RunConfig

data TestConfig = TestConfig
  { title :: Text
  , depth :: Depth
  }
  deriving (Show, Eq)

newtype DefaultCfg = DefaultCfg
  { depth :: Depth
  }
  deriving (Show, Eq)

defaults :: DefaultCfg
defaults =
  DefaultCfg
    { depth = DeepRegression
    }

testConfig :: Text -> TestConfig
testConfig title =
  mkFull defaults
 where
  mkFull DefaultCfg{..} =
    TestConfig
      { ..
      }

$(deriveJSON defaultOptions ''TestConfig)

instance C.Config TestConfig

newtype NTHook hz i o = NTHook {unHook :: C.Hook Action RunConfig hz i o}

mkBefore :: (C.Frequency hz) => (RunConfig -> Action o) -> NTHook hz () o
mkBefore = NTHook . C.Before

mkBefore' :: forall {k1} {k2} {m :: * -> *} {rc} {phz :: k1} {pi} {i1}        {hz :: k2} {i2} {o}. C.Hook m rc phz pi i1 -> NTHook hz i2 o
mkBefore' = NTHook . C.Before'

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
  After' ::
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

{-
TODO:
      - UX of after hook looks sus
       - how do I do a test with an each in and a once after
       - once after and thread after
      - split datatypes with conversion typeclasses at project level
-}

data Fixture hi where
  Full ::
    (C.Item i ds, Show as) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action as
    , parse :: as -> Either C.ParseException ds
    , items :: RunConfig -> [i]
    } ->
    Fixture ()
  Full' ::
    (C.Item i ds, Show as, C.Frequency hz) =>
    { config' :: TestConfig
    , depends :: Hook hz pw pi a
    , action' :: RunConfig -> a -> i -> Action as
    , parse' :: as -> Either C.ParseException ds
    , items' :: RunConfig -> [i]
    } ->
    Fixture a
  Direct ::
    forall i ds.
    (C.Item i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action ds
    , items :: RunConfig -> [i]
    } ->
    Fixture ()
  Direct' ::
    (C.Item i ds, C.Frequency hz) =>
    { config' :: TestConfig
    , depends :: Hook hz pw pi a
    , action' :: RunConfig -> a -> i -> Action ds
    , items' :: RunConfig -> [i]
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

mkFixture :: Fixture hi -> C.Fixture Action [] RunConfig TestConfig hi
mkFixture = \case
  Full{..} -> C.Full{..}
  Direct{..} -> C.Direct{..}
  Full'{..} -> C.Full' config' (mkHook depends) action' parse' items'
  Direct'{..} -> C.Direct'{depends = mkHook depends, ..}

mkHook :: Hook hz pw i o -> C.Hook (Eff ApEffs) RunConfig hz i o
mkHook = \case
  BeforeHook{..} -> C.Before{..}
  BeforeHook'{..} -> C.Before' (mkHook depends) action'
  AfterHook{..} -> C.After{..}
  After'{..} -> C.After'{afterDepends = mkHook afterDepends, ..}
  AroundHook{..} -> C.Around{..}
  AroundHook'
    { aroundDepends
    , setup'
    , teardown'
    } ->
      C.Around' (mkHook aroundDepends) setup' teardown'

mkSuite :: Node i -> C.Node Action [] RunConfig TestConfig i
mkSuite = \case
  Hook{..} ->
    C.Hook
      { hook = mkHook hook
      , subNodes = mkSuite <$> subNodes
      , ..
      }
  Fixture{..} -> C.Fixture{fixture = mkFixture fixture, ..}

mkTestRun :: Suite -> [C.Node Action [] RunConfig TestConfig ()]
mkTestRun tr = mkSuite <$> tr
