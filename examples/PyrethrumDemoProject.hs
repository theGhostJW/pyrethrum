module PyrethrumDemoProject where

import Check qualified as CH
import Core qualified as C
import DSL.FileSystemEffect (FSException, FileSystem)
import DSL.Internal.ApEvent (ApEvent)
import DSL.Internal.ApEvent qualified as AE
import DSL.Out (Out)
import Data.Aeson (ToJSON)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Effectful (Eff, IOE, type (:>))
import Effectful.Error.Static as E (Error)

type ApEffs = '[FileSystem, Out ApEvent, E.Error FSException, IOE]
type Action a = Eff '[FileSystem, Out ApEvent, E.Error FSException, IOE] a
type ApConstraints es = (FileSystem :> es, Out ApEvent :> es, Error FSException :> es, IOE :> es)
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
    (C.Item i ds, ToJSON as) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action as
    , parse :: as -> Eff '[E.Error C.ParseException] ds
    , items :: RunConfig -> [i]
    } ->
    Fixture ()
  Full' ::
    (C.Item i ds, ToJSON as, C.Frequency hz) =>
    { depends :: Hook hz pw pi a
    , config' :: TestConfig
    , action' :: RunConfig -> a -> i -> Action as
    , parse' :: as -> Eff '[E.Error C.ParseException] ds
    , items' :: RunConfig -> [i]
    } ->
    Fixture a
  NoParse ::
    forall i ds.
    (C.Item i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action ds
    , items :: RunConfig -> [i]
    } ->
    Fixture ()
  NoParse' ::
    (C.Item i ds, C.Frequency hz) =>
    { depends :: Hook hz pw pi a
    , config' :: TestConfig
    , action' :: RunConfig -> a -> i -> Action ds
    , items' :: RunConfig -> [i]
    } ->
    Fixture a
  Single ::
    (ToJSON as) =>
    { config :: TestConfig
    , singleAction :: RunConfig -> Action as
    , checks :: CH.Checks as
    } ->
    Fixture ()
  Single' ::
    (ToJSON as, C.Frequency hz) =>
    { depends :: Hook hz pw pi a
    , config' :: TestConfig
    , singleAction' :: RunConfig -> a -> Action as
    , checks' :: CH.Checks as
    } ->
    Fixture a

type Suite = [Node ()]
data Node i where
  Hook ::
    (C.Frequency hz) =>
    { path :: AE.Path
    , hook :: Hook hz pw i o
    , subNodes :: [Node o]
    } ->
    Node i
  Test ::
    { path :: AE.Path
    , test :: Fixture i
    } ->
    Node i

mkTest :: Fixture hi -> C.Test [] RunConfig TestConfig ApEffs hi
mkTest = \case
  Full{..} -> C.Full{..}
  NoParse{..} -> C.NoParse{..}
  Full'{..} -> C.Full' (mkHook depends) config' action' parse' items'
  NoParse'{..} -> C.NoParse'{depends = mkHook depends, ..}
  Single{..} -> C.Single{..}
  Single'{..} -> C.Single' (mkHook depends) config' singleAction' checks'

mkHook :: Hook hz pw i o -> C.Hook RunConfig ApEffs hz i o
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

mkSuite :: Node i -> C.Node [] RunConfig TestConfig ApEffs i
mkSuite = \case
  Hook{..} ->
    C.Hook
      { hook = mkHook hook
      , subNodes = mkSuite <$> subNodes
      , ..
      }
  Test{..} -> C.Fixture{fixture = mkTest test, ..}

mkTestRun :: Suite -> [C.Node [] RunConfig TestConfig ApEffs ()]
mkTestRun tr = mkSuite <$> tr
