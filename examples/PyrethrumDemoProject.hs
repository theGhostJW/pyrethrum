module PyrethrumDemoProject where

import qualified Check as CH
import qualified Core as C
import DSL.FileSystemEffect (FSException, FileSystem)
import DSL.Internal.ApEvent (ApEvent)
import DSL.Out (Out)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Effectful (Eff, IOE, type (:>))
import Effectful.Error.Static as E (Error)
import qualified DSL.Internal.ApEvent as AE

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
  , maxThreads :: Int
  , depth :: Depth
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestConfig)

instance C.Config TestConfig

data Hook loc i o where
  Before ::
    (C.Param loc) =>
    { action :: RunConfig -> Action o
    } ->
    Hook loc () o
  Before' ::
    (C.Param ploc, C.Param loc, C.ValidDepends ploc loc) =>
    { depends :: Hook ploc pi i
    , action' :: RunConfig -> i -> Action o
    } ->
    Hook loc i o
  After ::
    (C.Param loc) =>
    { afterAction :: RunConfig -> Action ()
    } ->
    Hook loc () ()
  After' ::
    (C.Param ploc, C.Param loc, C.ValidDepends ploc loc) =>
    { afterDepends :: Hook ploc pi i
    , afterAction' :: RunConfig -> Action ()
    } ->
    Hook loc i i
  Around ::
    (C.Param loc) =>
    { setup :: RunConfig -> Action o
    , teardown :: RunConfig -> o -> Action ()
    } ->
    Hook loc () o
  Around' ::
    (C.Param ploc, C.Param loc, C.ValidDepends ploc loc) =>
    { depends :: Hook ploc pi i
    , setup' :: RunConfig -> i -> Action o
    , teardown' :: RunConfig -> o -> Action ()
    } ->
    Hook loc i o

{-
TODO: 
      - UX of after hook looks sus
       - how do I do a test with an each in and a once after
       - once after and thread after
      - split datatypes with conversion typeclasses at project level
-}

data Test hi where
  Full ::
    (C.Item i ds, ToJSON as) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action as
    , parse :: as -> Eff '[E.Error C.ParseException] ds
    , items :: RunConfig -> [i]
    } ->
    Test ()
  Full' ::
    (C.Item i ds, ToJSON as, C.Param loc) =>
    { depends :: Hook loc pi a
    , config' :: TestConfig
    , action' :: RunConfig -> a -> i -> Action as
    , parse' :: as -> Eff '[E.Error C.ParseException] ds
    , items' :: RunConfig -> [i]
    } ->
    Test a
  NoParse ::
    forall i ds.
    (C.Item i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action ds
    , items :: RunConfig -> [i]
    } ->
    Test ()
  NoParse' ::
    (C.Item i ds, C.Param loc) =>
    { depends :: Hook loc pi a
    , config' :: TestConfig
    , action' :: RunConfig -> a -> i -> Action ds
    , items' :: RunConfig -> [i]
    } ->
    Test a
  Single ::
    (ToJSON as) =>
    { config :: TestConfig
    , singleAction :: RunConfig -> Action as
    , checks :: CH.Checks as
    } ->
    Test ()
  Single' ::
    (ToJSON as, C.Param loc) =>
    { depends :: Hook loc pi a
    , config' :: TestConfig
    , singleAction' :: RunConfig -> a -> Action as
    , checks' :: CH.Checks as
    } ->
    Test a

type Suite = [SuiteElement ()]
data SuiteElement i where
  Hook ::
    (C.Param loc) =>
    { path :: AE.Path
    , hook :: Hook loc i o
    , subNodes :: [SuiteElement o]
    } ->
    SuiteElement i
  Test ::
    { path :: AE.Path
    , test :: Test i
    } ->
    SuiteElement i

mkTest :: Test hi -> C.Test [] RunConfig TestConfig ApEffs hi
mkTest = \case
  Full{..} -> C.Full{..}
  NoParse{..} -> C.NoParse{..}
  Full'{..} -> C.Full' (mkHook depends) config' action' parse' items'
  NoParse'{..} -> C.NoParse'{depends = mkHook depends, ..}
  Single{..} -> C.Single{..}
  Single'{..} -> C.Single' (mkHook depends) config' singleAction' checks'

mkHook :: Hook loc i o -> C.Hook RunConfig ApEffs loc i o
mkHook = \case
  Before{..} -> C.Before{..}
  Before'{..} -> C.Before' (mkHook depends) action'
  After{..} -> C.After{..}
  After'{..} -> C.After'{afterDepends = mkHook afterDepends, ..}
  Around{..} -> C.Around{..}
  Around'
    { depends
    , setup'
    , teardown'
    } ->
      C.Around' (mkHook depends) setup' teardown'

mkSuite :: SuiteElement i -> C.SuiteElement [] RunConfig TestConfig ApEffs i
mkSuite = \case
  Hook{..} ->
    C.Hook
      { hook = mkHook hook
      , subNodes = mkSuite <$> subNodes
      , ..
      }
  Test{..} -> C.Test{test = mkTest test, ..}

mkTestRun :: Suite -> [C.SuiteElement [] RunConfig TestConfig ApEffs ()]
mkTestRun tr = mkSuite <$> tr
