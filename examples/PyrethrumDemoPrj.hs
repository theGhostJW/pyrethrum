module PyrethrumDemoPrj where

import qualified Core as C
import Data.Aeson.TH
import Data.Aeson.Types (
  FromJSON,
  ToJSON (toJSON),
  Value (String),
 )

import qualified DSL.FileSystemEffect as IOI
import qualified DSL.Internal.ApEvent as AE

import DSL.FileSystemEffect
import DSL.Internal.ApEvent
import DSL.Out
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, runError)
import PyrethrumExtras (txt, uu)

type ApEffs = '[FileSystem, Out ApEvent, Error FSException, IOE]
type Action a = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] a
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

$(deriveJSON defaultOptions ''TestConfig)

instance C.Config TestConfig

-- type Fixture a = AbstractFixture RunConfig TestConfig ApEffs a

data Fixture loc a where
  -- once hooks
  OnceBefore ::
    { onceAction :: RunConfig -> Action a
    } ->
    Fixture C.OnceParent a
  OnceBefore' ::
    forall loc a b.
    (C.OnceParam loc) =>
    { onceParent :: Fixture loc a
    , onceAction' :: a -> RunConfig -> Action b
    } ->
    Fixture C.OnceParent b
  OnceAfter ::
    forall loc.
    (C.OnceAfterParam loc) =>
    { onceBefore :: Fixture loc ()
    , onceAfter :: RunConfig -> Action ()
    } ->
    Fixture C.OnceAfter ()
  OnceResource ::
    { onceSetup :: RunConfig -> Action a
    , onceTearDown :: a -> Action ()
    } ->
    Fixture C.OnceParent ()
  OnceResource' ::
    forall loc a b.
    (C.OnceParam loc) =>
    { onceResourceParent :: Fixture loc a
    , onceSetup' :: a -> RunConfig -> Action b
    , onceTearDown' :: b -> Action ()
    } ->
    Fixture C.OnceParent a
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: RunConfig -> Action a
    } ->
    Fixture C.ThreadParent a
  ThreadBefore' ::
    forall loc a b.
    (C.ThreadParam loc) =>
    { threadParent :: Fixture loc a
    , threadAction' :: a -> RunConfig -> Action b
    } ->
    Fixture C.ThreadParent b
  ThreadAfter ::
    forall loc.
    (C.ThreadAfterParam loc) =>
    { threadBefore :: Fixture loc ()
    , threadAfterAction :: RunConfig -> Action ()
    } ->
    Fixture C.ThreadAfter ()
  ThreadResource ::
    { threadSetup :: RunConfig -> Action a
    , threadTearDown :: a -> Action ()
    } ->
    Fixture C.ThreadParent ()
  ThreadResource' ::
    forall loc a b.
    (C.ThreadParam loc) =>
    { threadResourceParent :: Fixture loc a
    , threadSetup' :: a -> RunConfig -> Action b
    , threadTearDown' :: b -> Action ()
    } ->
    Fixture C.ThreadParent a
  -- each hooks
  EachBefore ::
    { eachAction :: RunConfig -> Action a
    } ->
    Fixture C.EachParent a
  EachBefore' ::
    forall loc a b.
    (C.EachParam loc) =>
    { eachParent :: Fixture loc a
    , eachAction' :: a -> RunConfig -> Action b
    } ->
    Fixture C.EachParent b
  EachAfter ::
    forall loc.
    (C.EachAfterParam loc) =>
    { eachBefore :: Fixture loc ()
    , eachAfterAction :: RunConfig -> Action ()
    } ->
    Fixture C.EachAfter ()
  EachResource ::
    { eachSetup :: RunConfig -> Action a
    , eachTearDown :: a -> Action ()
    } ->
    Fixture C.EachParent ()
  EachResource' ::
    forall loc a b. (C.EachParam loc) =>
    { eachResourceParent :: Fixture loc a
    , eachSetup' :: a -> RunConfig -> Action b
    , eachTearDown' :: b -> Action ()
    } ->
    Fixture C.EachParent a
  Test ::
    { test :: Test
    } ->
    Fixture C.Test ()

type TestFixture = Fixture C.Test ()
data Test where
  Full ::
    forall i as ds.
    (C.ItemClass i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action as
    , parse :: as -> Either C.ParseException ds
    , items :: RunConfig -> [i]
    } ->
    Test
  NoParse ::
    forall i ds.
    (C.ItemClass i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action ds
    , items :: RunConfig -> [i]
    } ->
    Test
  Full' ::
    forall i as ds loc a.
    (C.ItemClass i ds) =>
    { parent :: (C.EachParam loc) => Fixture loc a
    , config :: TestConfig
    , childAction :: a -> RunConfig -> i -> Action as
    , parse :: as -> Either C.ParseException ds
    , items :: RunConfig -> [i]
    } ->
    Test
  NoParse' ::
    forall i ds loc a.
    (C.ItemClass i ds) =>
    { parent :: (C.EachParam loc) => Fixture loc a
    , config :: TestConfig
    , childAction :: a -> RunConfig -> i -> Action ds
    , items :: RunConfig -> [i]
    } ->
    Test
  Single ::
    { config :: TestConfig
    , singleAction :: RunConfig -> Action as
    , checks :: C.Checks as
    } ->
    Test
  Single' ::
    { parent :: (C.EachParam loc) => Fixture loc a
    , config :: TestConfig
    , childSingleAction :: a -> RunConfig -> Action as
    , checks :: C.Checks as
    } ->
    Test

mkAbstractTest :: Test -> C.AbstractTest RunConfig TestConfig ApEffs
mkAbstractTest = \case
  Full{..} -> C.Full{..}
  NoParse{..} -> C.NoParse{..}
  Full'{..} -> C.Full' (mkAbstractFx parent) config childAction parse items
  NoParse'{..} -> C.NoParse' (mkAbstractFx parent) config childAction items
  Single{..} -> C.Single{..}
  Single'{..} -> C.Single' (mkAbstractFx parent) config childSingleAction checks

mkAbstractFx :: Fixture loc a -> C.AbstractFixture RunConfig TestConfig ApEffs loc a
mkAbstractFx = \case
  OnceBefore{..} -> C.OnceBefore{..}
  OnceBefore'{..} -> C.OnceBefore' (mkAbstractFx onceParent) onceAction'
  OnceAfter{..} -> C.OnceAfter (mkAbstractFx onceBefore) onceAfter
  OnceResource'
    { onceResourceParent
    , onceSetup'
    , onceTearDown'
    } ->
      C.OnceResource' (mkAbstractFx onceResourceParent) onceSetup' onceTearDown'
  OnceResource{..} -> C.OnceResource{..}
  ThreadBefore{..} -> C.ThreadBefore{..}
  ThreadBefore'{..} -> C.ThreadBefore' (mkAbstractFx threadParent) threadAction'
  ThreadAfter{..} -> C.ThreadAfter (mkAbstractFx threadBefore) threadAfterAction
  ThreadResource{..} -> C.ThreadResource{..}
  ThreadResource'{threadResourceParent = p, ..} -> C.ThreadResource' (mkAbstractFx p) threadSetup' threadTearDown'
  EachBefore{..} -> C.EachBefore{..}
  EachBefore'{eachParent, eachAction'} -> C.EachBefore' (mkAbstractFx eachParent) eachAction'
  EachAfter{..} -> C.EachAfter (mkAbstractFx eachBefore) eachAfterAction
  EachResource{..} -> C.EachResource{..}
  EachResource'{eachResourceParent, eachSetup', eachTearDown'} -> C.EachResource' (mkAbstractFx eachResourceParent) eachSetup' eachTearDown'
  Test{test} -> C.Test $ mkAbstractTest test
