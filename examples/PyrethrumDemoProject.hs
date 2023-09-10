module PyrethrumDemoProject where

import qualified Core as C
import DSL.FileSystemEffect (FSException, FileSystem)
import DSL.Internal.ApEvent (ApEvent)
import DSL.Out (Out)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Effectful (Eff, IOE, type (:>))
import Effectful.Error.Static as E (Error)
import PreNode

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
    Fixture C.Once a
  OnceBefore' ::
    forall loc a b.
    (C.OnceParam loc) =>
    { onceParent :: Fixture loc a
    , onceAction' :: a -> RunConfig -> Action b
    } ->
    Fixture C.Once b
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
    Fixture C.Once ()
  OnceResource' ::
    forall loc a b.
    (C.OnceParam loc) =>
    { onceResourceParent :: Fixture loc a
    , onceSetup' :: a -> RunConfig -> Action b
    , onceTearDown' :: b -> Action ()
    } ->
    Fixture C.Once a
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: RunConfig -> Action a
    } ->
    Fixture C.Thread a
  ThreadBefore' ::
    forall loc a b.
    (C.ThreadParam loc) =>
    { threadParent :: Fixture loc a
    , threadAction' :: a -> RunConfig -> Action b
    } ->
    Fixture C.Thread b
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
    Fixture C.Thread ()
  ThreadResource' ::
    forall loc a b.
    (C.ThreadParam loc) =>
    { threadResourceParent :: Fixture loc a
    , threadSetup' :: a -> RunConfig -> Action b
    , threadTearDown' :: b -> Action ()
    } ->
    Fixture C.Thread a
  -- each hooks
  EachBefore ::
    { eachAction :: RunConfig -> Action a
    } ->
    Fixture C.Each a
  EachBefore' ::
    forall loc a b.
    (C.EachParam loc) =>
    { eachParent :: Fixture loc a
    , eachAction' :: a -> RunConfig -> Action b
    } ->
    Fixture C.Each b
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
    Fixture C.Each ()
  EachResource' ::
    forall loc a b.
    (C.EachParam loc) =>
    { eachResourceParent :: Fixture loc a
    , eachSetup' :: a -> RunConfig -> Action b
    , eachTearDown' :: b -> Action ()
    } ->
    Fixture C.Each a
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
    (C.ItemClass i ds, C.EachParam loc) =>
    { parent :: Fixture loc a
    , config :: TestConfig
    , childAction :: a -> RunConfig -> i -> Action as
    , parse :: as -> Either C.ParseException ds
    , items :: RunConfig -> [i]
    } ->
    Test
  NoParse' ::
    forall i ds loc a.
    (C.ItemClass i ds, C.EachParam loc) =>
    { parent :: Fixture loc a
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
    (C.EachParam loc) =>
    { parent :: Fixture loc a
    , config :: TestConfig
    , childSingleAction :: a -> RunConfig -> Action as
    , checks :: C.Checks as
    } ->
    Test

data PreNode i where
  Before ::
    { title :: Text
    , cardinality :: Cardinality
    , action :: RunConfig -> Eff effs o
    , subNodes :: [AbstractPreNode o]
    } ->
    PreNode i
  Before' ::
    { title :: Text
    , cardinality :: Cardinality
    , childAction :: i -> RunConfig -> Eff effs o
    , subNodes :: [PreNode o]
    } ->
    PreNode i
  After ::
    { title :: Text
    , cardinality :: Cardinality
    , before :: PreNode i
    , after :: RunConfig -> Eff effs ()
    } ->
    PreNode i
  Resource ::
    { title :: Text
    , cardinality :: Cardinality
    , setUp :: RunConfig -> Eff effs a
    , tearDown :: a -> Eff effs ()
    } ->
    PreNode i


data PreNode i where
  Before ::
    { title :: Text
    , cardinality :: Cardinality
    , action :: RunConfig -> Eff effs o
    , subNodes :: [AbstractPreNode rc tc m o]
    } ->
    AbstractPreNode rc tc m i
  Before' ::
    { title :: Text
    , cardinality :: Cardinality
    , childAction :: i -> rc -> m o
    , subNodes :: [AbstractPreNode rc tc m o]
    } ->
    AbstractPreNode rc tc m i
  After ::
    { title :: Text
    , cardinality :: Cardinality
    , before :: AbstractPreNode rc tc m i
    , after :: rc -> m ()
    } ->
    AbstractPreNode rc tc m i
  Resource ::
    { title :: Text
    , cardinality :: Cardinality
    , setUp :: rc -> m a
    , tearDown :: a -> m ()
    } ->
    AbstractPreNode rc tc m i
  Test ::
    { config :: tc
    , items :: [AbstractTestItem rc tc m i]
    } ->
    AbstractPreNode rc tc m ()

data AbstractTestItem rc tc m i = TestItem
  { id :: Int
  , title :: Text
  , test :: rc -> i -> m ()
  , chkText :: Text
  }

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
