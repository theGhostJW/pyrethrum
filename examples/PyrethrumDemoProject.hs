module PyrethrumDemoProject where

import qualified Core as C
import DSL.FileSystemEffect (FSException, FileSystem)
import DSL.Internal.ApEvent (ApEvent)
import DSL.Out (Out)
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

data Fixture loc i o where
  -- once hooks
  OnceBefore ::
    { onceAction :: RunConfig -> Action o
    } ->
    Fixture C.Once () o
  OnceBefore' ::
    -- forall loc a b.
    (C.OnceParam loc) =>
    { onceParent :: Fixture loc pi i
    , onceAction' :: i -> RunConfig -> Action o
    } ->
    Fixture C.Once i o
  OnceAfter ::
    forall loc.
    (C.OnceAfterParam loc) =>
    { onceBefore :: Fixture loc () ()
    , onceAfter :: RunConfig -> Action ()
    } ->
    Fixture C.OnceAfter () ()
  OnceResource ::
    { onceSetup :: RunConfig -> Action o
    , onceTearDown :: o -> Action ()
    } ->
    Fixture C.Once () o
  OnceResource' ::
    -- forall loc a b.
    (C.OnceParam loc) =>
    { onceResourceParent :: Fixture loc pi i
    , onceSetup' :: i -> RunConfig -> Action o
    , onceTearDown' :: o -> Action ()
    } ->
    Fixture C.Once i o
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: RunConfig -> Action o
    } ->
    Fixture C.Thread () o
  ThreadBefore' ::
    -- forall loc a b.
    (C.ThreadParam loc) =>
    { threadParent :: Fixture loc pi i
    , threadAction' :: i -> RunConfig -> Action o
    } ->
    Fixture C.Thread i o
  ThreadAfter ::
    -- forall loc.
    -- TODO: check this should probably be test
    (C.ThreadAfterParam loc) =>
    { threadBefore :: Fixture loc pi ()
    , threadAfterAction :: RunConfig -> Action ()
    } ->
    Fixture C.ThreadAfter () ()
  ThreadResource ::
    { threadSetup :: RunConfig -> Action o
    , threadTearDown :: a -> Action ()
    } ->
    Fixture C.Thread () o
  ThreadResource' ::
    -- forall loc a b.
    (C.ThreadParam loc) =>
    { threadResourceParent :: Fixture loc pi i
    , threadSetup' :: i -> RunConfig -> Action o
    , threadTearDown' :: o -> Action ()
    } ->
    Fixture C.Thread i o
  -- each hooks
  EachBefore ::
    { eachAction :: RunConfig -> Action o
    } ->
    Fixture C.Each () o
  EachBefore' ::
    -- forall loc a b.
    (C.EachParam loc) =>
    { eachParent :: Fixture loc pi i
    , eachAction' :: i -> RunConfig -> Action o
    } ->
    Fixture C.Each i o
  EachAfter ::
    (C.EachAfterParam loc) =>
    { eachBefore :: Fixture loc () ()
    , eachAfterAction :: RunConfig -> Action ()
    } ->
    Fixture C.EachAfter () ()
  EachResource ::
    { eachSetup :: RunConfig -> Action o
    , eachTearDown :: o -> Action ()
    } ->
    Fixture C.Each () o
  EachResource' ::
    (C.EachParam loc) =>
    { eachResourceParent :: Fixture loc pi i
    , eachSetup' :: i -> RunConfig -> Action o
    , eachTearDown' :: o -> Action ()
    } ->
    Fixture C.Each i o
  -- Test ::
  --   { test :: Test i
  --   } ->
  --   Fixture C.Test i ()
  -- Test' ::
  --   { test' :: Test i
  --   } ->
  --   Fixture C.Test i ()

-- type TestFixture i = Fixture C.Test i ()
data Test hi where
  Full ::
    -- forall i as ds.
    (C.ItemClass i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action as
    , parse :: as -> Either C.ParseException ds
    , items :: RunConfig -> [i]
    } ->
    Test ()
  Full' ::
    -- forall i as ds loc a.
    (C.ItemClass i ds, C.EachParam loc) =>
    { parent :: Fixture loc pi a
    , config' :: TestConfig
    , childAction :: a -> RunConfig -> i -> Action as
    , parse' :: as -> Either C.ParseException ds
    , items' :: RunConfig -> [i]
    } ->
    Test a
  NoParse ::
    forall i ds.
    (C.ItemClass i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Action ds
    , items :: RunConfig -> [i]
    } ->
    Test ()
  NoParse' ::
    -- forall i ds loc a.
    (C.ItemClass i ds, C.EachParam loc) =>
    { parent :: Fixture loc pi a
    , config' :: TestConfig
    , childAction :: a -> RunConfig -> i -> Action ds
    , items' :: RunConfig -> [i]
    } ->
    Test a
  Single ::
    { config :: TestConfig
    , singleAction :: RunConfig -> Action as
    , checks :: C.Checks as
    } ->
    Test ()
  Single' ::
    (C.EachParam loc) =>
    { parent :: Fixture loc pi a
    , config' :: TestConfig
    , childSingleAction :: a -> RunConfig -> Action as
    , checks' :: C.Checks as
    } ->
    Test a

-- data PreNode i where
--   Before ::
--     { title :: Text
--     , cardinality :: Cardinality
--     , action :: RunConfig -> Eff effs o
--     , subNodes :: [AbstractPreNode o]
--     } ->
--     PreNode i
--   Before' ::
--     { title :: Text
--     , cardinality :: Cardinality
--     , childAction :: i -> RunConfig -> Eff effs o
--     , subNodes :: [PreNode o]
--     } ->
--     PreNode i
--   After ::
--     { title :: Text
--     , cardinality :: Cardinality
--     , before :: PreNode i
--     , after :: RunConfig -> Eff effs ()
--     } ->
--     PreNode i
--   Resource ::
--     { title :: Text
--     , cardinality :: Cardinality
--     , setUp :: RunConfig -> Eff effs a
--     , tearDown :: a -> Eff effs ()
--     } ->
--     PreNode i

-- data PreNode i where
--   Before ::
--     { title :: Text
--     , cardinality :: Cardinality
--     , action :: RunConfig -> Eff effs o
--     , subNodes :: [AbstractPreNode rc tc m o]
--     } ->
--     AbstractPreNode rc tc m i
--   Before' ::
--     { title :: Text
--     , cardinality :: Cardinality
--     , childAction :: i -> rc -> m o
--     , subNodes :: [AbstractPreNode rc tc m o]
--     } ->
--     AbstractPreNode rc tc m i
--   After ::
--     { title :: Text
--     , cardinality :: Cardinality
--     , before :: AbstractPreNode rc tc m i
--     , after :: rc -> m ()
--     } ->
--     AbstractPreNode rc tc m i
--   Resource ::
--     { title :: Text
--     , cardinality :: Cardinality
--     , setUp :: rc -> m a
--     , tearDown :: a -> m ()
--     } ->
--     AbstractPreNode rc tc m i
--   Test ::
--     { config :: tc
--     , items :: [AbstractTestItem rc tc m i]
--     } ->
--     AbstractPreNode rc tc m ()

-- data AbstractTestItem rc tc m i = TestItem
--   { id :: Int
--   , title :: Text
--   , test :: rc -> i -> m ()
--   , chkText :: Text
--   }

{-
data Suite i where
  Node ::
    { path :: C.Path
    , fixture :: Fixture loc pi i
    , subNodes :: [Suite i]
    } ->
    Suite i
  -}

mkAbstractTest :: Test hi -> C.AbstractTest RunConfig TestConfig ApEffs hi
mkAbstractTest = \case
  Full{..} -> C.Full{..}
  NoParse{..} -> C.NoParse{..}
  Full'{..} -> C.Full' (mkAbstractFx parent) config' childAction parse' items'
  NoParse'{..} -> C.NoParse' (mkAbstractFx parent) config' childAction items'
  Single{..} -> C.Single{..}
  Single'{..} -> C.Single' (mkAbstractFx parent) config' childSingleAction checks'

mkAbstractFx :: Fixture loc i o -> C.AbstractFixture RunConfig TestConfig ApEffs loc i o
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


