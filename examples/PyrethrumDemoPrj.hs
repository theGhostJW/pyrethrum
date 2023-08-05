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
type Suite a = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] a
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
    { onceAction :: RunConfig -> Suite a
    } ->
    Fixture C.OnceBefore a
  OnceBefore' ::
    { onceParent :: Fixture C.OnceBefore a
    , onceChildAction :: RunConfig -> a -> Suite b
    } ->
    Fixture C.OnceBefore b
  OnceAfter ::
    (C.AfterTest loc) =>
    { onceBefore :: Fixture loc ()
    , onceAfterAction :: RunConfig -> Suite ()
    } ->
    Fixture C.OnceAfter ()
  OnceResource ::
    { onceSetup :: RunConfig -> Suite a
    , onceTearDown :: a -> Suite ()
    } ->
    Fixture C.OnceBefore ()
  OnceResource' ::
    { onceResourceParent :: Fixture C.OnceBefore a
    , onceChildSetup :: a -> RunConfig -> Suite b
    , onceChildTearDown :: b -> Suite ()
    } ->
    Fixture C.OnceBefore a
  -- once per thread hooks
  ThreadBefore ::
    { action :: RunConfig -> Suite a
    } ->
    Fixture C.ThreadBefore a
  ThreadBefore' ::
    (C.ThreadParam loc, C.BeforeTest loc) =>
    { parent :: Fixture loc a
    , childAction :: RunConfig -> a -> Suite b
    } ->
    Fixture C.ThreadBefore b
  ThreadAfter ::
    (C.AfterTest loc) =>
    { threadBefore :: Fixture loc ()
    , threadAfterAction :: RunConfig -> Suite ()
    } ->
    Fixture C.ThreadAfter ()
  ThreadResource ::
    { threadSetup :: RunConfig -> Suite a
    , threadTearDown :: a -> Suite ()
    } ->
    Fixture C.ThreadBefore ()
  ThreadResource' ::
    { threadResourceParent :: Fixture C.OnceBefore a
    , threadChildSetup :: a -> RunConfig -> Suite b
    , threadChildTearDown :: b -> Suite ()
    } ->
    Fixture C.ThreadBefore a
  -- each hooks
  EachBefore ::
    { eachAction :: RunConfig -> Suite a
    } ->
    Fixture C.EachBefore a
  EachBefore' ::
    { eachParent :: (C.BeforeTest loc) => Fixture loc a
    , eachChildAction :: RunConfig -> a -> Suite b
    } ->
    Fixture C.EachBefore b
  EachAfter ::
    { eachBefore :: (C.AfterTest loc) => Fixture loc ()
    , eachAfterAction :: RunConfig -> Suite ()
    } ->
    Fixture C.EachAfter ()
  EachResource ::
    { eachSetup :: RunConfig -> Suite a
    , eachTearDown :: a -> Suite ()
    } ->
    Fixture C.EachBefore ()
  EachResource' ::
    { eachResourceParent :: (C.BeforeTest loc) => Fixture loc a
    , eachChildSetup :: a -> RunConfig -> Suite b
    , eachChildTearDown :: b -> Suite ()
    } ->
    Fixture C.EachBefore a
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
    , action :: RunConfig -> i -> Suite as
    , parse :: as -> Either C.ParseException ds
    , items :: RunConfig -> [i]
    } ->
    Test
  NoParse ::
    forall i ds.
    (C.ItemClass i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Suite ds
    , items :: RunConfig -> [i]
    } ->
    Test
  Full' ::
    forall i as ds loc a.
    (C.ItemClass i ds) =>
    { parentHook :: (C.BeforeTest loc) => Fixture loc a
    , config :: TestConfig
    , childAction :: a -> RunConfig -> i -> Suite as
    , parse :: as -> Either C.ParseException ds
    , items :: RunConfig -> [i]
    } ->
    Test
  NoParse' ::
    forall i ds loc a.
    (C.ItemClass i ds) =>
    { parentHook :: (C.BeforeTest loc) => Fixture loc a
    , config :: TestConfig
    , childAction :: a -> RunConfig -> i -> Suite ds
    , items :: RunConfig -> [i]
    } ->
    Test

mkAbstractTest :: Test -> C.AbstractTest RunConfig TestConfig ApEffs
mkAbstractTest = \case
  Full{..} -> C.Full{..}
  NoParse{..} -> C.NoParse{..}
  Full'{..} -> C.Full' (mkAbstractFx parentHook) config childAction parse items
  NoParse'{..} -> C.NoParse' (mkAbstractFx parentHook) config childAction items

mkAbstractFx :: Fixture loc a -> C.AbstractFixture RunConfig TestConfig ApEffs loc a
mkAbstractFx = \case
  OnceBefore{..} -> C.OnceBefore{..}
  OnceBefore'{..} -> C.OnceBefore' (mkAbstractFx onceParent) onceChildAction
  OnceAfter{..} -> C.OnceAfter (mkAbstractFx onceBefore) onceAfterAction
  OnceResource'
    { onceResourceParent
    , onceChildSetup
    , onceChildTearDown
    } ->
      C.OnceResource' (mkAbstractFx onceResourceParent) onceChildSetup onceChildTearDown
  OnceResource{..} -> C.OnceResource{..}
  ThreadBefore{..} -> C.ThreadBefore{..}
  ThreadBefore'{..} -> C.ThreadBefore' (mkAbstractFx parent) childAction
  ThreadAfter{..} -> C.ThreadAfter (mkAbstractFx threadBefore) threadAfterAction
  ThreadResource{..} -> C.ThreadResource{..}
  ThreadResource'{threadResourceParent = p, ..} -> C.ThreadResource' (mkAbstractFx p) threadChildSetup threadChildTearDown
  EachBefore{..} -> C.EachBefore {..}
  EachBefore'{eachParent, eachChildAction} -> C.EachBefore' (mkAbstractFx eachParent) eachChildAction
  EachAfter{..} -> C.EachAfter (mkAbstractFx eachBefore) eachAfterAction
  EachResource{..} -> C.EachResource {..}
  EachResource'{eachResourceParent, eachChildSetup, eachChildTearDown} -> C.EachResource' (mkAbstractFx eachResourceParent) eachChildSetup eachChildTearDown
  Test{test} -> C.Test $ mkAbstractTest test

  
