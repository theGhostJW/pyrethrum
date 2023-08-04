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
type Suite es = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] es
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
  OnceBefore ::
    { onceAction :: RunConfig -> Suite a
    } ->
    Fixture C.OnceBefore a
  ChildOnceBefore ::
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
  ChildOnceResource ::
    { onceResourceParent :: Fixture C.OnceBefore a
    , onceChildSetup :: a -> RunConfig -> Suite b
    , onceChildTearDown :: b -> Suite ()
    } ->
    Fixture C.OnceBefore a
  OnceResource ::
    { onceSetup :: RunConfig -> Suite a
    , onceTearDown :: a -> Suite ()
    } ->
    Fixture C.OnceBefore ()
  ThreadBefore ::
    { action :: RunConfig -> Suite a
    } ->
    Fixture C.ThreadBefore a
  ChildThreadBefore ::
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
  ChildThreadResource ::
    { threadResourceParent :: Fixture C.OnceBefore a
    , threadChildSetup :: a -> RunConfig -> Suite b
    , threadChildTearDown :: b -> Suite ()
    } ->
    Fixture C.ThreadBefore a
  Test ::
    { test :: Test
    } ->
    Fixture C.Test ()
  ChildTest ::
    (C.ThreadParam loc, C.BeforeTest loc) =>
    { parentHook :: Fixture loc a
    , childTest :: a -> Test
    } ->
    Fixture C.Test ()

data Test where
  Full ::
    (C.ItemClass i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Suite as
    , parse :: as -> Either C.ParseException ds
    , items :: RunConfig -> [i]
    } ->
    Test
  NoParse ::
    (C.ItemClass i ds) =>
    { config :: TestConfig
    , action :: RunConfig -> i -> Suite ds
    , items :: RunConfig -> [i]
    } ->
    Test

-- TODO Singleton

mkAbstractTest :: Test -> C.AbstractTest RunConfig TestConfig ApEffs
mkAbstractTest = \case
  Full{..} -> C.Full{..}
  NoParse{..} -> C.NoParse{..}

mkAbstractFx :: Fixture loc a -> C.AbstractFixture RunConfig TestConfig ApEffs loc a
mkAbstractFx = \case
  OnceBefore{..} -> C.OnceBefore{..}
  ChildOnceBefore{..} -> C.ChildOnceBefore (mkAbstractFx onceParent) onceChildAction
  OnceAfter{..} -> C.OnceAfter (mkAbstractFx onceBefore) onceAfterAction
  ChildOnceResource
    { onceResourceParent
    , onceChildSetup
    , onceChildTearDown
    } ->
      C.ChildOnceResource (mkAbstractFx onceResourceParent) onceChildSetup onceChildTearDown
  OnceResource{..} -> C.OnceResource{..}
  ThreadBefore{..} -> C.ThreadBefore{..}
  ChildThreadBefore{..} -> C.ChildThreadBefore (mkAbstractFx parent) childAction
  ThreadAfter{..} -> C.ThreadAfter (mkAbstractFx threadBefore) threadAfterAction
  ThreadResource{..} -> C.ThreadResource{..}
  ChildThreadResource{threadResourceParent = p, ..} -> C.ChildThreadResource (mkAbstractFx p) threadChildSetup threadChildTearDown
  Test{test} -> C.Test $ mkAbstractTest test
  ChildTest{..} -> C.ChildTest (mkAbstractFx parentHook) (mkAbstractTest . childTest)