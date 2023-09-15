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

-- type Hook a = Hook RunConfig TestConfig ApEffs a

data Hook loc i o where
  -- once hooks
  OnceBefore ::
    { onceAction :: RunConfig -> Action o
    } ->
    Hook C.OnceBefore () o
  OnceBefore' ::
    -- forall loc a b.
    (C.OnceParam loc) =>
    { onceParent :: Hook loc pi i
    , onceAction' :: i -> RunConfig -> Action o
    } ->
    Hook C.OnceBefore i o
  OnceAfter ::
    { onceAfter :: RunConfig -> Action ()
    } ->
    Hook C.OnceAfter () ()
  OnceAfter' ::
    (C.OnceAfterParam loc) =>
    { onceAfterParent :: Hook loc pi i
    , onceAfter' :: RunConfig -> Action ()
    } ->
    Hook C.OnceAfter i i
  OnceAround ::
    { onceSetup :: RunConfig -> Action o
    , onceTearDown :: o -> Action ()
    } ->
    Hook C.OnceAround () o
  OnceAround' ::
    -- forall loc a b.
    (C.OnceParam loc) =>
    { onceAroundParent :: Hook loc pi i
    , onceSetup' :: i -> RunConfig -> Action o
    , onceTearDown' :: o -> Action ()
    } ->
    Hook C.OnceAround i o
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: RunConfig -> Action o
    } ->
    Hook C.ThreadBefore () o
  ThreadBefore' ::
    -- forall loc a b.
    (C.ThreadParam loc) =>
    { threadParent :: Hook loc pi i
    , threadAction' :: i -> RunConfig -> Action o
    } ->
    Hook C.ThreadBefore i o
  ThreadAfter ::
    { threadAfter :: RunConfig -> Action ()
    } ->
    Hook C.ThreadAfter () ()
  ThreadAfter' ::
    (C.ThreadAfterParam loc) =>
    { threadAfterParent :: Hook loc pi i
    , threadAfter' :: RunConfig -> Action ()
    } ->
    Hook C.ThreadAfter i i
  ThreadAround ::
    { threadSetup :: RunConfig -> Action o
    , threadTearDown :: a -> Action ()
    } ->
    Hook C.ThreadAround () o
  ThreadAround' ::
    (C.ThreadParam loc) =>
    { threadAroundParent :: Hook loc pi i
    , threadSetup' :: i -> RunConfig -> Action o
    , threadTearDown' :: o -> Action ()
    } ->
    Hook C.ThreadAround i o
  -- each hooks
  EachBefore ::
    { eachAction :: RunConfig -> Action o
    } ->
    Hook C.EachBefore () o
  EachBefore' ::
    -- forall loc a b.
    (C.EachParam loc) =>
    { eachParent :: Hook loc pi i
    , eachAction' :: i -> RunConfig -> Action o
    } ->
    Hook C.EachBefore i o
  EachAfter ::
    { eachAfter :: RunConfig -> Action ()
    } ->
    Hook C.EachAfter () ()
  EachAfter' ::
    (C.EachAfterParam loc) =>
    { eachAfterParent :: Hook loc pi i
    , eachAfter' :: RunConfig -> Action ()
    } ->
    Hook C.EachAfter i i
  EachAround ::
    { eachSetup :: RunConfig -> Action o
    , eachTearDown :: o -> Action ()
    } ->
    Hook C.EachAround () o
  EachAround' ::
    (C.EachParam loc) =>
    { eachAroundParent :: Hook loc pi i
    , eachSetup' :: i -> RunConfig -> Action o
    , eachTearDown' :: o -> Action ()
    } ->
    Hook C.EachAround i o

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
    { parent :: Hook loc pi a
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
    { parent :: Hook loc pi a
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
    { parent :: Hook loc pi a
    , config' :: TestConfig
    , childSingleAction :: a -> RunConfig -> Action as
    , checks' :: C.Checks as
    } ->
    Test a

type Suite = [SuiteElement ()]
data SuiteElement i where
  Hook ::
    { path :: C.Path
    , hook :: Hook loc i o
    , subNodes :: [SuiteElement o]
    } ->
    SuiteElement i
  Test ::
    { path :: C.Path
    , test :: Test i
    } ->
    SuiteElement i

mkTest :: Test hi -> C.Test RunConfig TestConfig ApEffs hi
mkTest = \case
  Full{..} -> C.Full{..}
  NoParse{..} -> C.NoParse{..}
  Full'{..} -> C.Full' (mkHook parent) config' childAction parse' items'
  NoParse'{..} -> C.NoParse' (mkHook parent) config' childAction items'
  Single{..} -> C.Single{..}
  Single'{..} -> C.Single' (mkHook parent) config' childSingleAction checks'

mkHook :: Hook loc i o -> C.Hook RunConfig TestConfig ApEffs loc i o
mkHook = \case
  OnceBefore{..} -> C.OnceBefore{..}
  OnceBefore'{..} -> C.OnceBefore' (mkHook onceParent) onceAction'
  OnceAfter{..} -> C.OnceAfter{..}
  OnceAfter'{..} -> C.OnceAfter'{onceAfterParent = mkHook  onceAfterParent,..}
  ThreadAfter'{..} -> C.ThreadAfter'{threadAfterParent = mkHook threadAfterParent,..}
  EachAfter'{..} -> C.EachAfter'{eachAfterParent = mkHook eachAfterParent,..}
  OnceAround'
    { onceAroundParent
    , onceSetup'
    , onceTearDown'
    } ->
      C.OnceAround' (mkHook onceAroundParent) onceSetup' onceTearDown'
  OnceAround{..} -> C.OnceAround{..}
  ThreadBefore{..} -> C.ThreadBefore{..}
  ThreadBefore'{..} -> C.ThreadBefore' (mkHook threadParent) threadAction'
  ThreadAfter{..} -> C.ThreadAfter{..}
  ThreadAround{..} -> C.ThreadAround{..}
  ThreadAround'{threadAroundParent = p, ..} -> C.ThreadAround' (mkHook p) threadSetup' threadTearDown'
  EachBefore{..} -> C.EachBefore{..}
  EachBefore'{eachParent, eachAction'} -> C.EachBefore' (mkHook eachParent) eachAction'
  EachAfter{..} -> C.EachAfter{..}
  EachAround{..} -> C.EachAround{..}
  EachAround'{eachAroundParent, eachSetup', eachTearDown'} -> C.EachAround' (mkHook eachAroundParent) eachSetup' eachTearDown'

mkSuite :: SuiteElement i -> C.SuiteElement RunConfig TestConfig ApEffs i
mkSuite = \case
  Hook{..} ->
    C.Hook
      { hook = mkHook hook
      , subNodes = mkSuite <$> subNodes
      , ..
      }
  Test{..} -> C.Test{test = mkTest test, ..}

mkTestRun :: Suite -> C.Suite RunConfig TestConfig ApEffs
mkTestRun tr = mkSuite <$> tr
