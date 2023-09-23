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


class OnceParam2 a
class ThreadParam2 a
class EachParam2 a
class Param2 a
class (Param2 a, Param2 b) => ValidParent2 a b

data Once2
instance OnceParam2 Once2
instance ThreadParam2 Once2
instance EachParam2 Once2
instance Param2 Once2

data Thread2
instance ThreadParam2 Thread2
instance EachParam2 Thread2
instance Param2 Thread2

data Each2
instance EachParam2 Each2
instance Param2 EachParam2
instance Param2 Each2

instance ValidParent2 Once2 Once2
instance ValidParent2 Once2 Thread2
instance ValidParent2 Once2 Each2

instance ValidParent2 Thread2 Thread2
instance ValidParent2 Thread2 Each2

instance ValidParent2 Each2 Each2

data Hook2 loc i o where
  Before2 ::
    (Param2 loc) =>
    { onceAction :: RunConfig -> Action o
    } ->
    Hook2 loc () o
  Before2' ::
    -- forall loc a b.
    (ValidParent2 ploc loc) =>
    { onceParent :: Hook2 ploc pi i
    , onceAction' :: i -> RunConfig -> Action o
    } ->
    Hook2 loc i o


data Hook loc i o where
  -- once hooks
  OnceBefore ::
    { onceAction :: RunConfig -> Action o
    } ->
    Hook C.Once () o
  ThreadBefore ::
    { threadAction :: RunConfig -> Action o
    } ->
    Hook C.Thread () o
  EachBefore ::
    { eachAction :: RunConfig -> Action o
    } ->
    Hook C.Each () o
  OnceBefore' ::
    -- forall loc a b.
    (C.OnceParam loc) =>
    { onceParent :: Hook loc pi i
    , onceAction' :: i -> RunConfig -> Action o
    } ->
    Hook C.Once i o
  ThreadBefore' ::
  -- once per thread hooks
    -- forall loc a b.
    (C.ThreadParam loc) =>
    { threadParent :: Hook loc pi i
    , threadAction' :: i -> RunConfig -> Action o
    } ->
    Hook C.Thread i o
  OnceAfter ::
    { onceAfter :: RunConfig -> Action ()
    } ->
    Hook C.Once () ()
  OnceAfter' ::
    (C.OnceParam loc) =>
    { onceAfterParent :: Hook loc pi i
    , onceAfter' :: RunConfig -> Action ()
    } ->
    Hook C.Once i i
  OnceAround ::
    { onceSetup :: RunConfig -> Action o
    , onceTearDown :: o -> Action ()
    } ->
    Hook C.Once () o
  OnceAround' ::
    -- forall loc a b.
    (C.OnceParam loc) =>
    { onceAroundParent :: Hook loc pi i
    , onceSetup' :: i -> RunConfig -> Action o
    , onceTearDown' :: o -> Action ()
    } ->
    Hook C.Once i o
  ThreadAfter ::
    { threadAfter :: RunConfig -> Action ()
    } ->
    Hook C.Thread  () ()
  ThreadAfter' ::
    (C.ThreadParam loc) =>
    { threadAfterParent :: Hook loc pi i
    , threadAfter' :: RunConfig -> Action ()
    } ->
    Hook C.Thread  i i
  ThreadAround ::
    { threadSetup :: RunConfig -> Action o
    , threadTearDown :: a -> Action ()
    } ->
    Hook C.Thread  () o
  ThreadAround' ::
    (C.ThreadParam loc) =>
    { threadAroundParent :: Hook loc pi i
    , threadSetup' :: i -> RunConfig -> Action o
    , threadTearDown' :: o -> Action ()
    } ->
    Hook C.Thread  i o
  -- each hooks
  EachBefore' ::
    -- forall loc a b.
    (C.EachParam loc) =>
    { eachParent :: Hook loc pi i
    , eachAction' :: i -> RunConfig -> Action o
    } ->
    Hook C.Each i o
  EachAfter ::
    { eachAfter :: RunConfig -> Action ()
    } ->
    Hook C.Each () ()
  EachAfter' ::
    (C.EachParam loc) =>
    { eachAfterParent :: Hook loc pi i
    , eachAfter' :: RunConfig -> Action ()
    } ->
    Hook C.Each i i
  EachAround ::
    { eachSetup :: RunConfig -> Action o
    , eachTearDown :: o -> Action ()
    } ->
    Hook C.Each () o
  EachAround' ::
    (C.EachParam loc) =>
    { eachAroundParent :: Hook loc pi i
    , eachSetup' :: i -> RunConfig -> Action o
    , eachTearDown' :: o -> Action ()
    } ->
    Hook C.Each i o

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
    , action' :: a -> RunConfig -> i -> Action as
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
    , action' :: a -> RunConfig -> i -> Action ds
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
    , singleAction' :: a -> RunConfig -> Action as
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
  Full'{..} -> C.Full' (mkHook parent) config' action' parse' items'
  NoParse'{..} -> C.NoParse' {parent = mkHook parent, .. }
  Single{..} -> C.Single{..}
  Single'{..} -> C.Single' (mkHook parent) config' singleAction' checks'

mkHook :: Hook loc i o -> C.Hook RunConfig TestConfig ApEffs loc i o
mkHook = \case
  OnceBefore{..} -> C.OnceBefore{..}
  OnceBefore'{..} -> C.OnceBefore' (mkHook onceParent) onceAction'
  OnceAfter{..} -> C.OnceAfter{..}
  OnceAfter'{..} -> C.OnceAfter'{onceAfterParent = mkHook  onceAfterParent,..}
  ThreadAfter'{..} -> C.ThreadAfter' {threadAfterParent = mkHook threadAfterParent,..}
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
  ThreadAfter{..} -> C.ThreadAfter {..}
  ThreadAround{..} -> C.ThreadAround {..}
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
