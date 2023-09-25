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

-- class OnceParam2 a
-- class ThreadParam2 a
-- class EachParam2 a
-- class Param2 a
-- class (Param2 a, Param2 b) => ValidParent2 a b

-- data Once2
-- instance OnceParam2 Once2
-- instance ThreadParam2 Once2
-- instance EachParam2 Once2
-- instance Param2 Once2

-- data Thread2
-- instance ThreadParam2 Thread2
-- instance EachParam2 Thread2
-- instance Param2 Thread2

-- data Each2
-- instance EachParam2 Each2
-- instance Param2 EachParam2
-- instance Param2 Each2

-- instance ValidParent2 Once2 Once2
-- instance ValidParent2 Once2 Thread2
-- instance ValidParent2 Once2 Each2

-- instance ValidParent2 Thread2 Thread2
-- instance ValidParent2 Thread2 Each2

-- instance ValidParent2 Each2 Each2

-- data Hook2 loc i o where
--   Before2 ::
--     (Param2 loc) =>
--     { onceAction :: RunConfig -> Action o
--     } ->
--     Hook2 loc () o
--   Before2' ::
--     -- forall loc a b.
--     (ValidParent2 ploc loc) =>
--     { onceParent :: Hook2 ploc pi i
--     , onceAction' :: i -> RunConfig -> Action o
--     } ->
--     Hook2 loc i o

data Hook loc i o where
  Before ::
    (C.Param loc) =>
    { action :: RunConfig -> Action o
    } ->
    Hook loc () o
  Before' ::
    (C.Param ploc, C.Param loc, C.ValidParent ploc loc) =>
    { parent :: Hook ploc pi i
    , action' :: RunConfig -> i -> Action o
    } ->
    Hook loc i o
  After ::
    (C.Param loc) =>
    { afterAction :: RunConfig -> Action ()
    } ->
    Hook loc () ()
  After' ::
    (C.Param ploc, C.Param loc, C.ValidParent ploc loc) =>
    { afterParent :: Hook ploc pi i
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
    (C.Param ploc, C.Param loc, C.ValidParent ploc loc) =>
    { parent :: Hook ploc pi i
    , setup' :: RunConfig -> i -> Action o
    , teardown' :: RunConfig -> o -> Action ()
    } ->
    Hook loc i o

-- TODO: split datatypes with conversion typeclasses
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
    (C.ItemClass i ds, C.Param loc) =>
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
    (C.ItemClass i ds, C.Param loc) =>
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
    (C.Param loc) =>
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
  NoParse'{..} -> C.NoParse'{parent = mkHook parent, ..}
  Single{..} -> C.Single{..}
  Single'{..} -> C.Single' (mkHook parent) config' singleAction' checks'

mkHook :: Hook loc i o -> C.Hook RunConfig ApEffs loc i o
mkHook = \case
  Before{..} -> C.Before{..}
  Before'{..} -> C.Before' (mkHook parent) action'
  After{..} -> C.After{..}
  After'{..} -> C.After'{afterParent = mkHook afterParent, ..}
  Around{..} -> C.Around{..}
  Around'
    { parent
    , setup'
    , teardown'
    } ->
      C.Around' (mkHook parent) setup' teardown'

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
