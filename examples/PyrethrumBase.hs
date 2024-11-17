module PyrethrumBase
  ( Action,
    Depth (..),
    Fixture (..),
    Hook (..),
    LogEffs,
    Node (..),
    RunConfig (..),
    Country (..),
    Environment (..),
    C.DataSource (..),
    Suite,
    SuiteRunner,
    FixtureConfig (..),
    fxCfg,
    HasLog,
    mkCoreSuite,
    ioRunner,
    docRunner,
    defaultRunConfig,
    docInterpreter
  )
where

import Core qualified as C
import DSL.FileSystemDocInterpreter qualified as FDoc (runFileSystem)
import DSL.FileSystemEffect (FileSystem)
import DSL.FileSystemIOInterpreter qualified as FIO (runFileSystem)
import DSL.Internal.NodeLog (NodeLog)
import DSL.Internal.NodeLog qualified as AE
import DSL.OutEffect (Out)
import DSL.OutInterpreter ( runOut )
import Effectful (Eff, IOE, runEff, type (:>))
import Filter (Filters)
import Internal.Logging qualified as L
import Internal.SuiteRuntime (ThreadCount, execute, executeWithoutValidation)
import PyrethrumConfigTypes as CG
  ( Country (..),
    Depth (..),
    Environment (..),
    FixtureConfig (..),
    RunConfig (..),
    defaultRunConfig,
    fxCfg,
  )
import WebDriverDocInterpreter qualified as WDDoc (runWebDriver)
import WebDriverEffect (WebUI (..))
import WebDriverIOInterpreter qualified as WDIO (runWebDriver)
import Prepare (prepare, PreNode)
import Internal.SuiteValidation (SuiteValidationError)
import Internal.SuiteFiltering (FilteredSuite(..))
import GHC.TypeLits qualified as TL

--  these will probably be split off and go into core or another library
-- module later
type Action = Eff ApEffs

type HasLog es = Out NodeLog :> es

type LogEffs a = forall es. (Out NodeLog :> es) => Eff es a

type ApEffs = '[FileSystem, WebUI, Out NodeLog, IOE]

-- type ApConstraints es = (FileSystem :> es, Out NodeLog :> es, Error FSException :> es, IOE :> es)
-- type AppEffs a = forall es. (FileSystem :> es, Out NodeLog :> es, Error FSException :> es, IOE :> es) => Eff es a

type SuiteRunner = Suite 
  -> Filters RunConfig FixtureConfig 
  -> RunConfig 
  -> ThreadCount 
  -> L.LogActions (L.Log L.ExePath AE.NodeLog)
  -> IO ()

ioInterpreter :: AE.LogSink -> Action a -> IO a
ioInterpreter sink ap =
  ap
    & FIO.runFileSystem
    & WDIO.runWebDriver
    & runOut sink
    & runEff


-- docRunner :: Suite -> Filters RunConfig FixtureConfig -> RunConfig -> ThreadCount -> L.MkLogActions (L.Event L.ExePath AE.NodeLog) (L.FLog L.ExePath AE.NodeLog) -> IO ()
-- docRunner suite filters runConfig threadCount logControls =
--   execute threadCount logControls $
--     C.MkSuiteExeParams
--       { interpreter = docInterpreter,
--         suite = mkCoreSuite suite,
--         filters,
--         runConfig
--       }

docRunner :: Bool -> Bool -> Suite -> Filters RunConfig FixtureConfig -> RunConfig -> ThreadCount -> L.LogActions (L.Log L.ExePath AE.NodeLog) -> IO ()
docRunner includeSteps includeChecks suite filters runConfig threadCount logControls =
  prepared & either 
    print
    (\s -> executeWithoutValidation threadCount logControls s.suite)
  where 
    prepared :: Either SuiteValidationError (FilteredSuite (PreNode IO ()))
    prepared = prepare $ C.MkSuiteExeParams
      { interpreter = docInterpreter,
        mode = C.Listing {includeSteps, includeChecks},
        suite = mkCoreSuite suite,
        filters,
        runConfig
      }
 


ioRunner :: Suite -> Filters RunConfig FixtureConfig -> RunConfig -> ThreadCount -> L.LogActions (L.Log L.ExePath AE.NodeLog) -> IO ()
ioRunner suite filters runConfig threadCount logControls =
  execute threadCount logControls $
    C.MkSuiteExeParams
      { interpreter = ioInterpreter,
        mode = C.Run,
        suite = mkCoreSuite suite,
        filters,
        runConfig
      }

docInterpreter :: AE.LogSink -> Action a  -> IO a
docInterpreter sink ap =
  ap
    & FDoc.runFileSystem
    & WDDoc.runWebDriver
    & runOut sink
    & runEff




{-
runErrorIO :: forall a e es. Exception e => Eff (Error e : es) a -> Eff es (Either (CallStack, SomeException) a)
runErrorIO effs = mapLeft upCastException <$> runError effs
  where
    upCastException :: (CallStack, e) -> (CallStack, SomeException)
    upCastException (cs, fsEx) = (cs, toException fsEx)

-- (\_cs (FSException e) -> throwIO e)

--- could be useful when we simplify the interpreter
-- runErrorIO ::  forall a es. (IOE :> es) => Eff (Error FSException : es) a -> Eff es a
-- runErrorIO = runErrorWith (\_cs (FSException e) -> throwIO e)

-- runOutIO & runWebDriverIO & runOutIO & runEff
-}
-- TODO: research StrictSTM

data Hook hz when input output where
  BeforeHook ::
    (C.Frequency hz) =>
    { action :: RunConfig -> Action o
    } ->
    Hook hz C.Before () o
  BeforeHook' ::
    (C.Frequency phz, C.Frequency hz, C.CanDependOn hz phz) =>
    { depends :: Hook phz pw pi i,
      action' :: RunConfig -> i -> Action o
    } ->
    Hook hz C.Before i o
  AfterHook ::
    (C.Frequency hz) =>
    { afterAction :: RunConfig -> Action ()
    } ->
    Hook hz C.After () ()
  AfterHook' ::
    (C.Frequency phz, C.Frequency hz, C.CanDependOn hz phz) =>
    { afterDepends :: Hook phz pw pi i,
      afterAction' :: RunConfig -> Action ()
    } ->
    Hook hz C.After i i
  AroundHook ::
    (C.Frequency hz) =>
    { setup :: RunConfig -> Action o,
      teardown :: RunConfig -> o -> Action ()
    } ->
    Hook hz C.Around () o
  AroundHook' ::
    (C.Frequency phz, C.Frequency hz, C.CanDependOn hz phz) =>
    { aroundDepends :: Hook phz pw pi i,
      setup' :: RunConfig -> i -> Action o,
      teardown' :: RunConfig -> o -> Action ()
    } ->
    Hook hz C.Around i o

data Fixture hi where
  Full ::
    (C.Item i ds, Show as) =>
    { config :: FixtureConfig,
      action :: RunConfig -> i -> Action as,
      parse :: as -> Either C.ParseException ds,
      items :: RunConfig -> C.DataSource i
    } ->
    Fixture ()
  Full' ::
    (C.Item i ds, Show as, C.Frequency hz) =>
    { config' :: FixtureConfig,
      depends :: Hook hz pw pi a,
      action' :: RunConfig -> a -> i -> Action as,
      parse' :: as -> Either C.ParseException ds,
      items' :: RunConfig -> C.DataSource i
    } ->
    Fixture a
  Direct ::
    forall i ds.
    (C.Item i ds) =>
    { config :: FixtureConfig,
      action :: RunConfig -> i -> Action ds,
      items :: RunConfig -> C.DataSource i
    } ->
    Fixture ()
  Direct' ::
    (C.Item i ds, C.Frequency hz) =>
    { config' :: FixtureConfig,
      depends :: Hook hz pw pi a,
      action' :: RunConfig -> a -> i -> Action ds,
      items' :: RunConfig -> C.DataSource i
    } ->
    Fixture a

{-
create IsFixture constraint
CHAPTER 12. CUSTOM TYPE ERRORS

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_errors.

gpt
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

data Foo = Foo | Bar

-- Define a type class that represents the constraint "IsFoo"
class IsFoo a where
    showFoo :: a -> Text

-- Provide instances for specific types (e.g., `Foo`)
instance IsFoo Foo where
    showFoo Foo = "Foo"
    showFoo Bar = "Bar"


-}
-- instance
--  ( TL.TypeError
--   ( TL.Text "Attempting to interpret a number as a function " TL.:$$: TL.Text "in the type `"
--     TL.:<>: TL.ShowType (a -> b)
--     TL.:<>: TL.Text "'"
--     TL.:$$: TL.Text "Did you forget to specify the function you wanted?"
--  )
--  ) => Fixture hi  where


type Suite = [Node ()]

data Node i where
  Hook ::
    (C.Frequency hz) =>
    { path :: AE.Path,
      hook :: Hook hz when i o,
      subNodes :: [Node o]
    } ->
    Node i
  Fixture ::
    { path :: AE.Path,
      fixture :: Fixture i
    } ->
    Node i

mkFixture :: Fixture hi -> C.Fixture Action RunConfig FixtureConfig hi
mkFixture = \case
  Full {..} -> C.Full {..}
  Direct {..} -> C.Direct {..}
  Full' {..} -> C.Full' config' (mkHook depends) action' parse' items'
  Direct' {..} -> C.Direct' {depends = mkHook depends, ..}

mkCoreSuite :: Suite -> [C.Node Action RunConfig FixtureConfig ()]
mkCoreSuite tr = mkNode <$> tr

mkHook :: Hook hz pw i o -> C.Hook (Eff ApEffs) RunConfig hz i o
mkHook = \case
  BeforeHook {..} -> C.Before {..}
  BeforeHook' {..} -> C.Before' (mkHook depends) action'
  AfterHook {..} -> C.After {..}
  AfterHook' {..} -> C.After' {afterDepends = mkHook afterDepends, ..}
  AroundHook {..} -> C.Around {..}
  AroundHook'
    { aroundDepends,
      setup',
      teardown'
    } ->
      C.Around' (mkHook aroundDepends) setup' teardown'

mkNode :: Node i -> C.Node Action RunConfig FixtureConfig i
mkNode = \case
  Hook {..} ->
    C.Hook
      { hook = mkHook hook,
        subNodes = mkNode <$> subNodes,
        ..
      }
  Fixture {..} -> C.Fixture {fixture = mkFixture fixture, ..}
