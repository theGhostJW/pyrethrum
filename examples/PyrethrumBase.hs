module PyrethrumBase
  ( Action,
    Depth (..),
    -- export fixture constructors for demo purposes (rmove later)
    Fixture(..),
    Hook (..),
    LogEffs,
    Node (..),
    RunConfig (..),
    Country (..),
    Environment (..),
    Suite,
    SuiteRunner,
    FixtureConfig (..),
    environment,
    fxCfg,
    HasLog,
    mkCoreSuite,
    ioRunner,
    docRunner,
    defaultRunConfig,
    docInterpreter,
    mkDirect,
    -- mkFullDemoErrMsgs,
    mkDirect',
    mkFull,
    mkFull',
    runConfig
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
import Effectful.Labeled.Reader as LR (ask, Reader, runReader, asks)

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
import CoreTypeFamilies (HasTestFields, FixtureTypeCheckFull, FixtureTypeCheckDirect, DataSource, ValStateType)
import Effectful.Labeled
-- import CoreTypeFamilies (DataSourceMatchesAction, DataSourceType, ActionInputType, ActionInputType')

--  these will probably be split off and go into core or another library
-- module later
type Action = Eff ApEffs

type HasLog es = Out NodeLog :> es

type LogEffs a = forall es. (Out NodeLog :> es) => Eff es a

type ApEffs = '[RunConfigReader, FileSystem, WebUI, Out NodeLog, IOE]
-- type ApEffs = '[FileSystem, WebUI, Out NodeLog, IOE]

-- Define a labeled Reader effect for RunConfig
type RunConfigReader = Labeled "runConfig" (LR.Reader RunConfig) 

-- type ApConstraints es = (FileSystem :> es, Out NodeLog :> es, Error FSException :> es, IOE :> es)
-- type AppEffs a = forall es. (FileSystem :> es, Out NodeLog :> es, Error FSException :> es, IOE :> es) => Eff es a

type SuiteRunner = Suite 
  -> Filters RunConfig FixtureConfig 
  -> RunConfig 
  -> ThreadCount 
  -> L.LogActions (L.Log L.ExePath AE.NodeLog)
  -> IO ()

ioInterpreter :: RunConfig -> AE.LogSink -> Action a -> IO a
ioInterpreter rc sink ap =
  ap
    & LR.runReader @"runConfig" rc
    & FIO.runFileSystem
    & WDIO.runWebDriver
    & runOut sink
    & runEff

runConfig :: RunConfigReader :> es => Eff es RunConfig
runConfig = LR.ask @"runConfig" 

environment :: RunConfigReader :> es => Eff es Environment
environment = LR.asks @"runConfig" (.environment)

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
docRunner includeSteps includeChecks suite filters runConfig' threadCount logControls =
  prepared & either 
    print
    (\s -> executeWithoutValidation threadCount logControls s.suite)
  where 
    prepared :: Either SuiteValidationError (FilteredSuite (PreNode IO ()))
    prepared = prepare $ C.MkSuiteExeParams
      { interpreter = docInterpreter runConfig',
        mode = C.Listing {includeSteps, includeChecks},
        suite = mkCoreSuite suite,
        filters,
        runConfig = runConfig'
      }
 
ioRunner :: Suite -> Filters RunConfig FixtureConfig -> RunConfig -> ThreadCount -> L.LogActions (L.Log L.ExePath AE.NodeLog) -> IO ()
ioRunner suite filters runConfig' threadCount logControls =
  execute threadCount logControls $
    C.MkSuiteExeParams
      { interpreter = ioInterpreter runConfig',
        mode = C.Run,
        suite = mkCoreSuite suite,
        filters,
        runConfig = runConfig'
      }

docInterpreter :: RunConfig -> AE.LogSink -> Action a  -> IO a
docInterpreter rc sink ap =
  ap
    & LR.runReader @"runConfig" rc
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
    { action :: Action o
    } ->
    Hook hz C.Before () o
  BeforeHook' ::
    (C.Frequency phz, C.Frequency hz, C.CanDependOn hz phz) =>
    { depends :: Hook phz pw pi i,
      action' :: i -> Action o
    } ->
    Hook hz C.Before i o
  AfterHook ::
    (C.Frequency hz) =>
    { afterAction :: Action ()
    } ->
    Hook hz C.After () ()
  AfterHook' ::
    (C.Frequency phz, C.Frequency hz, C.CanDependOn hz phz) =>
    { afterDepends :: Hook phz pw pi i,
      afterAction' :: Action ()
    } ->
    Hook hz C.After i i
  AroundHook ::
    (C.Frequency hz) =>
    { setup :: Action o,
      teardown :: o -> Action ()
    } ->
    Hook hz C.Around () o
  AroundHook' ::
    (C.Frequency phz, C.Frequency hz, C.CanDependOn hz phz) =>
    { aroundDepends :: Hook phz pw pi i,
      setup' :: i -> Action o,
      teardown' :: o -> Action ()
    } ->
    Hook hz C.Around i o

data Fixture hi where
  Full ::
     forall i vs as action dataSource parser. 
    (
     dataSource ~ (RunConfig -> DataSource i vs),
     action ~ (i -> Action as),
     parser ~ (as -> Either C.ParseException vs),
     FixtureTypeCheckFull action parser dataSource (ValStateType dataSource),
     Show as,
     HasTestFields i vs
    ) =>
    { config :: FixtureConfig,
      action :: action,
      parse :: parser,
      dataSource :: dataSource
    } ->
    Fixture ()
  Full' ::
      forall hz pw pi a i vs as action dataSource parser. 
    (
     dataSource ~ (RunConfig -> DataSource i vs),
     action ~ (a -> i -> Action as),
     parser ~ (as -> Either C.ParseException vs),
     FixtureTypeCheckFull action parser dataSource (ValStateType dataSource),
     Show as,
     HasTestFields i vs, 
     C.Frequency hz
    ) =>
    { config' :: FixtureConfig,
      depends :: Hook hz pw pi a,
      action' :: action,
      parse' :: parser,
      dataSource' :: dataSource
    } ->
    Fixture a
  Direct ::
    forall i vs action dataSource. 
    (
     dataSource ~ (RunConfig -> DataSource i vs),
     action ~ (i -> Action vs),
     FixtureTypeCheckDirect action dataSource,
     HasTestFields i vs
     ) =>
    { config :: FixtureConfig,
      action :: action,
      dataSource :: dataSource
    } ->
    Fixture ()
  Direct' ::
    forall i hz pw pi a vs action' dataSource'. 
    (
     dataSource' ~ (RunConfig -> DataSource i vs),
     action' ~ (a -> i -> Action vs),
     FixtureTypeCheckDirect action' dataSource',
     HasTestFields i vs, 
     C.Frequency hz
     ) =>
    { config' :: FixtureConfig,
      depends :: Hook hz pw pi a,
      action' :: action',
      dataSource' :: dataSource'
    } ->
    Fixture a


mkFull :: (
 dataSource ~ (RunConfig -> DataSource i vs),
 action ~ (i -> Action as),
 parser ~ (as -> Either C.ParseException vs),
 FixtureTypeCheckFull action parser dataSource (ValStateType dataSource),
 HasTestFields i vs, 
 Show as
 ) =>
 FixtureConfig 
 -> action
 -> parser
 -> dataSource
 -> Fixture ()
mkFull config action parse dataSource = Full {..}

mkFull' :: (
 dataSource ~ (RunConfig -> DataSource i vs),
 action ~ (ho -> i -> Action as),
 parser ~ (as -> Either C.ParseException vs),
 HasTestFields i vs, 
 FixtureTypeCheckFull action parser dataSource (ValStateType dataSource),
 Show as, 
 C.Frequency hz
 ) =>
 FixtureConfig 
 -> Hook hz pw pi ho
 -> action
 -> parser
 -> dataSource
 -> Fixture ho
mkFull' config' depends action' parse' dataSource' = Full' {..}

mkDirect :: (
 dataSource ~ (RunConfig -> DataSource i vs),
 action ~ (i -> Action vs),
 FixtureTypeCheckDirect action dataSource,
 HasTestFields i vs
 ) =>
 FixtureConfig 
 -> action
 -> dataSource
 -> Fixture ()
mkDirect config action dataSource = Direct {..}

mkDirect'  :: (
 dataSource ~ (RunConfig -> DataSource i vs),
 action ~ (ho -> i -> Action vs),
 FixtureTypeCheckDirect action dataSource,
 HasTestFields i vs, 
 C.Frequency hz
 ) =>
 FixtureConfig 
 -> Hook hz pw pi ho
 -> action
 -> dataSource
 -> Fixture ho
mkDirect' config' depends action' dataSource' = Direct' {..}



-- Type synonyms for readability
-- type MkAction i as = RunConfig -> i -> Action as
-- type Parser as vs = as -> Either C.ParseException vs
-- type MkDataSource i = RunConfig -> DataSource i

{- 
example full fixture with custom type error ~ requires similar contraints on the Full value 
constructor to compile

  Full ::
     forall i ds dataSource action as. 
    (
     Show as,
     HasTestFields i ds, 
     dataSource ~ (RunConfig -> C.DataSource i),
     action ~ (RunConfig -> i -> Action as),
     DataSourceType dataSource ~ i,
     ActionInputType action ~ i,
     DataSourceMatchesAction (DataSourceType dataSource) (ActionInputType action)
    ) =>
    { config :: FixtureConfig,
      action :: action,
      parse :: as -> Either C.ParseException ds,
      dataSource :: dataSource
    } ->
    Fixture ()
    
-- | Creates a full fixture using the provided configuration, action, parser, and data source.
mkFullDemoErrMsgs :: forall i as vs action dataSource. (
 action ~ (RunConfig -> i -> Action as),
 dataSource ~ (RunConfig -> DataSource i),
 C.HasTestFields i vs, 
 Show as, 
 DataSourceMatchesAction (DataSourceType dataSource) (ActionInputType action)
 ) =>
 FixtureConfig 
 -> action-- action :: RunConfig -> i -> Action as
 -> as -> Either C.ParseException vs -- parser  :: as -> Either C.ParseException vs
 -> dataSource-- dataSource :: RunConfig -> DataSource i
 -> Fixture ()
mkFullDemoErrMsgs config action parse dataSource = Full {..}
-}




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
  Full' {..} -> C.Full' config' (mkHook depends) action' parse' dataSource'
  Direct' {..} -> C.Direct' {depends = mkHook depends, ..}

mkCoreSuite :: Suite -> [C.Node Action RunConfig FixtureConfig ()]
mkCoreSuite tr = mkNode <$> tr

mkHook :: Hook hz pw i o -> C.Hook (Eff ApEffs) hz i o
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
