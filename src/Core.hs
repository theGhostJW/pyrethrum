{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Core where

import Check
import Control.Concurrent.Async qualified as Async
import Control.Monad.Extra (foldM_)
import DSL.Internal.ApEvent hiding (Check)
import DSL.Internal.ApEvent qualified as AE
import Data.Aeson (ToJSON (..))
import GHC.Records (HasField)
import Internal.ThreadEvent (Hz (..))
import PyrethrumExtras (txt, uu, (?))
import System.IO.Unsafe (unsafePerformIO)
import System.Log.TSLogger qualified as TS

data Before
data After
data Around

class When a

instance When Before
instance When After
instance When Around

newtype CheckFailure = CheckFailure Text
    deriving (Show)

newtype ParseException
    = ParseFailure Text
    deriving (Show, Read)

instance Exception ParseException

type HasTitle a = HasField "title" a Text

type HasId a = HasField "id" a Int

class (HasTitle a, Show a, ToJSON a, Eq a) => Config a

type Item i ds = (HasTitle i, HasId i, HasField "checks" i (Checks ds), Show i, Read i, Show ds, Show i)

failParser :: Text -> Either ParseException a
failParser = Left . ParseFailure

-- add a function to serialise known properties to JSON on id, title, -> optional tricky may need to parse from string  notes, calcs
-- https://softwareengineering.stackexchange.com/a/250135https://softwareengineering.stackexchange.com/a/250135
{-

Property [Data] Checks Shrinker

where data is serialisable to and from JSON so can rerun speicific instances and add
to list

-}

{-
TODO:: idea defect reconciler
 - can pretty print (No JSON) to log so long as id/title/hash is logged and a hash of the show is logged so
    we can warn if the item has changed since it was run

 - for property based / generative tests we insist on deriving JSON and log as YAML and pprint
    on failure

 - consider removing ids from items
    ~ regular use title which must be unique or title stub (which can include test)
    ~ property based stub
      ~ generator

 - global stub function
 - tst stub function  Item -> [Item] -> [Item] ~ ignores list and creates singleton
-}

-- TODO:: look into listLike

--

class Frequency a where
    frequency :: Hz

class CanDependOn a b

data Once
data Thread
data Each

instance Frequency Once where
    frequency :: Hz
    frequency = Once

instance Frequency Thread where
    frequency :: Hz
    frequency = Thread

instance Frequency Each where
    frequency :: Hz
    frequency = Each

instance CanDependOn Once Once
instance CanDependOn Thread Once
instance CanDependOn Thread Thread
instance CanDependOn Each Once
instance CanDependOn Each Thread
instance CanDependOn Each Each

data Hook m hz i o where
    Before ::
        (Frequency hz) =>
        { action :: m o
        } ->
        Hook m hz () o
    Before' ::
        (Frequency phz, Frequency hz, CanDependOn hz phz) =>
        { depends :: Hook m phz pi i
        , action' :: i -> m o
        } ->
        Hook m hz i o
    After ::
        (Frequency hz) =>
        { afterAction :: m ()
        } ->
        Hook m hz () ()
    After' ::
        (Frequency phz, Frequency hz, CanDependOn hz phz) =>
        { afterDepends :: Hook m phz pi i
        , afterAction' :: m ()
        } ->
        Hook m hz i i
    Around ::
        (Frequency hz) =>
        { setup :: m o
        , teardown :: o -> m ()
        } ->
        Hook m hz () o
    Around' ::
        (Frequency phz, Frequency hz, CanDependOn hz phz) =>
        { depends :: Hook m phz pi i
        , setup' :: i -> m o
        , teardown' :: o -> m ()
        } ->
        Hook m hz i o

hookFrequency :: forall m hz i o. (Frequency hz) => Hook m hz i o -> Hz
hookFrequency _ = frequency @hz

instance Concurrently IO where
    runConcurrently as = Async.forConcurrently as id

class (Monad m) => Logger m where
    fLog :: FLog -> m ()

logger :: TS.Logger
logger = unsafePerformIO $ TS.newLogger (0, TS.dbgLvl) [TS.OutputTo stdout] TS.DontWait

instance Logger IO where
    fLog = TS.logStrLn logger 0 . show

class Concurrently m where
    runConcurrently :: (Traversable c) => c (m a) -> m (c a)

forConcurrently_ ::
    (Functor m, Traversable c, Concurrently m) =>
    c a ->
    (a -> m b) ->
    m ()
forConcurrently_ t f = runConcurrently (f <$> t) $> ()

runFixture ::
    (Logger m, Concurrently m, Traversable c) =>
    Path ->
    m o ->
    (o -> m ()) ->
    Fixture m c tc o ->
    m ()
runFixture p beforeEachHook afterEachHook f = case f of
    Direct{config, action, items} -> forConcurrently_ items $ \i -> do
        ds <- action i
        runCheck p ds i.checks.un
    Direct'{config', action', items'} -> forConcurrently_ items' $ \i -> do
        o <- beforeEachHook
        ds <- action' o i
        afterEachHook o
        runCheck p ds i.checks.un
    Full{config, parse, action, items} -> forConcurrently_ items $ \i -> do
        d <- action i
        case parse d of
            Left e -> (fLog $ Exception (show e) (show callStack))
            Right ds -> runCheck p ds i.checks.un
    Full'{config', parse', action', items'} -> forConcurrently_ items' $ \i -> do
        hi <- beforeEachHook
        o <- action' hi i
        afterEachHook hi
        case parse' o of
            Left e -> (fLog $ Exception (show e) (show callStack))
            Right ds -> runCheck p ds i.checks.un

runNode ::
    (Logger m, Concurrently m, Traversable c) =>
    m hi ->
    (hi -> m ()) ->
    Node m c tc hi ->
    m ()
runNode bh ah Fixture{path, fixture} = runFixture path bh ah fixture
runNode bh ah Hook{hook, subNodes = nodes} =
    let f = hookFrequency hook
     in case hook of
            Before{action}
                | f == Each -> forConcurrently_ nodes $ \n ->
                    runNode action (const . pure $ ()) n
                | otherwise -> do
                    i <- action
                    forConcurrently_ nodes $ \n -> do
                        runNode (pure i) (const . pure $ ()) n
            Before'{action'}
                | f == Each -> forConcurrently_ nodes $ \n ->
                    runNode (bh >>= action') (const . pure $ ()) n
                | otherwise -> do
                    o <- bh
                    i <- action' o
                    forConcurrently_ nodes $ \n -> do
                        runNode (pure i) (const . pure $ ()) n
            After{afterAction}
                | f == Each -> forConcurrently_ nodes $ \n ->
                    runNode bh (\i -> afterAction >> ah i) n
                | otherwise -> do
                    forConcurrently_ nodes $ \n -> runNode bh (const . pure $ ()) n
                    afterAction
            After'{afterAction'}
                | f == Each -> forConcurrently_ nodes $ \n ->
                    runNode bh (\i -> afterAction' >> ah i) n
                | otherwise -> do
                    forConcurrently_ nodes $ \n -> runNode bh (const . pure $ ()) n
                    afterAction'
            Around{setup, teardown}
                | f == Each -> forConcurrently_ nodes $ \n -> do
                    hi <- bh
                    runNode setup teardown n
                    ah hi
                | otherwise -> do
                    hi <- bh
                    o <- setup
                    forConcurrently_ nodes $ \n -> runNode (pure o) (const . pure $ ()) n
                    teardown o
                    ah hi
            Around'{setup', teardown'}
                | f == Each -> forConcurrently_ nodes $ \n -> do
                    hi <- bh
                    runNode (setup' hi) teardown' n
                    ah hi
                | otherwise -> do
                    hi <- bh
                    o <- setup' hi
                    forConcurrently_ nodes $ \n -> runNode (pure o) (const . pure $ ()) n
                    teardown' o
                    ah hi

-- runNode ::
--     (Monad m, Concurrently m, Traversable c) =>
--     Node m c tc hi ->
--     m ()
-- runNode = uu
--
-- runHook ::
--     (Monad m, Concurrently m, Traversable c) =>
--     Hook m hz pi o ->
--     c (o -> m ()) ->
--     m ()
-- runHook hook as = case hook of
--     Before{action} -> case hookFrequency hook of
--         Once -> action >>= \o -> forConcurrently_ as $ \a -> a o
--         Thread -> forConcurrently_ as $ \a -> do
--             o <- action
--             a o
--         Each -> forConcurrently_ as $ \a -> do
--             o <- action
--             a o
--     Before'{depends, action'} -> case hookFrequency hook of
--         Once -> runHook depends newAs
--           where
--             newAs =
--                 as <&> \a i -> do
--                     o <- action' i
--                     a o
--         Thread -> runHook depends newAs
--           where
--             newAs =
--                 as <&> \a i -> do
--                     o <- action' i
--                     a o
--         Each -> runHook depends newAs
--           where
--             newAs =
--                 as <&> \a i -> do
--                     o <- action' i
--                     a o
--     Around{setup, teardown} -> case hookFrequency hook of
--         Once -> do
--             o <- setup
--             forConcurrently_ as $ \a -> a o
--             teardown o
--         Thread -> forConcurrently_ as $ \a -> do
--             o <- setup
--             a o
--             teardown o
--         Each -> forConcurrently_ as $ \a -> do
--             o <- setup
--             a o
--             teardown o
--     Around'{depends, setup', teardown'} -> case hookFrequency hook of
--         Once -> runHook depends newAs
--           where
--             newAs =
--                 as <&> \a i -> do
--                     o <- setup' i
--                     a o
--                     teardown' o
--         Thread -> runHook depends newAs
--           where
--             newAs =
--                 as <&> \a i -> do
--                     o <- setup' i
--                     a o
--                     teardown' o
--         Each -> runHook depends newAs
--           where
--             newAs =
--                 as <&> \a i -> do
--                     o <- setup' i
--                     a o
--                     teardown' o
--     After{afterAction} -> case hookFrequency hook of
--         Once -> do
--             forConcurrently_ as $ \a -> a ()
--             afterAction
--         Thread -> forConcurrently_ as $ \a -> do
--             a ()
--             afterAction
--         Each -> forConcurrently_ as $ \a -> do
--             a ()
--             afterAction
--     After'{afterDepends, afterAction'} -> case hookFrequency hook of
--         Once -> runHook afterDepends newAs
--           where
--             newAs =
--                 as <&> \a i -> do
--                     a i
--                     afterAction'
--         Thread -> runHook afterDepends newAs
--           where
--             newAs =
--                 as <&> \a i -> do
--                     a i
--                     afterAction'
--         Each -> runHook afterDepends newAs
--           where
--             newAs =
--                 as <&> \a i -> do
--                     a i
--                     afterAction'
--
-- runFixture ::
--     (HasCallStack, Logger m, Concurrently m, Traversable c) =>
--     Path ->
--     Fixture m c tc hi ->
--     m ()
-- runFixture p Direct'{config', depends, action', items'} =
--     runHook depends $
--         items' <&> \i ho -> do
--             ds <- action' ho i
--             runCheck p ds i.checks.un
-- runFixture p Direct{config, action, items} =
--     forConcurrently_ items $ \i -> do
--         ds <- action i
--         runCheck p ds i.checks.un
-- runFixture p Full{config, action, parse, items} = forConcurrently_ items $ \i -> do
--     d <- action i
--     case parse d of
--         Left e -> (fLog $ Exception (show e) (show callStack))
--         Right ds -> runCheck p ds i.checks.un
-- runFixture p Full'{config', depends, action', parse', items'} =
--     runHook depends $
--         items' <&> \i ho -> do
--             o <- action' ho i
--             case parse' o of
--                 Left e -> (fLog $ Exception (show e) (show callStack))
--                 Right ds -> runCheck p ds i.checks.un

-- runfixture ::
--     (hascallstack, logger m, concurrently m, traversable c, cache m) =>
--     path ->
--     fixture m c tc hi ->
--     m ()
-- runfixture p direct'{config', depends, action', items'} =
--     runhook depends $
--         items' <&> \i ho -> do
--             ds <- action' ho i
--             runcheck p ds i.checks.un
-- runfixture p direct{config, action, items} =
--     forconcurrently_ items $ \i -> do
--         ds <- action i
--         runcheck p ds i.checks.un
-- runfixture p full{config, action, parse, items} = forconcurrently_ items $ \i -> do
--     d <- action i
--     case parse d of
--         left e -> (flog $ exception (show e) (show callstack))
--         right ds -> runcheck p ds i.checks.un
-- runfixture p full'{config', depends, action', parse', items'} =
--     runhook depends $
--         items' <&> \i ho -> do
--             o <- action' ho i
--             case parse' o of
--                 left e -> (flog $ exception (show e) (show callstack))
--                 Right ds -> runCheck p ds i.checks.un
-- runFull ::
--     (HasCallStack, Logger m, Item i ds, Traversable c, Concurrently m) =>
--     Path ->
--     (i -> m as) ->
--     (as -> Either ParseException ds) ->
--     c i ->
--     m ()
-- runFull p action parse items = forConcurrently_ items applyAction
--   where
--     applyAction i = do
--         d <- action i
--         case parse d of
--             Left e -> (fLog $ Exception (show e) (show callStack))
--             Right ds -> runCheck p ds i.checks.un

-- runFixture ::
--     (HasCallStack, Logger m, Concurrently m, Traversable c) =>
--     Path ->
--     Fixture m c tc hi ->
--     m ()
-- runFixture p Direct'{config', depends, action', items'} = case hookFrequency depends of
--     Once -> runHook depends $ \ho ->
--         forConcurrently_ items' $ \i -> do
--             ds <- action' ho i
--             runCheck p ds i.checks.un
--     Each ->
--         forConcurrently_ items' $ \i ->
--             runHook depends $ \ho -> do
--                 ds <- action' ho i
--                 runCheck p ds i.checks.un
-- runFixture p Direct{config, action, items} =
--     forConcurrently_ items $ \i -> do
--         ds <- action i
--         runCheck p ds i.checks.un
-- runFixture p Full{config, action, parse, items} = forConcurrently_ items $ \i -> do
--     d <- action i
--     case parse d of
--         Left e -> (fLog $ Exception (show e) (show callStack))
--         Right ds -> runCheck p ds i.checks.un
-- runFixture p Full'{config', depends, action', parse', items'} = case hookFrequency depends of
--     Once -> runHook depends $ \ho ->
--         forConcurrently_ items' $ \i -> do
--             d <- action' ho i
--             case parse' d of
--                 Left e -> (fLog $ Exception (show e) (show callStack))
--                 Right ds -> runCheck p ds i.checks.un
--     Each -> forConcurrently_ items' $ \i ->
--         runHook depends $ \ho -> do
--             d <- action' ho i
--             case parse' d of
--                 Left e -> (fLog $ Exception (show e) (show callStack))
--                 Right ds -> runCheck p ds i.checks.un
--
runChecks :: (Traversable c, Logger m, Show ds, Concurrently m) => Path -> c (ds, [Check ds]) -> m ()
runChecks p cds =
    forConcurrently_ cds $ \(ds, checks) -> runCheck p ds checks

runCheck :: (Logger m, Show ds) => Path -> ds -> [Check ds] -> m ()
runCheck p ds checks = do
    fLog . CheckStart p . DStateText $ txt ds
    foldM_ (applyCheckk p ds) NonTerminal checks

applyCheckk :: (Logger m) => Path -> ds -> FailStatus -> Check ds -> m FailStatus
applyCheckk p ds previousStatus ck = do
    let (result, status) = case previousStatus of
            Terminal -> (Skip, Terminal)
            NonTerminal -> ck.rule ds ? (Pass, NonTerminal) $ (Fail, ck.failStatus)
    fLog . AE.Check p $ report result
    pure status
  where
    report r = CheckReport r ck.header (ck.message & maybe "" (ds &))

data Fixture m c tc hi where
    Full ::
        (Item i ds, Show as) =>
        { config :: tc
        , action :: i -> m as
        , parse :: as -> Either ParseException ds
        , items :: c i
        } ->
        Fixture m c tc ()
    Full' ::
        (Item i ds, Show as, Frequency hz) =>
        { config' :: tc
        , depends :: Hook m hz pi hi
        , action' :: hi -> i -> m as
        , parse' :: as -> Either ParseException ds
        , items' :: c i
        } ->
        Fixture m c tc hi
    Direct ::
        (Item i ds) =>
        { config :: tc
        , action :: i -> m ds
        , items :: c i
        } ->
        Fixture m c tc ()
    Direct' ::
        (Item i ds, Frequency hz) =>
        { config' :: tc
        , depends :: Hook m hz pi hi
        , action' :: hi -> i -> m ds
        , items' :: c i
        } ->
        Fixture m c tc hi

data Node m c tc hi where
    Hook ::
        (Frequency hz) =>
        { path :: Path
        , hook :: Hook m hz hi o
        , subNodes :: c (Node m c tc o)
        } ->
        Node m c tc hi
    Fixture ::
        { path :: Path
        , fixture :: Fixture m c tc hi
        } ->
        Node m c tc hi

data ExeParams m c rc tc where
    ExeParams ::
        { suite :: c (Node m c tc ())
        , interpreter :: forall a. m a -> IO (Either (CallStack, SomeException) a)
        , runConfig :: rc
        } ->
        ExeParams m c rc tc

-- try this
-- part 1
--  - do notation :: NA
-- - interpretor + writer :: NA
-- - extract hz from item (hard code for now) + fixtureType and dependency  hz
-- - stub for checks (see part 4)
-- - add missing fixtures
-- - reinstate before
-- - hook around
-- - fixture hooks
-- - instance hooks
-- - check constraints line up
-- change prenode and execution to match new structure
-- - need 2 files
--  - fixture list - plain old do for initial extraction -- hard code for now
--  - test Suite - the actual test suite with interperetor as param
--   - error as warning extension added to the top of both generated files
-- stub for run test
-- - generate suite based on prenode
-- part 2
-- reinstrate run test
-- demo running single test
-- part 3
--  - unit tests for suite runtime
-- part 4
-- - implement check for recursion check
-- - generator (hie or below)
-- remove stubLoc from type
-- see weeder :: HIE
-- start with:: https://github.com/theGhostJW/pyrethrum-extras/blob/master/src/Language/Haskell/TH/Syntax/Extended.hs
-- see also:: https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/Language-Haskell-TH-Syntax.html#t:Name
-- part 5 reinstate filtering // tree shaking

{-
todo plugin issu

differ
case 1:
  hook.depends = hook1
  hook depends hook 1

case 2:
  hook1 depends hook2
  hook = hook1
  hook depends hook2

-}
