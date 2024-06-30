module Prepare where

import Check (Check, Checks (..), FailStatus (NonTerminal), applyCheck, skipChecks)
import Control.Exception (throwIO)
import Control.Exception.Extra (throw)
import Control.Monad.Extra (foldM_)
import Core qualified as C
import DSL.Internal.NodeEvent
  ( NodeEvent (Framework),
    ApStateText (ApStateText),
    DStateText (DStateText),
    FLog (Action, Check, CheckStart, Parse, SkipedCheckStart),
    ItemText (ItemText),
    Path,
    exceptionEvent,
  )
import Data.Either.Extra (mapLeft)
import Internal.ThreadEvent (Hz)
import PyrethrumExtras (toS, txt)
import UnliftIO.Exception (tryAny)


-- TODO Full E2E property tests from Core fixtures and Hooks --> logs
-- can reuse some suiteruntime chks
-- should be able to write a converter from template to core hooks and fixtures

prepare :: (C.Config rc, C.Config fc) => SuitePrepParams m rc fc -> [PreNode IO ()]
prepare SuitePrepParams {suite, interpreter, runConfig} =
  prepSuiteElm (PrepParams interpreter runConfig) <$> suite

data PreNode m hi where
  Before ::
    { path :: Path,
      frequency :: Hz,
      action :: ApEventSink -> hi -> m o,
      subNodes :: [PreNode m o]
    } ->
    PreNode m hi
  After ::
    { path :: Path,
      frequency :: Hz,
      subNodes' :: [PreNode m hi],
      after :: ApEventSink -> m ()
    } ->
    PreNode m hi
  Around ::
    { path :: Path,
      frequency :: Hz,
      setup :: ApEventSink -> hi -> m o,
      subNodes :: [PreNode m o],
      teardown :: ApEventSink -> o -> m ()
    } ->
    PreNode m hi
  Fixture ::
    (C.Config fc) =>
    { config :: fc,
      path :: Path,
      tests :: C.DataSource (Test m hi)
    } ->
    PreNode m hi

type ApEventSink = NodeEvent -> IO ()

-- used in debugging
listPaths :: forall m hi. PreNode m hi -> [(Int, Path)]
listPaths =
  reverse . step 0 []
  where
    step :: forall hi'. Int -> [(Int, Path)] -> PreNode m hi' -> [(Int, Path)]
    step i accum n =
      n & \case
        Fixture {} -> accum'
        Before {subNodes} -> accumPaths subNodes
        After {subNodes'} -> accumPaths subNodes'
        Around {subNodes} -> accumPaths subNodes
      where
        accum' = (i, n.path) : accum
        accumPaths :: forall hii. [PreNode m hii] -> [(Int, Path)]
        accumPaths = foldl' (step $ succ i) accum'

data Test m hi = MkTest
  { id :: Int,
    title :: Text,
    action :: ApEventSink -> hi -> m ()
  }

data PrepParams m rc fc where
  PrepParams ::
    { interpreter :: forall a. m a -> IO (Either (CallStack, SomeException) a),
      runConfig :: rc
    } ->
    PrepParams m rc fc

prepSuiteElm :: forall m rc fc hi. (C.Config rc, C.Config fc) => PrepParams m rc fc -> C.Node m rc fc hi -> PreNode IO hi
prepSuiteElm pp@PrepParams {interpreter, runConfig} suiteElm =
  suiteElm & \case
    C.Hook {hook, path, subNodes = subNodes'} ->
      hook & \case
        C.Before {action} ->
          Before
            { path,
              frequency,
              action = \snk -> const . intprt snk $ action runConfig,
              subNodes
            }
        C.Before'
          { action'
          } ->
            Before
              { path,
                frequency,
                action = \snk -> intprt snk . action' runConfig,
                subNodes
              }
        C.After {afterAction} ->
          After
            { path,
              frequency,
              subNodes' = subNodes,
              after = \snk -> intprt snk $ afterAction runConfig
            }
        C.After' {afterAction'} ->
          After
            { path,
              frequency,
              subNodes' = subNodes,
              after = \snk -> intprt snk $ afterAction' runConfig
            }
        C.Around
          { setup,
            teardown
          } ->
            Around
              { path,
                frequency,
                setup = \snk -> const . intprt snk $ setup runConfig,
                subNodes,
                teardown = \snk -> intprt snk . teardown runConfig
              }
        C.Around'
          { setup',
            teardown'
          } ->
            Around
              { path,
                frequency,
                setup = \snk -> intprt snk . setup' runConfig,
                subNodes,
                teardown = \snk -> intprt snk . teardown' runConfig
              }
      where
        frequency = C.hookFrequency hook
        subNodes = run <$> subNodes'

        run :: forall a. C.Node m rc fc a -> PreNode IO a
        run = prepSuiteElm pp

        intprt :: forall a. ApEventSink -> m a -> IO a
        intprt snk a = interpreter a >>= unTry snk
    C.Fixture {path, fixture} -> prepareTest pp path fixture

flog :: ApEventSink -> FLog -> IO ()
flog sink = sink . Framework

unTry :: forall a e. (Exception e) => ApEventSink -> Either (CallStack, e) a -> IO a
unTry es = either (uncurry $ logThrow es) pure

logThrow :: (Exception e) => ApEventSink -> CallStack -> e -> IO a
logThrow sink cs ex = sink (exceptionEvent ex cs) >> throwIO ex

prepareTest :: forall m rc fc hi. (C.Config fc) => PrepParams m rc fc -> Path -> C.Fixture m rc fc hi -> PreNode IO hi
prepareTest PrepParams {interpreter, runConfig} path =
  \case
    C.Full {config, action, parse, items} ->
      Fixture
        { config,
          path,
          tests =
            ( \i ->
                MkTest
                  { id = i.id,
                    title = i.title,
                    action = \snk _hi -> runTest (action runConfig) parse i snk
                  }
            )
              <$> items runConfig
        }
    C.Full' {config', action', parse', items'} ->
      Fixture
        { config = config',
          path,
          tests =
            ( \i ->
                MkTest
                  { id = i.id,
                    title = i.title,
                    action = \snk hi -> runTest (action' runConfig hi) parse' i snk
                  }
            )
              <$> items' runConfig
        }
    C.Direct {config, action, items} ->
      Fixture
        { config,
          path,
          tests =
            ( \i ->
                MkTest
                  { id = i.id,
                    title = i.title,
                    action = \snk _hi -> runDirectTest (action runConfig) i snk
                  }
            )
              <$> items runConfig
        }
    C.Direct' {config', action', items'} ->
      Fixture
        { config = config',
          path,
          tests =
            ( \i ->
                MkTest
                  { id = i.id,
                    title = i.title,
                    action = \snk hi -> runDirectTest (action' runConfig hi) i snk
                  }
            )
              <$> items' runConfig
        }
  where
    applyParser :: forall as ds. ((HasCallStack) => as -> Either C.ParseException ds) -> as -> IO (Either (CallStack, C.ParseException) ds)
    applyParser parser as =
      tryAny (pure $ parser as)
        <&> either
          (\e -> Left (callStack, C.ParseFailure . toS $ displayException e))
          (mapLeft (callStack,))

    runAction :: forall i as ds. (C.Item i ds) => ApEventSink -> (i -> m as) -> i -> IO as
    runAction snk action i =
      do
        flog snk . Action path . ItemText $ txt i
        eas <- interpreter $ action i
        unTry snk eas

    runTest :: forall i as ds. (Show as, C.Item i ds) => (i -> m as) -> ((HasCallStack) => as -> Either C.ParseException ds) -> i -> ApEventSink -> IO ()
    runTest action parser i snk =
      do
        ds <- tryAny
          do
            as <- runAction snk action i
            flog snk . Parse path . ApStateText $ txt as
            eds <- applyParser parser as
            unTry snk eds
        applyChecks snk path i.checks ds

    runDirectTest :: forall i ds. (C.Item i ds) => (i -> m ds) -> i -> ApEventSink -> IO ()
    runDirectTest action i snk =
      tryAny (runAction snk action i) >>= applyChecks snk path i.checks

applyChecks :: forall ds. (Show ds) => ApEventSink -> Path -> Checks ds -> Either SomeException ds -> IO ()
applyChecks snk p chks =
  either
    ( \e -> do
        log $ SkipedCheckStart p
        traverse_ logChk (skipChecks chks)
        throw e
    )
    applyChecks'
  where
    log = flog snk
    logChk = log . Check p
    applyChecks' ds =
      do
        flog snk . CheckStart p . DStateText $ txt ds
        foldM_ (applyCheck' ds) NonTerminal chks.un

    applyCheck' :: ds -> FailStatus -> Check ds -> IO FailStatus
    applyCheck' ds ts chk = do
      (cr, ts') <- applyCheck ds ts chk
      logChk cr
      pure ts'

data SuitePrepParams m rc fc where
  SuitePrepParams ::
    { suite :: [C.Node m rc fc ()],
      interpreter :: forall a. m a -> IO (Either (CallStack, SomeException) a),
      runConfig :: rc
    } ->
    SuitePrepParams m rc fc
