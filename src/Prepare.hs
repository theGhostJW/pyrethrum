module Prepare where

import Check (Check, Checks (..), FailStatus (NonTerminal), applyCheck, skipChecks)
import Control.Exception (throwIO)
import Control.Exception.Extra (throw)
import Control.Monad.Extra (foldM_)
import Core qualified as C
import DSL.Internal.ApEvent (
  ApEvent (Framework),
  ApStateText (ApStateText),
  DStateText (DStateText),
  FLog (Action, Check, CheckStart, Parse, SkipedCheckStart),
  ItemText (ItemText),
  Path,
  exceptionEvent,
 )
import Data.Either.Extra (mapLeft)
import Internal.ThreadEvent (Hz)
import PyrethrumExtras (toS, txt, uu)
import UnliftIO.Exception (tryAny)

data PreNode m c hi where
  Before ::
    { path :: Path
    , frequency :: Hz
    , action :: ApEventSink -> hi -> m o
    , subNodes :: c (PreNode m c o)
    } ->
    PreNode m c hi
  After ::
    { path :: Path
    , frequency :: Hz
    , subNodes' :: c (PreNode m c hi)
    , after :: ApEventSink -> m ()
    } ->
    PreNode m c hi
  Around ::
    { path :: Path
    , frequency :: Hz
    , setup :: ApEventSink -> hi -> m o
    , subNodes :: c (PreNode m c o)
    , teardown :: ApEventSink -> o -> m ()
    } ->
    PreNode m c hi
  Test ::
    (C.Config tc) =>
    { config :: tc
    , path :: Path
    , tests :: c (TestItem m hi)
    } ->
    PreNode m c hi

type ApEventSink = ApEvent -> IO ()

-- used in debugging
listPaths :: forall m c hi. Foldable c =>  PreNode m c hi -> [(Int, Path)]
listPaths = 
  reverse . step 0 []
 where
  step :: forall hi'. Int -> [(Int, Path)] -> PreNode m c hi' -> [(Int, Path)]
  step i accum n =
    n & \case
      Test{} -> accum'
      Before{subNodes} -> accumPaths subNodes
      After{subNodes'} -> accumPaths subNodes'
      Around{subNodes} -> accumPaths subNodes
   where
    accum' = (i, n.path) : accum
    accumPaths :: forall hii. c (PreNode m c hii) -> [(Int, Path)]
    accumPaths = foldl' (step $ succ i) accum'

data TestItem m hi = TestItem
  { id :: Int
  , title :: Text
  , action :: ApEventSink -> hi -> m ()
  }

data PrepParams m rc tc where
  PrepParams ::
    { interpreter :: forall a. m a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    PrepParams m rc tc

prepSuiteElm :: forall m c rc tc hi. (C.Config rc, C.Config tc, Applicative c, Traversable c) => PrepParams m rc tc -> C.Node m c rc tc hi -> PreNode IO c hi
prepSuiteElm pp@PrepParams{interpreter, runConfig} suiteElm =
  suiteElm & \case
    C.Hook{hook, path, subNodes = subNodes'} ->
      hook & \case
        C.Before{action} ->
          Before
            { path
            , frequency
            , action = \snk -> const . intprt snk $ action runConfig
            , subNodes
            }
        C.Before'
          { action'
          } ->
            Before
              { path
              , frequency
              , action = \snk -> intprt snk . action' runConfig
              , subNodes
              }
        C.After{afterAction} ->
          After
            { path
            , frequency
            , subNodes' = subNodes
            , after = \snk -> intprt snk $ afterAction runConfig
            }
        C.After'{afterAction'} ->
          After
            { path
            , frequency
            , subNodes' = subNodes
            , after = \snk -> intprt snk $ afterAction' runConfig
            }
        C.Around
          { setup
          , teardown
          } ->
            Around
              { path
              , frequency
              , setup = \snk -> const . intprt snk $ setup runConfig
              , subNodes
              , teardown = \snk -> intprt snk . teardown runConfig
              }
        C.Around'
          { setup'
          , teardown'
          } ->
            Around
              { path
              , frequency
              , setup = \snk -> intprt snk . setup' runConfig
              , subNodes
              , teardown = \snk -> intprt snk . teardown' runConfig
              }
     where
      frequency = C.hookFrequency hook

      subNodes = run <$> subNodes'

      run :: forall a. C.Node m c rc tc a -> PreNode IO c a
      run = prepSuiteElm pp

      intprt :: forall a. ApEventSink -> m a -> IO a
      intprt snk a = interpreter a >>= unTry snk
    C.Fixture{path, fixture} -> prepareTest pp path fixture

flog :: ApEventSink -> FLog -> IO ()
flog sink = sink . Framework

unTry :: forall a e. (Exception e) => ApEventSink -> Either (CallStack, e) a -> IO a
unTry es = either (uncurry $ logThrow es) pure

logThrow :: (Exception e) => ApEventSink -> CallStack -> e -> IO a
logThrow sink cs ex = sink (exceptionEvent ex cs) >> throwIO ex

prepareTest :: forall m c rc tc hi. (C.Config tc, Applicative c) => PrepParams m rc tc -> Path -> C.Fixture m c rc tc hi -> PreNode IO c hi
prepareTest PrepParams{interpreter, runConfig} path =
  \case
    C.Full{config, action, parse, items} ->
      Test
        { config
        , path
        , tests =
            ( \i ->
                TestItem
                  { id = i.id
                  , title = i.title
                  , action = \snk _hi -> runTest (action runConfig) parse i snk
                  }
            )
              <$> items runConfig
        }
    C.Full'{config', action', parse', items'} ->
      Test
        { config = config'
        , path
        , tests =
            ( \i ->
                TestItem
                  { id = i.id
                  , title = i.title
                  , action = \snk hi -> runTest (action' runConfig hi) parse' i snk
                  }
            )
              <$> items' runConfig
        }
    C.Direct{config, action, items} ->
      Test
        { config
        , path
        , tests =
            ( \i ->
                TestItem
                  { id = i.id
                  , title = i.title
                  , action = \snk _hi -> runDirectTest (action runConfig) i snk
                  }
            )
              <$> items runConfig
        }
    C.Direct'{config', action', items'} ->
      Test
        { config = config'
        , path
        , tests =
            ( \i ->
                TestItem
                  { id = i.id
                  , title = i.title
                  , action = \snk hi -> runDirectTest (action' runConfig hi) i snk
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

  {-
  TODO :: Try Different Stategies
    - eg. Eff vs dependency injection - vs Eff embedded in core (see before remove effectful from core)
    - tests
      - hooks with lower / different capabilities than tests (rest hook vs ui test) see "Demonstraits using partial effect" for effectful version
      - effects that use effects / logger
      - arbitary STM as an example of adding an effect that requires its own context

  -}
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

data SuitePrepParams m c rc tc where
  SuitePrepParams ::
    { suite :: c (C.Node m c rc tc ())
    , interpreter :: forall a. m a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    SuitePrepParams m c rc tc

--
-- Suite rc tc effs
-- TODO:
--    - prenode subnodes => m
--    - filtering
--    - tree shaking
--    - querying
--    - validation - eg. no repeat item titles
-- will return more info later such as filter log and have to return an either
filterSuite :: forall m c rc tc. c (C.Node m c rc tc ()) -> c (C.Node m c rc tc ())
filterSuite = uu

prepare :: (C.Config rc, C.Config tc, Applicative c, Traversable c) => SuitePrepParams m c rc tc -> c (PreNode IO c ())
prepare SuitePrepParams{suite, interpreter, runConfig} =
  prepSuiteElm pp <$> filterSuite suite
 where
  pp = PrepParams interpreter runConfig
