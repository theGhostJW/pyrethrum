module Prepare where

import Check (Check, Checks(..), TerminationStatus (NonTerminal), applyCheck, skipChecks)
import Control.Exception (throwIO)
import Control.Exception.Extra (throw)
import Control.Monad.Extra (foldM_)
import Core qualified as C
import DSL.Internal.ApEvent
    ( ApEvent(Framework),
      Path,
      exceptionEvent,
      ApStateJSON(ApStateJSON),
      DStateJSON(DStateJSON),
      FLog(SkipedCheckStart, Parse, Action, Check, CheckStart),
      ItemJSON(ItemJSON) )
import Data.Aeson (ToJSON (toJSON))
import Data.Either.Extra ( mapLeft)
import Effectful (Eff, runPureEff)
import Effectful.Error.Static qualified as E
import Internal.ThreadEvent (Hz)
import UnliftIO.Exception (tryAny)
import PyrethrumExtras (uu)

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

data TestItem m hi = TestItem
  { id :: Int
  , title :: Text
  , action :: ApEventSink -> hi -> m ()
  }

data PrepParams rc tc effs where
  PrepParams ::
    { interpreter :: forall a. Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    PrepParams rc tc effs

prepSuiteElm :: forall m rc tc effs hi. (C.Config rc, C.Config tc, Applicative m, Traversable m) => PrepParams rc tc effs -> C.Node m rc tc effs hi -> PreNode IO m hi
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

      run :: forall a. C.Node m rc tc effs a -> PreNode IO m a
      run = prepSuiteElm pp

      intprt :: forall a. ApEventSink -> Eff effs a -> IO a
      intprt snk a = interpreter a >>= unTry snk
    C.Fixture{path, fixture} -> prepareTest pp path fixture

flog :: ApEventSink -> FLog -> IO ()
flog sink = sink . Framework

unTry :: forall a. ApEventSink -> Either (CallStack, SomeException) a -> IO a
unTry es = either (uncurry $ logThrow es) pure

logThrow :: ApEventSink -> CallStack -> SomeException -> IO a
logThrow sink cs ex = sink (exceptionEvent ex cs) >> throwIO ex

prepareTest :: forall m rc tc hi effs. (C.Config tc, Applicative m) => PrepParams rc tc effs -> Path -> C.Test m rc tc effs hi -> PreNode IO m hi
prepareTest PrepParams{interpreter, runConfig} path =
  \case
    C.Full{config, action, parse, items} ->
      Test
        { config
        , path
        , tests =  ( \i ->
                TestItem
                  { id = i.id
                  , title = i.title
                  , action = \snk _hi -> runTest (action runConfig) parse i snk
                  }
            ) <$> items runConfig
        }

    C.Full'{config', action', parse', items'} ->
      Test
        { config = config'
        , path
        , tests =  ( \i ->
                TestItem
                  { id = i.id
                  , title = i.title
                  , action = \snk hi -> runTest (action' runConfig hi) parse' i snk
                  }
            ) <$> items' runConfig
        }
    C.NoParse{config, action, items} ->
      Test
        { config
        , path
        , tests =
            ( \i ->
                TestItem
                  { id = i.id
                  , title = i.title
                  , action =  \snk _hi -> runNoParseTest (action runConfig) i snk
                  }
            )
              <$> items runConfig
        }
    C.NoParse'{config', action', items'} ->
      Test
        { config = config'
        , path
        , tests =
            ( \i ->
                TestItem
                  { id = i.id
                  , title = i.title
                  , action = \snk hi -> runNoParseTest (action' runConfig hi) i snk
                  }
            )
              <$> items' runConfig
        }
    C.Single{config, singleAction, checks} ->
      Test
        { config
        , path
        , tests =
            pure
              $ TestItem
                { id = 0
                , title = config.title
                , action = \snk _hi ->
                    do
                      ds <- tryAny $ do
                        flog snk . Action path . ItemJSON $ toJSON config.title
                        eas <- interpreter (singleAction runConfig)
                        unTry snk eas
                      applyChecks snk path checks ds
                }
        }
    C.Single'{config', singleAction', checks'} ->
      Test
        { config = config'
        , path
        , tests =
            pure
              $ TestItem
                { id = 0
                , title = config'.title
                , action = \snk hi ->
                    do
                      ds <- tryAny $ do
                        flog snk . Action path . ItemJSON $ toJSON config'.title
                        eas <- interpreter (singleAction' runConfig hi)
                        unTry snk eas
                      applyChecks snk path checks' ds
                }
        }
 where
  applyParser parser = mapLeft (fmap toException) . runPureEff . E.runError . parser

  runAction :: forall i as ds. (C.Item i ds) => ApEventSink -> (i -> Eff effs as) -> i -> IO as
  runAction snk action i =
    do
      flog snk . Action path . ItemJSON $ toJSON i
      eas <- interpreter $ action i
      unTry snk eas

  runTest :: forall i as ds. (ToJSON as, C.Item i ds) => (i -> Eff effs as) -> (as -> Eff '[E.Error C.ParseException] ds) -> i -> ApEventSink -> IO ()
  runTest action parser i snk =
    do
      ds <- tryAny
        do
          as <- runAction snk action i
          flog snk . Parse path . ApStateJSON $ toJSON as
          let eds = applyParser parser as
          unTry snk eds
      applyChecks snk path i.checks ds

  runNoParseTest :: forall i ds. (C.Item i ds) => (i -> Eff effs ds) -> i -> ApEventSink -> IO ()
  runNoParseTest action i snk =
    tryAny (runAction snk action i) >>= applyChecks snk path i.checks

applyChecks :: forall ds. (ToJSON ds) => ApEventSink -> Path -> Checks ds -> Either SomeException ds -> IO ()
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
      flog snk . CheckStart p . DStateJSON $ toJSON ds
      foldM_ (applyCheck' ds) NonTerminal chks.un

  applyCheck' :: ds -> TerminationStatus -> Check ds -> IO TerminationStatus
  applyCheck' ds ts chk = do
    (cr, ts') <- applyCheck ds ts chk
    logChk cr
    pure ts'

data SuitePrepParams m rc tc effs where
  SuitePrepParams ::
    { suite :: m (C.Node m rc tc effs ())
    , interpreter :: forall a. Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    SuitePrepParams m rc tc effs

--
-- Suite rc tc effs
-- TODO:
--    - prenode subnodes => m
--    - filtering
--    - tree shaking
--    - querying
--    - validation ??
-- will return more info later such as filter log and have to return an either
filterSuite :: [C.Node [] rc tc effs ()] -> m (C.Node m rc tc effs ())
filterSuite = uu

prepare :: (C.Config rc, C.Config tc,  Applicative m, Traversable m) => SuitePrepParams [] rc tc effs -> m (PreNode IO m ())
prepare SuitePrepParams{suite, interpreter, runConfig} =
  prepSuiteElm pp <$> filterSuite suite
 where
  pp = PrepParams interpreter runConfig
