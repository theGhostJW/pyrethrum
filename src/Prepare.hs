module Prepare where

import CheckNew (Check, Checks, TerminationStatus (NonTerminal), applyCheck, skipChecks, un)
import Control.Exception (throwIO)
import Control.Exception.Extra (throw)
import Control.Monad.Extra (foldM_)
import qualified Core as C
import DSL.Internal.ApEvent
import DSL.Out (Sink (..))
import Data.Aeson (ToJSON (toJSON))
import Data.Either.Extra (fromRight', mapLeft)
import Effectful (Eff, runEff, runPureEff)
import Effectful.Dispatch.Dynamic (interpret)
import qualified Effectful.Error.Static as E
import Internal.RunTimeLogging (ExeLog)
import PyrethrumExtras (MonadCatch (catch), try, uu)
import UnliftIO.Exception (tryAny)

data PreNode m c hi where
  Before ::
    { path :: Path
    , frequency :: C.Frequency
    , action :: ApEventSink -> hi -> m o
    , subNodes :: c (PreNode m c o)
    } ->
    PreNode m c hi
  After ::
    { path :: Path
    , frequency :: C.Frequency
    , subNodes' :: c (PreNode m c hi)
    , after :: ApEventSink -> m ()
    } ->
    PreNode m c hi
  Around ::
    { path :: Path
    , frequency :: C.Frequency
    , setup :: ApEventSink -> hi -> m o
    , subNodes :: c (PreNode m c o)
    , teardown :: ApEventSink -> o -> m ()
    } ->
    PreNode m c hi
  Test ::
    (C.Config tc) =>
    { config :: tc
    , path :: Path
    , tests :: c (ApEventSink -> hi -> m ())
    } ->
    PreNode m c hi

type ApEventSink = ApEvent -> IO ()

data PrepParams rc tc effs where
  PrepParams ::
    { interpreter :: forall a. Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    PrepParams rc tc effs

prepSuiteElm :: forall m rc tc effs hi. (C.Config rc, C.Config tc, Applicative m) => PrepParams rc tc effs -> C.SuiteElement m rc tc effs hi -> PreNode IO m hi
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

      run :: forall a. C.SuiteElement m rc tc effs a -> PreNode IO m a
      run = prepSuiteElm pp

      intprt :: forall a. ApEventSink -> Eff effs a -> IO a
      intprt snk a = interpreter a >>= unTry snk
    C.Test{path, test} -> prepareTest pp path test

flog :: ApEventSink -> FLog -> IO ()
flog sink = sink . Framework

unTry :: forall a. ApEventSink -> Either (CallStack, SomeException) a -> IO a
unTry es = either (uncurry $ logThrow es) pure

logThrow :: ApEventSink -> CallStack -> SomeException -> IO a
logThrow sink cs ex = sink (exceptionEvent ex cs) >> throwIO ex

prepareTest :: forall m rc tc hi effs. (C.Config tc, Applicative m) => PrepParams rc tc effs -> Path -> C.Test m rc tc effs hi -> PreNode IO m hi
prepareTest pp@PrepParams{interpreter, runConfig} path =
  \case
    C.Full{config, action, parse, items} ->
      Test
        { config
        , path
        , tests = runTest (action runConfig) parse <$> items runConfig
        }
    C.Full'{config', depends, action', parse', items'} ->
      Test
        { config = config'
        , path
        , tests = (\i snk hi -> runTest (action' runConfig hi) parse' i snk hi) <$> items' runConfig
        }
    C.NoParse{config, action, items} ->
      Test
        { config
        , path
        , tests = runNoParseTest (action runConfig) <$> items runConfig
        }
    C.NoParse'{config', action', items'} ->
      Test
        { config = config'
        , path
        , tests = (\i snk hi -> runNoParseTest (action' runConfig hi) i snk hi) <$> items' runConfig
        }
    C.Single{config, singleAction, checks} ->
      Test
        { config
        , path
        , tests =
            pure $ \snk hi ->
              do
                ds <- tryAny $ do
                  flog snk . Action path . ItemJSON $ toJSON config.title
                  eas <- interpreter (singleAction runConfig)
                  unTry snk eas
                applyChecks snk path checks ds
        }
    C.Single'{config', singleAction', checks'} ->
      Test
        { config = config'
        , path
        , tests =
            pure $ \snk hi ->
              do
                ds <- tryAny $ do
                  -- no item to log so just log the test title
                  flog snk . Action path . ItemJSON $ toJSON config'.title
                  eas <- interpreter (singleAction' runConfig hi)
                  unTry snk eas
                applyChecks snk path checks' ds
        }
 where
  applyParser parser = mapLeft (fmap toException) . runPureEff . E.runError . parser

  runAction :: forall i as ds. (C.Item i ds) => ApEventSink -> (i -> Eff effs as) -> i -> hi -> IO as
  runAction snk action i hi =
    do
      flog snk . Action path . ItemJSON $ toJSON i
      eas <- interpreter $ action i
      unTry snk eas

  runTest :: forall i as ds. (ToJSON as, C.Item i ds) => (i -> Eff effs as) -> (as -> Eff '[E.Error C.ParseException] ds) -> i -> ApEventSink -> hi -> IO ()
  runTest action parser i snk hi =
    do
      ds <- tryAny
        do
          as <- runAction snk action i hi
          flog snk . Parse path . ApStateJSON $ toJSON as
          let eds = applyParser parser as
          unTry snk eds
      applyChecks snk path i.checks ds

  runNoParseTest :: forall i ds. (C.Item i ds) => (i -> Eff effs ds) -> i -> ApEventSink -> hi -> IO ()
  runNoParseTest action i snk hi =
    tryAny (runAction snk action i hi) >>= applyChecks snk path i.checks

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
    { suite :: m (C.SuiteElement m rc tc effs ())
    , interpreter :: forall a. Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    SuitePrepParams m rc tc effs

--
-- Suite rc tc effs
-- TODO:
--    - prenode subnodes => nonEmpty
--    - filtering
--    - tree shaking
--    - querying
--    - validation ??
-- will return more info later such as filter log and have to return an either
filterSuite :: [C.SuiteElement [] rc tc effs ()] -> NonEmpty (C.SuiteElement NonEmpty rc tc effs ())
filterSuite = uu

prepare :: (C.Config rc, C.Config tc) => SuitePrepParams [] rc tc effs -> NonEmpty (PreNode IO NonEmpty ())
prepare spp@SuitePrepParams{suite, interpreter, runConfig} =
  prepSuiteElm pp <$> filterSuite suite
 where
  pp = PrepParams interpreter runConfig
