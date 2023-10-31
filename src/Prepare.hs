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
    , action :: hi -> m o
    , subNodes :: c (PreNode m c o)
    } ->
    PreNode m c hi
  After ::
    { path :: Path
    , frequency :: C.Frequency
    , subNodes :: c (PreNode m c o)
    , after :: m ()
    } ->
    PreNode m c hi
  Around ::
    { path :: Path
    , frequency :: C.Frequency
    , setup :: hi -> m o
    , subNodes :: c (PreNode m c o)
    , teardown :: o -> m ()
    } ->
    PreNode m c hi
  Test ::
    (C.Config tc) =>
    { config :: tc
    , path :: Path
    , tests :: c (hi -> m ())
    } ->
    PreNode m c hi

type ApEventSink = ApEvent -> IO ()

data PrepParams rc tc effs where
  PrepParams ::
    { eventSink :: ApEventSink
    , interpreter :: forall a. Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    PrepParams rc tc effs

prepSuiteElm :: forall m rc tc effs hi. (C.Config rc, C.Config tc, Applicative m) => PrepParams rc tc effs -> C.SuiteElement m rc tc effs hi -> PreNode IO m hi
prepSuiteElm pp@PrepParams{eventSink, interpreter, runConfig} suiteElm =
  suiteElm & \case
    C.Hook{hook, path, subNodes = subNodes'} ->
      hook & \case
        C.Before{action} ->
          Before
            { path
            , frequency
            , action = const . intprt $ action runConfig
            , subNodes
            }
        C.Before'
          { action'
          } ->
            Before
              { path
              , frequency
              , action = intprt . action' runConfig
              , subNodes
              }
        C.After{afterAction} ->
          After
            { path
            , frequency
            , subNodes
            , after = intprt $ afterAction runConfig
            }
        C.After'{afterAction'} ->
          After
            { path
            , frequency
            , subNodes
            , after = intprt $ afterAction' runConfig
            }
        C.Around
          { setup
          , teardown
          } ->
            Around
              { path
              , frequency
              , setup = const . intprt $ setup runConfig
              , subNodes
              , teardown = intprt . teardown runConfig
              }
        C.Around'
          { setup'
          , teardown'
          } ->
            Around
              { path
              , frequency
              , setup = intprt . setup' runConfig
              , subNodes
              , teardown = intprt . teardown' runConfig
              }
     where
      frequency = C.hookFrequency hook


      subNodes = run <$> subNodes'

      run :: forall a. C.SuiteElement m rc tc effs a -> PreNode IO m a
      run = prepSuiteElm pp

      intprt :: forall a. Eff effs a -> IO a
      intprt a = interpreter a >>= unTry eventSink
    C.Test{path, test} -> prepareTest pp path test



frameworkLog :: ApEventSink -> FLog -> IO ()
frameworkLog sink = sink . Framework

unTry :: forall a. ApEventSink -> Either (CallStack, SomeException) a -> IO a
unTry es = either (uncurry $ logThrow es) pure

logThrow :: ApEventSink -> CallStack -> SomeException -> IO a
logThrow sink cs ex = sink (exceptionEvent ex cs) >> throwIO ex

prepareTest :: forall m rc tc hi effs. (C.Config tc, Applicative m) => PrepParams rc tc effs -> Path -> C.Test m rc tc effs hi -> PreNode IO m hi
prepareTest pp@PrepParams{eventSink, interpreter, runConfig} path =
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
        , tests = (\i hi -> runTest (action' runConfig hi) parse' i hi) <$> items' runConfig
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
        , tests = (\i hi -> runNoParseTest (action' runConfig hi) i hi) <$> items' runConfig
        }
    C.Single{config, singleAction, checks} ->
      Test
        { config
        , path
        , tests =
            pure $ \hi ->
              do
                ds <- tryAny $ do
                  flog . Action path . ItemJSON $ toJSON config.title
                  eas <- interpreter (singleAction runConfig)
                  unTry' eas
                applyChecks eventSink path checks ds
        }
    C.Single'{config', singleAction', checks'} ->
      Test
        { config = config'
        , path
        , tests =
            pure $ \hi ->
              do
                ds <- tryAny $ do
                  flog . Action path . ItemJSON $ toJSON config'.title
                  eas <- interpreter (singleAction' runConfig hi)
                  unTry' eas
                applyChecks eventSink path checks' ds
        }
 where
  flog = frameworkLog eventSink

  applyParser parser = mapLeft (fmap toException) . runPureEff . E.runError . parser

  unTry' :: Either (CallStack, SomeException) a -> IO a
  unTry' = unTry eventSink

  runAction :: forall i as ds. (C.Item i ds) => (i -> Eff effs as) -> i -> hi -> IO as
  runAction action i hi =
    do
      flog . Action path . ItemJSON $ toJSON i
      eas <- interpreter $ action i
      unTry' eas

  runTest :: forall i as ds. (ToJSON as, C.Item i ds) => (i -> Eff effs as) -> (as -> Eff '[E.Error C.ParseException] ds) -> i -> hi -> IO ()
  runTest action parser i hi =
    do
      ds <- tryAny
        do
          as <- runAction action i hi
          flog . Parse path . ApStateJSON $ toJSON as
          let eds = applyParser parser as
          unTry' eds
      applyChecks eventSink path i.checks ds

  runNoParseTest :: forall i ds. (C.Item i ds) => (i -> Eff effs ds) -> i -> hi -> IO ()
  runNoParseTest action i hi =
    tryAny (runAction action i hi) >>= applyChecks eventSink path i.checks

applyChecks :: forall ds. (ToJSON ds) => ApEventSink -> Path -> Checks ds -> Either SomeException ds -> IO ()
applyChecks es p chks =
  either
    ( \e -> do
        flog $ SkipedCheckStart p
        traverse_ logChk (skipChecks chks)
        throw e
    )
    applyChecks'
 where
  flog = frameworkLog es
  logChk = flog . Check p
  applyChecks' ds =
    do
      flog . CheckStart p . DStateJSON $ toJSON ds
      foldM_ applyCheck' NonTerminal chks.un
   where
    applyCheck' :: TerminationStatus -> Check ds -> IO TerminationStatus
    applyCheck' ts chk = do
      (cr, ts') <- applyCheck ds ts chk
      logChk cr
      pure ts'

data SuitePrepParams m rc tc effs where
  SuitePrepParams ::
    { suite :: m (C.SuiteElement m rc tc effs ())
    , eventSink :: ApEventSink
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
-- will return more info later such as filter log and have to return an either
filterSuite :: [C.SuiteElement [] rc tc effs ()] -> NonEmpty (C.SuiteElement NonEmpty rc tc effs ())
filterSuite = uu

prepare :: (C.Config rc, C.Config tc) => SuitePrepParams [] rc tc effs -> NonEmpty (PreNode IO NonEmpty ())
prepare spp@SuitePrepParams{suite, eventSink, interpreter, runConfig} =
  prepSuiteElm pp <$> filterSuite suite
 where
  pp = PrepParams eventSink interpreter runConfig

data TestItem rc tc m i = TestItem
  { id :: Int
  , path :: Path
  , test :: rc -> i -> m ()
  , chkText :: Text
  }
