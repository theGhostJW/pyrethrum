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
    { path :: C.Path
    , frequency :: C.Frequency
    , action :: hi -> m o
    , subNodes :: c (PreNode m c o)
    } ->
    PreNode m c hi
  After ::
    { path :: C.Path
    , frequency :: C.Frequency
    , subNodes :: c (PreNode m c o)
    , after :: m ()
    } ->
    PreNode m c hi
  Around ::
    { path :: C.Path
    , frequency :: C.Frequency
    , setup :: hi -> m o
    , subNodes :: c (PreNode m c o)
    , teardown :: o -> m ()
    } ->
    PreNode m c hi
  Test ::
    (C.Config tc) =>
    { config :: tc
    , path :: C.Path
    , tests :: c (hi -> m ())
    } ->
    PreNode m c hi

data ApLog = ApLog
  { path :: C.Path
  , event :: ApEvent
  }
  deriving (Show)

type EvntSink = ApLog -> IO ()

data PrepParams rc tc effs where
  PrepParams ::
    { eventSink :: EvntSink
    , interpreter :: forall a. Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    PrepParams rc tc effs

prepSuiteElm :: forall c rc tc effs i. (C.Config rc, C.Config tc, Applicative c) => PrepParams rc tc effs -> C.SuiteElement c rc tc effs i -> PreNode IO c i
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

      run :: forall a. C.SuiteElement c rc tc effs a -> PreNode IO c a
      run = prepSuiteElm pp

      intprt :: forall a. Eff effs a -> IO a
      intprt a = interpreter a >>= unTry eventSink path
    C.Test{path, test} -> prepareTest pp path test

log :: EvntSink -> C.Path -> ApEvent -> IO ()
log eventSink path = eventSink . ApLog path

frameworkLog :: EvntSink -> C.Path -> FLog -> IO ()
frameworkLog eventSink path = log eventSink path . Framework

unTry :: forall a. EvntSink -> C.Path -> Either (CallStack, SomeException) a -> IO a
unTry es p = either (uncurry $ logThrow es p) pure

logThrow :: EvntSink -> C.Path -> CallStack -> SomeException -> IO a
logThrow es p cs ex = log es p (exceptionEvent ex cs) >> throwIO ex

prepareTest :: forall m rc tc hi effs. (C.Config tc, Applicative m) => PrepParams rc tc effs -> C.Path -> C.Test m rc tc effs hi -> PreNode IO m hi
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
                  flog . Action . ItemJSON $ toJSON config.title
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
                  flog . Action . ItemJSON $ toJSON config'.title
                  eas <- interpreter (singleAction' runConfig hi)
                  unTry' eas
                applyChecks eventSink path checks' ds
        }
 where
  flog = frameworkLog eventSink path

  applyParser parser = mapLeft (fmap toException) . runPureEff . E.runError . parser

  unTry' :: Either (CallStack, SomeException) a -> IO a
  unTry' = unTry eventSink path

  runAction :: forall i as ds. (C.Item i ds) => (i -> Eff effs as) -> i -> hi -> IO as
  runAction action i hi =
    do
      flog . Action . ItemJSON $ toJSON i
      eas <- interpreter $ action i
      unTry' eas

  runTest :: forall i as ds. (ToJSON as, C.Item i ds) => (i -> Eff effs as) -> (as -> Eff '[E.Error C.ParseException] ds) -> i -> hi -> IO ()
  runTest action parser i hi =
    do
      ds <- tryAny
        do
          as <- runAction action i hi
          flog . Parse . ApStateJSON $ toJSON as
          let eds = applyParser parser as
          unTry' eds
      applyChecks eventSink path i.checks ds

  runNoParseTest :: forall i ds. (C.Item i ds) => (i -> Eff effs ds) -> i -> hi -> IO ()
  runNoParseTest action i hi =
    tryAny (runAction action i hi) >>= applyChecks eventSink path i.checks

applyChecks :: forall ds. (ToJSON ds) => EvntSink -> C.Path -> Checks ds -> Either SomeException ds -> IO ()
applyChecks es p chks =
  either
    ( \e -> do
        flog SkipedCheckStart
        traverse_ logChk (skipChecks chks)
        throw e
    )
    applyChecks'
 where
  flog = frameworkLog es p
  logChk = flog . Check
  applyChecks' ds =
    do
      flog . CheckStart . DStateJSON $ toJSON ds
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
    , eventSink :: EvntSink
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
  , path :: C.Path
  , test :: rc -> i -> m ()
  , chkText :: Text
  }
