module Prepare where

import CheckNew (Check, Checks, TerminationStatus (NonTerminal), applyCheck, skipChecks)
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
import qualified Effectful.Error.Dynamic as E
import Internal.RunTimeLogging (ExeLog)
import PyrethrumExtras (MonadCatch (catch), try, uu)
import UnliftIO.Exception (tryAny)

data PreNode m c i where
  Before ::
    { path :: C.Path
    , frequency :: C.Frequency
    , action :: i -> m o
    , subNodes :: c (PreNode m c o)
    } ->
    PreNode m c i
  After ::
    { path :: C.Path
    , frequency :: C.Frequency
    , subNodes :: c (PreNode m c o)
    , after :: m ()
    } ->
    PreNode m c i
  Around ::
    { path :: C.Path
    , frequency :: C.Frequency
    , setup :: i -> m o
    , subNodes :: c (PreNode m c o)
    , teardown :: o -> m ()
    } ->
    PreNode m c i
  Test ::
    (C.Config tc) =>
    { config :: tc
    , path :: C.Path
    , tests :: c (i -> m ())
    } ->
    PreNode m c i

type PreSuite m c i = [PreNode m c i]

-- Suite rc tc effs
-- todo:
--    - prenode subnodes => nonEmpty
--    - filtering
--    - tree shaking
--    - querying

data PrepLog = PrepLog
  { path :: C.Path
  , event :: ApEvent
  }
  deriving (Show)

type EvntSink = PrepLog -> IO ()

data PrepParams rc tc effs where
  PrepParams ::
    { eventSink :: EvntSink
    , interpreter :: forall a. Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    PrepParams rc tc effs

ioRunSuiteElm :: forall rc tc effs i. (C.Config rc, C.Config tc) => PrepParams rc tc effs -> C.SuiteElement rc tc effs i -> PreNode IO [] i
ioRunSuiteElm pp@PrepParams{eventSink, interpreter, runConfig} suiteElm =
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
      run :: forall a. C.SuiteElement rc tc effs a -> PreNode IO [] a
      run = ioRunSuiteElm pp
      intprt :: forall a. Eff effs a -> IO a
      intprt a = interpreter a >>= unTry eventSink path
    C.Test{path, test} -> prepareTest pp path test

log :: EvntSink -> C.Path -> ApEvent -> IO ()
log eventSink path = eventSink . PrepLog path

frameworkLog :: EvntSink -> C.Path -> FLog -> IO ()
frameworkLog eventSink path = log eventSink path . Framework

unTry :: forall a. EvntSink -> C.Path -> Either (CallStack, SomeException) a -> IO a
unTry es p = either (uncurry $ logThrow es p) pure



logThrow :: EvntSink -> C.Path -> CallStack -> SomeException -> IO a
logThrow es p cs ex = log es p (exceptionEvent ex cs) >> throwIO ex

prepareTest :: forall rc tc hi effs. (C.Config rc, C.Config tc, HasCallStack) => PrepParams rc tc effs -> C.Path -> C.Test rc tc effs hi -> PreNode IO [] hi
prepareTest pp@PrepParams{eventSink, interpreter, runConfig} path =
  \case
    C.Full{config, action, parse, items} ->
      Test
        { config
        , path
        , tests = runTest <$> items runConfig
        }
     where
      runTest i _ =
        do
          ds <- tryAny
            do
              flog . Action . ItemJSON $ toJSON i
              eas <- interpreter (action runConfig i)
              as <- unTry' eas
              flog . Parse . ApStateJSON $ toJSON as
              let eds = applyParser parse as
              unTry' eds
          
          applyChecks eventSink path i.checks ds
    C.Full'{config', depends, action', parse', items'} ->
      Test
        { config = config'
        , path
        , tests = runTest <$> items' runConfig
        }
     where
      runTest i hi  =
        do
          ds <- tryAny
            do
              flog . Action . ItemJSON $ toJSON i
              eas <- interpreter (action' runConfig hi i)
              as <- unTry' eas
              flog . Parse . ApStateJSON $ toJSON as
              let eds = applyParser parse' as
              unTry' eds
          applyChecks eventSink path i.checks ds
    C.NoParse{config, action, items} -> uu
    C.NoParse'{config', action', items'} -> uu
    C.Single{config, singleAction, checks} -> uu
    C.Single'{config', singleAction', checks'} -> uu
 where
  flog = frameworkLog eventSink path

  applyParser parser = mapLeft (fmap toException) . runPureEff . E.runError . parser

  unTry' :: Either (CallStack, SomeException) a -> IO a
  unTry' = unTry eventSink path
  runTest :: forall i as ds. (ToJSON as, C.Item i ds) => (i -> Eff effs as) -> (as -> Eff '[Error C.ParseException] ds) -> i -> hi -> IO ()
  runTest itoEffs parser i hi =
        do
          ds <- tryAny
            do
              flog . Action . ItemJSON $ toJSON i
              eas <- interpreter $ itoEffs i
              as <- unTry' eas
              flog . Parse . ApStateJSON $ toJSON as
              let eds = applyParser parser as
              unTry' eds
          applyChecks eventSink path i.checks ds

{-
= Action {item :: ItemJSON}
\| Parse {id :: Int, apState :: ApStateJSON}
\| CheckStart {id :: Int, apState :: ApStateJSON}
\| Check { description :: Text, result :: CheckResult }
\| Step Text
   -}

-- TODO ::
-- unit tests for all failure points including exception thrown in applying check and genrating check detailed messge
--  look out for double logging
--  exit codes with and without known error parsing

-- use FoldM and applyCheck to log check reuslts
applyChecks :: forall ds. (HasCallStack) => EvntSink -> C.Path -> Checks ds -> Either SomeException ds -> IO ()
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

data SuitePrepParams rc tc effs where
  SuitePrepParams ::
    { suite :: C.Suite rc tc effs
    , eventSink :: EvntSink
    , interpreter {- EvntSink -> -} :: Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    SuitePrepParams rc tc effs

ioRun :: SuitePrepParams rc tc effs -> PreSuite IO [] (Either (CallStack, SomeException) ())
ioRun pp@SuitePrepParams{interpreter} = uu
 where
  intprt = interpreter

data TestItem rc tc m i = TestItem
  { id :: Int
  , path :: C.Path
  , test :: rc -> i -> m ()
  , chkText :: Text
  }
