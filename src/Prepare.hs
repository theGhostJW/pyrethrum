{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Prepare
  ( PreNode (..),
    LogSink,
    Test (..),
    prepare,
    listPaths,
  )
where

import Check (Check, Checks (..), FailStatus (NonTerminal), applyCheck, skipChecks, listChecks, CheckReport)
import Control.Exception (throwIO)
import Control.Exception.Extra (throw)
import Control.Monad.Extra (foldM_)
import Core (Mode (..), SuiteExeParams)
import Core qualified as C
import CoreUtils (Hz)
import DSL.Internal.NodeEvent
  ( ApStateText (ApStateText),
    DStateText (DStateText),
    FrameworkLog (Action, Check, CheckStart, Parse, SkipedCheckStart),
    ItemText (ItemText),
    LogSink,
    NodeEvent (Framework),
    Path,
    exceptionEvent,
  )
import Data.Either.Extra (mapLeft) -- ToDO: move to Pyrelude
import Internal.SuiteFiltering (FilteredSuite (..), filterSuite)
import Internal.SuiteValidation (SuiteValidationError (..), chkSuite)
import PyrethrumExtras (txt, uu)
import UnliftIO.Exception (tryAny)

-- TODO Full E2E property tests from Core fixtures and Hooks --> logs
-- can reuse some suiteruntime chks
-- should be able to write a converter from template to core hooks and fixtures

prepare :: (C.Config rc, C.Config fc, HasCallStack) => SuiteExeParams m rc fc -> Either SuiteValidationError (FilteredSuite (PreNode IO ()))
prepare C.MkSuiteExeParams {suite, mode, filters, interpreter, runConfig = rc} =
  mSuiteError
    & maybe
      ( Right $
          MkFilteredSuite
            { suite = prepSuite mode interpreter rc filtered.suite,
              filterResults = filtered.filterResults
            }
      )
      Left
  where
    filtered = filterSuite filters rc suite
    mSuiteError = chkSuite filtered.filterResults

prepSuite :: (C.Config rc, C.Config fc, HasCallStack) => Mode -> (forall a. LogSink -> m a -> IO a) -> rc -> [C.Node m rc fc ()] -> [PreNode IO ()]
prepSuite mode interpreter rc suite = prepNode mode interpreter rc <$> suite

data PreNode m hi where
  Before ::
    { path :: Path,
      frequency :: Hz,
      action :: LogSink -> hi -> m o,
      subNodes :: [PreNode m o]
    } ->
    PreNode m hi
  After ::
    { path :: Path,
      frequency :: Hz,
      subNodes' :: [PreNode m hi],
      after :: LogSink -> m ()
    } ->
    PreNode m hi
  Around ::
    { path :: Path,
      frequency :: Hz,
      setup :: LogSink -> hi -> m o,
      subNodes :: [PreNode m o],
      teardown :: LogSink -> o -> m ()
    } ->
    PreNode m hi
  Fixture ::
    (C.Config fc) =>
    { config :: fc,
      path :: Path,
      tests :: C.DataSource (Test m hi)
    } ->
    PreNode m hi

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
    action :: LogSink -> hi -> m ()
  }

prepNode ::
  forall m rc fc hi.
  (HasCallStack, C.Config rc, C.Config fc) =>
  Mode ->
  (forall a. LogSink -> m a -> IO a) -> -- interpreter
  rc -> -- runConfig
  C.Node m rc fc hi -> -- node
  PreNode IO hi
prepNode mode interpreter rc suiteElm =
  suiteElm & \case
    C.Hook {hook, path, subNodes = subNodes'} ->
      hook & \case
        C.Before {action} ->
          Before
            { path,
              frequency,
              action = \snk -> const . intprt snk $ action rc,
              subNodes
            }
        C.Before'
          { action'
          } ->
            Before
              { path,
                frequency,
                action = \snk -> intprt snk . action' rc,
                subNodes
              }
        C.After {afterAction} ->
          After
            { path,
              frequency,
              subNodes' = subNodes,
              after = \snk -> intprt snk $ afterAction rc
            }
        C.After' {afterAction'} ->
          After
            { path,
              frequency,
              subNodes' = subNodes,
              after = \snk -> intprt snk $ afterAction' rc
            }
        C.Around
          { setup,
            teardown
          } ->
            Around
              { path,
                frequency,
                setup = \snk -> const . intprt snk $ setup rc,
                subNodes,
                teardown = \snk -> intprt snk . teardown rc
              }
        C.Around'
          { setup',
            teardown'
          } ->
            Around
              { path,
                frequency,
                setup = \snk -> intprt snk . setup' rc,
                subNodes,
                teardown = \snk -> intprt snk . teardown' rc
              }
      where
        frequency = C.hookFrequency hook
        subNodes = run <$> subNodes'

        run :: forall a. C.Node m rc fc a -> PreNode IO a
        run = prepNode mode interpreter rc

        intprt :: forall a. LogSink -> m a -> IO a
        intprt snk a = catchLog snk $ interpreter snk a
    C.Fixture {path, fixture} -> prepareTest mode interpreter rc path fixture

flog :: (HasCallStack) => LogSink -> FrameworkLog -> IO ()
flog sink = sink . Framework

catchLog :: forall a. (HasCallStack) => LogSink -> IO a -> IO a
catchLog as io = tryAny io >>= either (logThrow as) pure

logThrow :: (HasCallStack) => LogSink -> SomeException -> IO a
logThrow sink ex = sink (exceptionEvent ex callStack) >> throwIO ex

unTry :: forall a. LogSink -> Either SomeException a -> IO a
unTry es = either (logThrow es) pure

prepareTest ::
  forall m rc fc hi.
  (C.Config fc) =>
  Mode -> -- meta interpreter mode
  (forall a. LogSink -> m a -> IO a) -> -- interpreter
  rc -> -- runConfig
  Path -> -- path of test
  C.Fixture m rc fc hi ->
  PreNode IO hi
prepareTest mode interpreter rc path =
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
                    action = \snk _hi -> runTest (action rc) parse i snk
                  }
            )
              <$> items rc
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
                    action = \snk hi -> runTest (action' rc hi) parse' i snk
                  }
            )
              <$> items' rc
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
                    action = \snk _hi -> runDirectTest (action rc) i snk
                  }
            )
              <$> items rc
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
                    action = \snk hi -> runDirectTest (action' rc hi) i snk
                  }
            )
              <$> items' rc
        }
  where
    applyParser :: forall as ds. ((HasCallStack) => as -> Either C.ParseException ds) -> as -> Either SomeException ds
    applyParser parser as = mapLeft toException $ parser as

    logItem :: forall i. (Show i) => LogSink -> i -> IO ()
    logItem snk = flog snk . Action path . ItemText . txt 

    runAction :: forall i as ds. (C.Item i ds) => LogSink -> (i -> m as) -> i -> IO as
    runAction snk action = catchLog snk . interpreter snk . action

    runTest :: forall i as ds. (Show as, C.Item i ds) => (i -> m as) -> ((HasCallStack) => as -> Either C.ParseException ds) -> i -> LogSink -> IO ()
    runTest action parser i snk =
      case mode of
        Run ->
          do
            ds <- tryAny
              do
                logItem snk i
                as <- runAction snk action i
                let !evt = Parse path . ApStateText $ txt as
                flog snk evt
                unTry snk $ applyParser parser as
            applyChecks snk path i.checks ds
        Listing {includeSteps, includeChecks} -> 
          do
            logItem snk i
            when includeSteps $
              void $ runAction snk action i
            when includeChecks $
              traverse_ logChk (listChecks i.checks)
            where 
              logChk = flog snk . Check path
 
            -- runTest' action parser i snk
            -- if includeChecks then runChecks action parser i snk else pure ()

    runDirectTest :: forall i ds. (C.Item i ds) => (i -> m ds) -> i -> LogSink -> IO ()
    runDirectTest action i snk =
      here refactor and reuse Listing case above
      tryAny (runAction snk action i) >>= applyChecks mode snk path i.checks


applyChecks :: forall ds. (Show ds) => LogSink -> Path -> Checks ds -> Either SomeException ds -> IO ()
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
 
