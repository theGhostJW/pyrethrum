module Prepare where

import Control.Exception (throwIO)
import qualified Core as C
import DSL.Internal.ApEvent
import DSL.Out (Sink (..))
import Effectful (Eff, runEff, runPureEff)
import Internal.RunTimeLogging (ExeLog)
import PyrethrumExtras ( uu, MonadCatch(catch), try )
import Data.Either.Extra (fromRight', mapLeft)
import Data.Aeson (ToJSON(toJSON))
import qualified Effectful.Error.Dynamic as E
import Effectful.Dispatch.Dynamic (interpret)
import CheckNew (applyCheck)

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
    {
      config :: tc,
      path :: C.Path,
      tests :: c (i -> m ())
    } ->
    PreNode m c i

type PreSuite m c i = [PreNode m c i]

-- Suite rc tc effs
-- todo:
--    - prenode subnodes => nonEmpty
--    - filtering
--    - tree shaking
--    - querying

data PrepLog = PrepLog {
  path :: C.Path,
  event :: ApEvent
} deriving Show

type EvntSink = PrepLog -> IO ()

data PrepParams rc tc effs where
  PrepParams ::
    { eventSink :: EvntSink
    , interpreter :: forall a. {- EvntSink -> -} Eff effs a -> IO (Either (CallStack, SomeException) a)
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
      intprt a = interpreter a >>= toIO eventSink path
    C.Test{path, test} -> prepareTest pp path test




frameworkLog :: EvntSink -> C.Path -> FLog -> IO ()
frameworkLog eventSink path = eventSink . PrepLog path . Framework

toIO :: EvntSink -> C.Path -> Either (CallStack, SomeException) a -> IO a
toIO es p = either (\(cs, ex) -> frameworkLog es p (Exception ex cs) >> throwIO ex) pure

log :: EvntSink -> C.Path -> FLog -> IO ()
log eventSink path = eventSink . PrepLog path . Framework

runChecks :: EvntSink -> C.Path -> Either (CallStack, SomeException) ds -> IO ()
runChecks es p eds = \case
  Left (cs, ex) -> frameworkLog es p (exceptionEvent ex cs) >> throwIO ex
  Right ds -> frameworkLog es p (Check ds)

prepareTest :: forall rc tc hi effs. (C.Config rc, C.Config tc) => PrepParams rc tc effs -> C.Path -> C.Test rc tc effs hi -> PreNode IO [] hi
prepareTest pp@PrepParams{eventSink, interpreter, runConfig} path =
   \case
     C.Full {config, action, parse, items} -> Test {
                                                    config,
                                                    path,
                                                    tests = items
                                                }
                                              where
                                                runTest i =
                                                  do
                                                    ds <- try
                                                          do
                                                             log' . Action . ItemJSON $ toJSON i
                                                             eas <- interpreter (action runConfig i)
                                                             as <- toIO' eas
                                                             log' . Parse . ApStateJSON $ toJSON as
                                                             let eds = applyParser parse as
                                                             toIO' eds
                                                             log . CheckStart . DStateJSON $ toJSON ds
                                                      -- Run and log checks




     C.Full' {config', depends, action', parse', items'} -> uu
     C.NoParse {config, action, items} -> uu
     C.NoParse' {config', action', items'} -> uu
     C.Single {config, singleAction, checks} -> uu
     C.Single' {config', singleAction', checks'} -> uu
    where
     log' = log eventSink path
     applyParser parser = mapLeft (fmap toException) . runPureEff . E.runError . parser
     toIO' :: Either (CallStack, SomeException) a -> IO a
     toIO' = toIO eventSink path
     {-
  = Action {item :: ItemJSON}
  | Parse {id :: Int, apState :: ApStateJSON}
  | CheckStart {id :: Int, apState :: ApStateJSON} 
  | Check { description :: Text, result :: CheckResult }
  | Step Text
     -}

applyChecks -- use FoldM and applyCheck
data SuitePrepParams rc tc effs where
  SuitePrepParams ::
    { suite :: C.Suite rc tc effs
    , eventSink :: EvntSink
    , interpreter :: {- EvntSink -> -} Eff effs a -> IO (Either (CallStack, SomeException) a)
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