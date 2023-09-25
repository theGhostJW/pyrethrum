module Prepare where

import Control.Exception (throwIO)
import qualified Core as C
import DSL.Internal.ApEvent
import DSL.Out (Sink (..))
import Effectful (Eff)
import Internal.RunTimeLogging (ExeLog)
import PyrethrumExtras (uu)

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
    { tests :: c (i -> m ())
    } ->
    PreNode m c i

type PreSuite m c i = [PreNode m c i]

-- Suite rc tc effs
-- todo:
--    - prenode subnodes => nonEmpty
--    - filtering
--    - tree shaking
--    - querying

type EvntSink = ExeLog C.Path ApEvent

data PrepParams rc tc effs where
  PrepParams ::
    { eventSink :: EvntSink
    , interpreter :: forall a. EvntSink -> Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    PrepParams rc tc effs

ioRunSuiteElm :: forall rc tc effs i. PrepParams rc tc effs -> C.SuiteElement rc tc effs i -> PreNode IO [] i
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
            { path = path
            , frequency = frequency
            , subNodes = subNodes
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
    C.Test{path, test} -> runTest pp path test
 where
  run :: forall a. C.SuiteElement rc tc effs a -> PreNode IO [] a
  run = ioRunSuiteElm pp

  intprt :: forall a. Eff effs a -> IO a
  intprt action =
    interpreter eventSink action
      >>=
      -- todo throwing away callstack recreate nested exceptption or something to preserve callstack
      either
        (\(callstack, exception) -> throwIO exception)
        pure


runTest :: PrepParams rc tc effs -> C.Path -> C.Test rc tc effs i -> PreNode IO [] i
runTest pp@PrepParams{eventSink, interpreter, runConfig} path = 
   \case 
     C.Full {config, action, parse, items} -> uu
     C.Full' {config', parent, action', parse', items'} -> uu
     C.NoParse {config, action, items} -> uu
     C.NoParse' {parent, action', items'} -> uu
     C.Single {config, singleAction, checks} -> uu
     C.Single' {parent, singleAction', checks'} -> uu
    -- where 
    --   uu
    
    
data SuitePrepParams rc tc effs where
  SuitePrepParams ::
    { suite :: C.Suite rc tc effs
    , eventSink :: EvntSink
    , interpreter :: EvntSink -> Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    SuitePrepParams rc tc effs

ioRun :: SuitePrepParams rc tc effs -> PreSuite IO [] (Either (CallStack, SomeException) ())
ioRun pp@SuitePrepParams{interpreter} = uu
 where
  intprt = interpreter pp.eventSink

data TestItem rc tc m i = TestItem
  { id :: Int
  , path :: C.Path
  , test :: rc -> i -> m ()
  , chkText :: Text
  }

-- prepare :: (Suite rc tc effs -> m b) -> Suite rc tc effs ->

-- type Suite rc tc effs = [SuiteElement rc tc effs ()]

-- data SuiteElement rc tc effs i where
--   Hook ::
--     { path :: Path
--     , hook :: Hook rc tc effs loc i o
--     , subNodes :: [SuiteElement rc tc effs o]
--     } ->
--     SuiteElement rc tc effs i--   Test ::
--     { path :: Path
--     , test :: Test rc tc effs i
--     } ->
--     SuiteElement rc tc effs i
