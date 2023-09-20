module Prepare where

import Control.Exception (throwIO)
import qualified Core as C
import DSL.Internal.ApEvent
import DSL.Out (Sink (..))
import Effectful (Eff)
import Internal.RunTimeLogging (ExeLog)
import PyrethrumExtras (uu)

data Frequency = Once | Thread | Each deriving (Show, Eq)

data PreNode m c i where
  Before ::
    { path :: C.Path
    , frequency :: Frequency
    , action :: i -> m o
    , subNodes :: c (PreNode m c o)
    } ->
    PreNode m c i
  After ::
    { path' :: C.Path
    , frequency' :: Frequency
    , subNodes' :: c (PreNode m c o)
    , after :: m ()
    } ->
    PreNode m c ()
  Around ::
    { path :: C.Path
    , frequency :: Frequency
    , setUp :: i -> m o
    , subNodes :: c (PreNode m c o)
    , tearDown :: o -> m ()
    } ->
    PreNode m c i
  Test ::
    { tests :: [i -> m ()]
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
    C.Hook{hook, path, subNodes} ->
      hook & \case
        C.OnceBefore{onceAction} ->
          Before
            { path
            , frequency = Once
            , action = \_ -> intprt $ onceAction runConfig
            , subNodes = run <$> subNodes
            }
        C.OnceBefore'
          { onceAction'
          } ->
            Before
              { path
              , frequency = Once
              , action = \i -> intprt $ onceAction' i runConfig
              , subNodes = run <$> subNodes
              }
        C.OnceAfter{onceAfter} ->
          After
            { path' = path
            , frequency' = Once
            , subNodes' = run <$> subNodes
            , after = intprt $ onceAfter runConfig
            }
        C.OnceAfter'{} -> uu
        C.OnceAround{} -> uu
        C.OnceAround'{} -> uu
        C.ThreadBefore{threadAction} ->
          Before
            { path
            , frequency = Thread
            , action = \_ -> intprt $ threadAction runConfig
            , subNodes = run <$> subNodes
            }
        C.ThreadBefore'{threadAction'} ->
          Before
            { path
            , frequency = Thread
            , action = \i -> intprt $ threadAction' i runConfig
            , subNodes = run <$> subNodes
            }
        C.ThreadAfter{} -> uu
        C.ThreadAfter'{} -> uu
        C.ThreadAround{} -> uu
        C.ThreadAround'{} -> uu
        C.EachBefore{eachAction} ->
          Before
            { path
            , frequency = Each
            , action = \_ -> intprt $ eachAction runConfig
            , subNodes = run <$> subNodes
            }
        C.EachBefore'{eachAction'} ->
          Before
            { path
            , frequency = Each
            , action = \i -> intprt $ eachAction' i runConfig
            , subNodes = run <$> subNodes
            }
        C.EachAfter{} -> uu
        C.EachAfter'{} -> uu
        C.EachAround{} -> uu
        C.EachAround'{} -> uu
    C.Test{} -> uu
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
