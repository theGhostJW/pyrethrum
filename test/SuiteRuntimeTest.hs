module SuiteRuntimeTest where

import qualified Core
import DSL.Internal.ApEvent (ApEvent, Path (..))
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import qualified FullSuiteTestTemplate as T
import Internal.RunTimeLogging (ExePath, testLogControls)
import Internal.SuiteRuntime (ThreadCount (..), executeNodeList)
import Internal.ThreadEvent (Frequency (..), ThreadEvent (..))
import qualified Prepare as P
import PyrethrumExtras (toS, txt, (?))
import PyrethrumExtras.Test hiding (chkEq', maybe, test)
import Text.Show.Pretty (pPrint, ppShow)
import UnliftIO.Concurrent as C (
  threadDelay,
 )
import UnliftIO.STM (TQueue, tryReadTQueue)
import Prelude hiding (id, pass)
import qualified Data.Map.Strict as M
import qualified List.Extra as L

type LogItem = ThreadEvent ExePath DSL.Internal.ApEvent.ApEvent

chkProperties :: Int -> [Template] -> [LogItem] -> IO ()
chkProperties _mxThrds _t evts = do
  traverse_
    (evts &)
    [ chkStartEndExecution
    , chkThreadLogsInOrder
    ]
  putStrLn " checks done"

chkStartEndExecution :: [ThreadEvent ExePath DSL.Internal.ApEvent.ApEvent] -> IO ()
chkStartEndExecution evts =
  (,)
    <$> L.head evts
    <*> L.last evts
      & maybe
        (fail "no events")
        ( \(s, e) -> do
            s & \case
              StartExecution{} -> pure ()
              _ -> fail $ "first event is not StartExecution:\n " <> toS (ppShow s)
            e & \case
              EndExecution{} -> pure ()
              _ -> fail $ "last event is not EndExecution:\n " <> toS (ppShow e)
        )


-- TODO - add tests add to pyrelude
-- research groupon on lists is consecutive and dodgy
-- conver to to non-empty and use groupby from relude??
groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f =
  M.elems . foldl' fld M.empty . reverse
 where
  fld m a =
    M.lookup (f a) m
      & maybe
        (M.insert (f a) [a] m)
        (\as -> M.insert (f a) (a : as) m)

chkThreadLogsInOrder :: [LogItem] -> IO ()
chkThreadLogsInOrder evts =
  do
    chk' "Nothing found in heads - groupOn error this should not happen" (all isJust heads)
    traverse_ (chkEq' "first index of thread should be 0" 0 . (.idx)) $ catMaybes heads
    traverse_ chkIds threads
 where
  threads = groupOn (.threadId) evts
  -- TODO: need to draw a line in the sand re maybe vs nonemptyList
  heads = L.head <$> threads
  chkIds evts' =
    for_
      (zip evts' $ drop 1 evts')
      ( \(ev1, ev2) ->
          let idx1 = ev1.idx
              idx2 = ev2.idx
           in chkEqfmt' (succ idx1) idx2 $
                "event idx not consecutive\n"
                  <> toS (ppShow ev1)
                  <> "\n"
                  <> toS (ppShow ev2)
      )

-- -- TODO - better formatting chkEq pyrelude
chkEqfmt' :: (Eq a, Show a) => a -> a -> Text -> IO ()
chkEqfmt' e a msg = chkEq' msg e a

chkEq' :: (Eq a, Show a) => Text -> a -> a -> IO ()
chkEq' msg e a =
  when (e /= a) $
    fail $
      "\n"
        <> toS msg
        <> "\n"
        <> "equality check failed:\n"
        <> "Expected:\n  "
        <> ppShow e
        <> "\nDoes not Equal:\n  "
        <> ppShow a
        <> "\n"

-- $> unit_simple_pass
unit_simple_pass :: IO ()
unit_simple_pass = runTest True 1 [onceAround True True [test [testItem True, testItem False]]]

-- $ > unit_simple_fail
unit_simple_fail :: IO ()
unit_simple_fail = runTest False 1 [onceAround False True [test [testItem True, testItem False]]]

-- $ > unit_nested_thread_pass_fail
unit_nested_thread_pass_fail :: IO ()
unit_nested_thread_pass_fail =
  runTest
    False
    1
    [ onceAround
        True
        True
        [ threadAround True True [eachAfter False [test [testItem True, testItem False]]]
        , threadAround False True [eachAfter True [test [testItem True, testItem False]]]
        ]
    ]

onceBefore :: Bool -> [Template] -> Template
onceBefore = OnceBefore 0

onceAfter :: Bool -> [Template] -> Template
onceAfter = OnceAfter 0

onceAround :: Bool -> Bool -> [Template] -> Template
onceAround = OnceAround 0

threadBefore :: Bool -> [Template] -> Template
threadBefore = ThreadBefore 0

threadAfter :: Bool -> [Template] -> Template
threadAfter = ThreadAfter 0

threadAround :: Bool -> Bool -> [Template] -> Template
threadAround = ThreadAround 0

eachBefore :: Bool -> [Template] -> Template
eachBefore = EachBefore 0

eachAfter :: Bool -> [Template] -> Template
eachAfter = EachAfter 0

eachAround :: Bool -> Bool -> [Template] -> Template
eachAround = EachAround 0

test :: [TestItem] -> Template
test = Test

testItem :: Bool -> TestItem
testItem = TestItem 0

runTest :: Bool -> Int -> [Template] -> IO ()
runTest wantConsole maxThreads templates = do
  lg <- exeTemplate wantConsole (ThreadCount maxThreads) templates
  chkProperties maxThreads templates lg

exeTemplate :: Bool -> ThreadCount -> [Template] -> IO [ThreadEvent ExePath DSL.Internal.ApEvent.ApEvent]
exeTemplate wantConsole maxThreads testNodes = do
  (lc, logQ) <- testLogControls wantConsole
  let templates = setPaths "" $ toList testNodes
  when wantConsole $ do
    putStrLn ""
    pPrint templates
    putStrLn "========="
  executeNodeList maxThreads lc $ mkNodes templates
  atomically $ q2List logQ

q2List :: TQueue a -> STM [a]
q2List qu = reverse <$> recurse [] qu
 where
  recurse :: [a] -> TQueue a -> STM [a]
  recurse l q =
    tryReadTQueue q
      >>= maybe (pure l) (\e -> recurse (e : l) q)

setPaths :: Text -> [Template] -> [T.Template]
setPaths address ts =
  uncurry setPath <$> zip [0 ..] ts
 where
  nxtAdd idx =
    let
      txIdx = txt idx
      sfx = T.null address ? txIdx $ "." <> txIdx
     in
      address <> sfx

  setPath :: Int -> Template -> T.Template
  setPath idx tp =
    case tp of
      SuiteRuntimeTest.Test{testItems} ->
        T.Test
          { path = newPath "Test"
          , testItems = zip [0 ..] testItems <&> \(idx', TestItem{..}) -> T.TestItem{title = newAdd <> " TestItem", id = idx', ..}
          }
      -- _ -> case tp of
      -- get a (invalid??) ambiguous update warining if I try to use record update for these 2 fields
      -- in a subscase statement here - try refactor after record update changes go into GHC
      OnceBefore{..} -> T.OnceBefore{path = newPath "OnceBefore", subNodes = newNodes, ..}
      OnceAfter{..} -> T.OnceAfter{path = newPath "OnceAfter", subNodes = newNodes, ..}
      OnceAround{..} -> T.OnceAround{path = newPath "OnceAround", subNodes = newNodes, ..}
      ThreadBefore{..} -> T.ThreadBefore{path = newPath "ThreadBefore", subNodes = newNodes, ..}
      ThreadAfter{..} -> T.ThreadAfter{path = newPath "ThreadAfter", subNodes = newNodes, ..}
      ThreadAround{..} -> T.ThreadAround{path = newPath "ThreadAround", subNodes = newNodes, ..}
      EachBefore{..} -> T.EachBefore{path = newPath "EachBefore", subNodes = newNodes, ..}
      EachAfter{..} -> T.EachAfter{path = newPath "EachAfter", subNodes = newNodes, ..}
      EachAround{..} -> T.EachAround{path = newPath "EachAround", subNodes = newNodes, ..}
   where
    newPath = DSL.Internal.ApEvent.SuiteElmPath newAdd
    newAdd = nxtAdd idx
    newNodes = setPaths newAdd tp.subNodes

newtype TestConfig = TestConfig
  {title :: Text}
  deriving (Generic, Show, Eq)

instance ToJSON TestConfig
instance Core.Config TestConfig

tc :: TestConfig
tc = TestConfig{title = "test config"}

mkVoidAction :: forall desc. (Show desc) => desc -> Int -> Bool -> P.ApEventSink -> IO ()
mkVoidAction path delay pass _sink =
  do
    C.threadDelay delay
    -- sink . User $ Log msg
    unless pass . error $ toS msg
 where
  msg = pass ? txt path <> " passed" $ txt path <> " failed"

mkAction :: forall hi desc. (Show desc) => desc -> Int -> Bool -> P.ApEventSink -> hi -> IO ()
mkAction path delay pass sink _in = mkVoidAction path delay pass sink

mkNodes :: [T.Template] -> [P.PreNode IO [] ()]
mkNodes = fmap mkNode
 where
  mkNode :: T.Template -> P.PreNode IO [] ()
  mkNode = \case
    T.Test
      { path
      , testItems
      } ->
        P.Test
          { config = tc
          , path
          , tests = mkTestItem <$> testItems
          }
    T.OnceBefore
      { path
      , delay
      , pass
      , subNodes
      } ->
        P.Before
          { path
          , frequency = Once
          , action = mkAction path delay pass
          , subNodes = mkNodes subNodes
          }
    T.OnceAfter
      { path
      , delay
      , pass
      , subNodes
      } ->
        P.After
          { path
          , frequency = Once
          , after = mkVoidAction path delay pass
          , subNodes' = mkNodes subNodes
          }
    T.OnceAround
      { path
      , delay
      , passSetup
      , passTeardown
      , subNodes
      } ->
        P.Around
          { path
          , frequency = Once
          , setup = mkAction path delay passSetup
          , teardown = mkAction path delay passTeardown
          , subNodes = mkNodes subNodes
          }
    T.ThreadBefore
      { path
      , delay
      , pass
      , subNodes
      } ->
        P.Before
          { path
          , frequency = Thread
          , action = mkAction path delay pass
          , subNodes = mkNodes subNodes
          }
    T.ThreadAfter
      { path
      , delay
      , pass
      , subNodes
      } ->
        P.After
          { path
          , frequency = Thread
          , after = mkVoidAction path delay pass
          , subNodes' = mkNodes subNodes
          }
    T.ThreadAround
      { path
      , delay
      , passSetup
      , passTeardown
      , subNodes
      } ->
        P.Around
          { path
          , frequency = Thread
          , setup = mkAction path delay passSetup
          , teardown = mkAction path delay passTeardown
          , subNodes = mkNodes subNodes
          }
    T.EachBefore
      { path
      , delay
      , pass
      , subNodes
      } ->
        P.Before
          { path
          , frequency = Each
          , action = mkAction path delay pass
          , subNodes = mkNodes subNodes
          }
    T.EachAfter
      { path
      , delay
      , pass
      , subNodes
      } ->
        P.After
          { path
          , frequency = Each
          , after = mkVoidAction path delay pass
          , subNodes' = mkNodes subNodes
          }
    T.EachAround
      { path
      , delay
      , passSetup
      , passTeardown
      , subNodes
      } ->
        P.Around
          { path
          , frequency = Each
          , setup = mkAction path delay passSetup
          , teardown = mkAction path delay passTeardown
          , subNodes = mkNodes subNodes
          }

data Template
  = OnceBefore
      { delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | OnceAfter
      { delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | OnceAround
      { delay :: Int
      , passSetup :: Bool
      , passTeardown :: Bool
      , subNodes :: [Template]
      }
  | ThreadBefore
      { delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | ThreadAfter
      { delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | ThreadAround
      { delay :: Int
      , passSetup :: Bool
      , passTeardown :: Bool
      , subNodes :: [Template]
      }
  | EachBefore
      { delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | EachAfter
      { delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | EachAround
      { delay :: Int
      , passSetup :: Bool
      , passTeardown :: Bool
      , subNodes :: [Template]
      }
  | Test
      { testItems :: [TestItem]
      }
  deriving (Show, Eq)

data TestItem = TestItem
  { delay :: Int
  , pass :: Bool
  }
  deriving (Show, Eq)

mkTestItem :: T.TestItem -> P.TestItem IO ()
mkTestItem T.TestItem{id, title, delay, pass} = P.TestItem id title (mkAction title delay pass)