module SuiteRuntimeTest where

import qualified Core
import DSL.Internal.ApEvent (ApEvent, Path (..))
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as M
import Data.Set (difference)
import qualified Data.Text as T
import FullSuiteTestTemplate (Result (..))
import qualified FullSuiteTestTemplate as T
import Internal.RunTimeLogging (ExePath, testLogControls, topPath)
import Internal.SuiteRuntime (ThreadCount (..), executeNodeList)
import Internal.ThreadEvent as TE (Hz (..), SuiteEvent (..), ThreadEvent (..), ThreadId, hasSuiteEvent, isStart, onceHook, onceSuiteEvent, isEnd, threadHook, isHook, isHookParentFailure)
import List.Extra as LE
import qualified List.Extra as L
import qualified Prepare as P
import PyrethrumExtras (toS, txt, uu, (?), debug)
import PyrethrumExtras.Test hiding (chkEq', filter, maybe, test)
import Text.Show.Pretty (pPrint, ppShow, ppShowList)
import UnliftIO.Concurrent as C (
  threadDelay,
 )
import UnliftIO.STM (TQueue, tryReadTQueue)
import Prelude hiding (id)

type LogItem = ThreadEvent ExePath DSL.Internal.ApEvent.ApEvent

chkProperties :: Int -> [T.Template] -> [LogItem] -> IO ()
chkProperties _mxThrds ts evts = do
  traverse_
    (evts &)
    [ chkStartEndExecution
    , chkThreadLogsInOrder
    , chkAllTemplateItemsLogged ts
    , chkStartsOnce "once hooks and tests" shouldOccurOnce
    , chkStartSuiteEventImmediatlyFollowedByEnd "once hooks" (hasSuiteEvent onceHook)
    , chkThreadHooksStartedOnceInThread
    , chkAllStartSuitEventsInThreadImmedialyFollowedByEnd
    -- preceeding parent events in thread + once
    -- subsequent parent events in thread + once
    -- setup followed by teardown in thread + once
    -- failure propagation
    ]
  putStrLn " checks done"

chkAllStartSuitEventsInThreadImmedialyFollowedByEnd :: [LogItem] -> IO ()
chkAllStartSuitEventsInThreadImmedialyFollowedByEnd = 
  traverse_ 
    (chkStartSuiteEventImmediatlyFollowedByEnd "thread suite elements" (hasSuiteEvent (const True))) 
    . threadVisibleSequences

chkThreadHooksStartedOnceInThread :: [LogItem] -> IO ()
chkThreadHooksStartedOnceInThread =
  traverse_
    (chkStartsOnce "thread elements" (hasSuiteEvent threadHook))
    . threadVisibleSequences

-- TODO:: reexport putStrLn et. al with text conversion

chkStartSuiteEventImmediatlyFollowedByEnd :: Text -> (LogItem -> Bool) -> [LogItem] -> IO ()
chkStartSuiteEventImmediatlyFollowedByEnd errSfx p l = do
  --  putStrLn $ ppShowList trgEvnts
  unless (null startNotFollwedByEnd) $
    fail $
      toS errSfx <> " start not followed by end:\n" <> toS (ppShow startNotFollwedByEnd)
 where
  trgEvnts = filter p l
  startNotFollwedByEnd = filter (\(s, e) -> isStart s && (not (isEnd e) || s.loc /= e.loc)) . zip trgEvnts $ drop 1 trgEvnts

chkStartsOnce :: Text -> (LogItem -> Bool) -> [LogItem] -> IO ()
chkStartsOnce errSfx p l = do
  --  putStrLn $ ppShowList trgEvnts
  unless (null dupLocs) $
    fail $
      toS errSfx <> ":\n" <> toS (ppShow dupLocs)
 where
  trgEvnts = filter p l
  starts = filter isStart trgEvnts
  dupLocs = filter ((> 1) . length) . fmap (L.head . fmap (.loc)) . groupOn' (.loc) $ starts

chkAllTemplateItemsLogged :: [T.Template] -> [LogItem] -> IO ()
chkAllTemplateItemsLogged ts lgs =
  unless (null errMissng || null errExtra) $
    fail (errMissng <> "\n" <> errExtra)
 where
  errMissng = null missing ? "" $ "template items not present in log:\n" <> toS (ppShow missing)
  errExtra = null extra ? "" $ "extra items in the log that are not in the template:\n" <> toS (ppShow extra)
  extra = difference logStartPaths tmplatePaths
  missing = difference tmplatePaths logStartPaths

  tmplatePaths :: Set Path
  tmplatePaths = fromList $ (.path) <$> (ts >>= T.eventPaths)

  logStartPaths :: Set Path
  logStartPaths =
    fromList $
      Prelude.mapMaybe
        ( \lg ->
            do
              xp <- case lg of
                ParentFailure{loc} -> Just loc
                Start{loc} -> Just loc
                _ -> Nothing
              topPath xp
        )
        lgs

nxtHookLog :: [LogItem] -> Maybe LogItem
nxtHookLog = find (\l -> hasSuiteEvent isHook l || isHookParentFailure l) 

threadVisible :: ThreadId -> [LogItem] -> [LogItem]
threadVisible tid =
  filter (\l -> tid == l.threadId || hasSuiteEvent onceHook l)

threadIds :: [LogItem] -> [ThreadId]
threadIds = nub . fmap (.threadId)

threadVisibleSequences :: [LogItem] -> [[LogItem]]
threadVisibleSequences l =
  (`threadVisible` l) <$> threadIds l

shouldOccurOnce :: LogItem -> Bool
shouldOccurOnce = hasSuiteEvent onceSuiteEvent

-- isTest :: ThreadEvent l a -> Bool
-- isTest = (==) Test . (.suiteEvent)

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
groupOn' :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn' f =
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
  threads = groupOn' (.threadId) evts
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

-- $ > unit_simple_pass
unit_simple_pass :: IO ()
unit_simple_pass = runTest False 1 [onceAround Pass Pass [test [testItem Pass, testItem Fail]]]

-- $ > unit_simple_fail
unit_simple_fail :: IO ()
unit_simple_fail = runTest False 1 [onceAround Fail Pass [test [testItem Pass, testItem Fail]]]

-- $> unit_nested_thread_pass_fail
unit_nested_thread_pass_fail :: IO ()
unit_nested_thread_pass_fail =
  runTest
    True
    1
    [ onceAround
        Pass
        Pass
        [ threadAround Pass Pass [eachAfter Fail [test [testItem Pass, testItem Fail]]]
        , threadAround Fail Pass [eachAfter Pass [test [testItem Fail, testItem Pass]]]
        ]
    ]

onceBefore :: Result -> [Template] -> Template
onceBefore = OnceBefore 0

onceAfter :: Result -> [Template] -> Template
onceAfter = OnceAfter 0

onceAround :: Result -> Result -> [Template] -> Template
onceAround = OnceAround 0

threadBefore :: Result -> [Template] -> Template
threadBefore = ThreadBefore 0

threadAfter :: Result -> [Template] -> Template
threadAfter = ThreadAfter 0

threadAround :: Result -> Result -> [Template] -> Template
threadAround = ThreadAround 0

eachBefore :: Result -> [Template] -> Template
eachBefore = EachBefore 0

eachAfter :: Result -> [Template] -> Template
eachAfter = EachAfter 0

eachAround :: Result -> Result -> [Template] -> Template
eachAround = EachAround 0

test :: [TestItem] -> Template
test = SuiteRuntimeTest.Test
 
testItem :: Result -> TestItem
testItem = TestItem 0

runTest :: Bool -> Int -> [Template] -> IO ()
runTest wantConsole maxThreads templates = do
  let fullTs = setPaths "" templates
  lg <- exeTemplate wantConsole (ThreadCount maxThreads) fullTs
  chkProperties maxThreads fullTs lg

exeTemplate :: Bool -> ThreadCount -> [T.Template] -> IO [ThreadEvent ExePath DSL.Internal.ApEvent.ApEvent]
exeTemplate wantConsole maxThreads templates = do
  (lc, logQ) <- testLogControls wantConsole
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

mkVoidAction :: forall desc. (Show desc) => desc -> Int -> Result -> P.ApEventSink -> IO ()
mkVoidAction path delay outcome _sink =
  do
    C.threadDelay delay
    unless (outcome == Pass) $
      error . toS $
        txt path <> " failed"

-- TODO: make bug / error functions that uses text instead of string
-- TODO: check callstack 

mkAction :: forall hi desc. (Show desc) => desc -> Int -> Result -> P.ApEventSink -> hi -> IO ()
mkAction path delay rslt sink _in = mkVoidAction path delay rslt sink

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
      , result
      , subNodes
      } ->
        P.Before
          { path
          , frequency = Once
          , action = mkAction path delay result
          , subNodes = mkNodes subNodes
          }
    T.OnceAfter
      { path
      , delay
      , result
      , subNodes
      } ->
        P.After
          { path
          , frequency = Once
          , after = mkVoidAction path delay result
          , subNodes' = mkNodes subNodes
          }
    T.OnceAround
      { path
      , delay
      , setupResult
      , teardownResult
      , subNodes
      } ->
        P.Around
          { path
          , frequency = Once
          , setup = mkAction path delay setupResult
          , teardown = mkAction path delay teardownResult
          , subNodes = mkNodes subNodes
          }
    T.ThreadBefore
      { path
      , delay
      , result
      , subNodes
      } ->
        P.Before
          { path
          , frequency = Thread
          , action = mkAction path delay result
          , subNodes = mkNodes subNodes
          }
    T.ThreadAfter
      { path
      , delay
      , result
      , subNodes
      } ->
        P.After
          { path
          , frequency = Thread
          , after = mkVoidAction path delay result
          , subNodes' = mkNodes subNodes
          }
    T.ThreadAround
      { path
      , delay
      , setupResult
      , teardownResult
      , subNodes
      } ->
        P.Around
          { path
          , frequency = Thread
          , setup = mkAction path delay setupResult
          , teardown = mkAction path delay teardownResult
          , subNodes = mkNodes subNodes
          }
    T.EachBefore
      { path
      , delay
      , result
      , subNodes
      } ->
        P.Before
          { path
          , frequency = Each
          , action = mkAction path delay result
          , subNodes = mkNodes subNodes
          }
    T.EachAfter
      { path
      , delay
      , result
      , subNodes
      } ->
        P.After
          { path
          , frequency = Each
          , after = mkVoidAction path delay result
          , subNodes' = mkNodes subNodes
          }
    T.EachAround
      { path
      , delay
      , setupResult
      , teardownResult
      , subNodes
      } ->
        P.Around
          { path
          , frequency = Each
          , setup = mkAction path delay setupResult
          , teardown = mkAction path delay teardownResult
          , subNodes = mkNodes subNodes
          }
data Template
  = OnceBefore
      { delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | OnceAfter
      { delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | OnceAround
      { delay :: Int
      , setupResult :: Result
      , teardownResult :: Result
      , subNodes :: [Template]
      }
  | ThreadBefore
      { delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | ThreadAfter
      { delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | ThreadAround
      { delay :: Int
      , setupResult :: Result
      , teardownResult :: Result
      , subNodes :: [Template]
      }
  | EachBefore
      { delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | EachAfter
      { delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | EachAround
      { delay :: Int
      , setupResult :: Result
      , teardownResult :: Result
      , subNodes :: [Template]
      }
  | Test
      { testItems :: [TestItem]
      }
  deriving (Show, Eq)

data TestItem = TestItem
  { delay :: Int
  , result :: Result
  }
  deriving (Show, Eq)

mkTestItem :: T.TestItem -> P.TestItem IO ()
mkTestItem T.TestItem{id, title, delay, result} = P.TestItem id title (mkAction title delay result)