module SuiteRuntimeTest where

import Core qualified
import DSL.Internal.ApEvent qualified as AE
import Data.Aeson (ToJSON)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import FullSuiteTestTemplate (Result (..))
import FullSuiteTestTemplate qualified as T
import Internal.RunTimeLogging (ExePath (..), parentPath, testLogControls, topPath)
import Internal.SuiteRuntime (ThreadCount (..), executeNodeList)
import Internal.ThreadEvent as TE (
  HookPos (..),
  Hz (..),
  SuiteEvent (..),
  ThreadEvent (..),
  ThreadId,
  hasSuiteEvent,
  isEnd,
  isHook,
  isHookParentFailure,
  isStart,
  isTestEventOrTestParentFailure,
  onceHook,
  onceSuiteEvent,
  startSuiteEventLoc,
  suiteEvent,
  suiteEventOrParentFailureSuiteEvent,
  threadHook,
 )

import List.Extra as LE
import List.Extra qualified as L
import Prepare qualified as P
import PyrethrumExtras (debug, debug', debug'_, toS, txt, uu, (?))
import PyrethrumExtras.Test hiding (chkEq', filter, mapMaybe, maybe, test)
import Text.Show.Pretty (pPrint, ppShow)
import UnliftIO.Concurrent as C (
  threadDelay,
 )
import UnliftIO.STM (TQueue, tryReadTQueue)
import Prelude hiding (id)

-- $ > unit_simple_pass
unit_simple_pass :: IO ()
unit_simple_pass = runTest 1 [onceAround Pass Pass [test [testItem Pass, testItem Fail]]]

-- $ > unit_simple_fail
unit_simple_fail :: IO ()
unit_simple_fail = runTest 1 [onceAround Fail Pass [test [testItem Pass, testItem Fail]]]

-- $ > unit_nested_pass_fail
unit_nested_pass_fail :: IO ()
unit_nested_pass_fail =
  runTest
    1
    [ onceAround
        Pass
        Pass
        [ threadAround Pass Pass [eachAfter Pass [test [testItem Fail, testItem Pass]]]
        , threadAround Pass Pass [eachAfter Fail [test [testItem Pass, testItem Fail]]]
        , threadAround Fail Pass [eachAfter Pass [test [testItem Fail, testItem Pass]]]
        , threadAround Pass Pass [eachBefore Fail [test [testItem Fail, testItem Pass]]]
        , eachAround Fail Pass [test [testItem Fail, testItem Pass]]
        , eachBefore
            Fail
            [ test [testItem Pass, testItem Pass]
            , eachAround
                Pass
                Pass
                [ test
                    [ testItem Pass
                    , testItem Pass
                    ]
                ]
            ]
        ]
    ]

-- $> unit_nested_threaded_pass_fail
unit_nested_threaded_pass_fail :: IO ()
unit_nested_threaded_pass_fail =
  do
    let mxThrds = 10
        logging' = True
    r <-
      execute
        logging'
        mxThrds
        [ OnceAround
            1000
            Pass
            Pass
            [ ThreadAround
                10
                Pass
                Pass
                [ EachAfter
                    50
                    Pass
                    [ test
                        [ TestItem 0 Pass
                        , TestItem 1 Fail
                        ]
                    ]
                ]
            , ThreadAround
                100
                Pass
                Fail
                [ ThreadAround
                    300
                    Pass
                    Pass
                    [ threadAround Pass Pass [eachAfter Pass [test [TestItem 3000 Fail, TestItem 1000 Pass]]]
                    , ThreadAround 50 Pass Pass [eachAfter Fail [test [TestItem 1000 Pass, TestItem 1000 Fail]]]
                    , threadAround Fail Pass [eachAfter Pass [test [TestItem 1000 Fail, TestItem 1000 Pass]]]
                    , threadAround Pass Pass [EachBefore 300 Fail [test [TestItem 1000 Fail, TestItem 3000 Pass]]]
                    , eachAround Fail Pass [test [TestItem 40 Fail, TestItem 10 Pass]]
                    , eachBefore
                        Fail
                        [ test [TestItem 300 Pass, TestItem 10 Pass]
                        , EachAround
                            50
                            Pass
                            Pass
                            [ test
                                [ TestItem 1000 Pass
                                , TestItem 200 Pass
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    chkProperties r.expandedTemplate r.log
    chkThreadCount mxThrds r.log

type LogItem = ThreadEvent ExePath AE.ApEvent

chkProperties :: [T.Template] -> [LogItem] -> IO ()
chkProperties ts evts = do
  -- these checks apply to the log as a whole
  traverse_
    (evts &)
    [ chkStartEndExecution
    , chkThreadLogsInOrder
    , chkAllTemplateItemsLogged ts
    , chkStartsOnce "once hooks and tests" shouldOccurOnce
    , chkStartSuiteEventImmediatlyFollowedByEnd "once hooks" (hasSuiteEvent onceHook)
    , chkTemplateAbsoluteFailsAndPassesLogged ts
    ]
  -- these checks apply to each thread log (ie. Once events + events with the same thread id)
  threadLogChks
    evts
    [ chkThreadHooksStartedOnceInThread
    , chkAllStartSuitEventsInThreadImmedialyFollowedByEnd
    , chkPrecedingSuiteEventAsExpected (T.expectedParentPrecedingEvents ts)
    , chkSubsequentSuiteEventAsExpected (T.expectedParentSubsequentEvents ts)
    , chkFailureLocEqualsLastStartLoc
    , chkFailurePropagation
    ]
  putStrLn " checks done"

data FailInfo = FailInfo
  { idx :: Int
  , threadId :: ThreadId
  , suiteEvent :: SuiteEvent
  , loc :: ExePath
  , failStartTail :: [LogItem]
  }
  deriving (Show)

-- TODO: logging options ~ only log failures - need a Pass summary log Object

data Acc = MkAcc
  { fails :: Set (AE.Path, SuiteEvent)
  , parentFails :: Set (AE.Path, SuiteEvent)
  , allEvents :: Set (AE.Path, SuiteEvent)
  , lastStarted :: Maybe SuiteEvent
  }
  deriving (Show)

emptyAcc :: Acc
emptyAcc = MkAcc S.empty S.empty S.empty Nothing

--  note this test will only work if there are enough delays in the template
--  and the template is big enough to ensure that the threads are used
chkThreadCount :: Int -> [LogItem] -> IO ()
chkThreadCount mxThrds evts =
  chkEq' 
    "Thread count" 
     (mxThrds + 1) -- main thread + number of threads specified
     (length $ threadIds evts)

-- needs some work
-- partial passes will be recorded as fail
chkTemplateAbsoluteFailsAndPassesLogged :: [T.Template] -> [LogItem] -> IO ()
chkTemplateAbsoluteFailsAndPassesLogged ts lgs =
  do
    -- existence checks are in another test. just checking results correspond to expected
    -- this test will need to be updated when non-deterministic thread templates results are added
    chkEq' "All expected failures should actually fail" S.empty (expectedFail S.\\ (summary.fails <> summary.parentFails))
    -- for threaded events that can occur more than once
    chkEq' "Nothing expected to fail should pass" S.empty (S.intersection expectedFail passes)
    chkEq' "All expected passes should actually pass" S.empty (expectedPass S.\\ (passes <> summary.parentFails))
    -- for threaded events that can occur more than once
    chkEq' "Nothing expected to pass should fail" S.empty (S.intersection expectedPass summary.fails)
 where
  actuals = foldl' logResults emptyAcc
  threadedActuals = actuals <$> threadedLogs False lgs
  passes = foldMap' (\acc -> acc.allEvents S.\\ acc.fails S.\\ acc.parentFails) threadedActuals

  summary :: Acc
  summary = foldl' aggregate emptyAcc $ actuals <$> threadedLogs False lgs

  aggregate :: Acc -> Acc -> Acc
  aggregate a1 a2 =
    MkAcc
      { fails = a1.fails <> a2.fails
      , parentFails = a1.parentFails <> a2.parentFails
      , allEvents = a1.allEvents <> a2.allEvents
      , lastStarted = Nothing
      }

  logResults :: Acc -> LogItem -> Acc
  logResults acc@MkAcc{fails, parentFails, allEvents, lastStarted} li =
    case li of
      End{} -> acc
      Failure{loc} ->
        let failEvnt = fromMaybe (error $ "Fail with no preceeding start: " <> (toS $ ppShow li)) lastStarted
         in acc
              { fails = S.insert (mkTpl loc failEvnt) fails
              }
      ParentFailure{loc, suiteEvent = se} ->
        acc
          { parentFails = S.insert (mkTpl loc se) parentFails
          , allEvents = S.insert (mkTpl loc se) allEvents
          }
      Start{loc, suiteEvent = se} ->
        acc
          { allEvents = S.insert (mkTpl loc se) allEvents
          , -- there is another check that failures match with last started
            lastStarted = Just se
          }
      StartExecution{} -> acc
      ApEvent{} -> acc
      EndExecution{} -> acc
   where
    mkTpl loc se =
      topPath loc
        & maybe
          (error $ "Empty path in: " <> (toS $ ppShow li))
          (\p -> (p, se))

  expectedResults :: Result -> Set (AE.Path, SuiteEvent)
  expectedResults r = fromList $ (\ep -> (ep.path, ep.suiteEvent)) <$> filter ((== r) . (.result)) (ts >>= T.eventPaths)

  expectedPass :: Set (AE.Path, SuiteEvent)
  expectedPass = expectedResults Pass

  expectedFail :: Set (AE.Path, SuiteEvent)
  expectedFail = expectedResults Fail

chkFailurePropagation :: [LogItem] -> IO ()
chkFailurePropagation lg =
  do
    traverse_ chkDiscreteFailsAreNotPropagated failTails
    traverse_ chkNonDiscreteFailsPropagated failTails
 where
  failTails = snd $ failInfo lg

-- discrete events no child
-- parent events expect all chidren to fail
-- pall the way to last sibling including last sibling when setup / teardown

isDiscrete :: SuiteEvent -> Bool
isDiscrete = \case
  Hook _hz pos -> pos == After
  TE.Test{} -> True

data ChkState = ExpectParentFail | DoneChecking
  deriving (Show, Eq)

chkNonDiscreteFailsPropagated :: FailInfo -> IO ()
chkNonDiscreteFailsPropagated
  f@FailInfo
    { failStartTail
    , loc = failLoc
    , suiteEvent = failSuiteEvent
    } =
    unless (isDiscrete f.suiteEvent) $ do
      void $ foldlM validateEvent ExpectParentFail failStartTail
   where
    isFailChildLog :: LogItem -> Bool
    isFailChildLog li =
      let
        mSuitEvnt = suiteEvent li
        thisEventisTeardown =
          mSuitEvnt & maybe
            False
            \case
              Hook _ Teardown -> True
              _ -> False
        failEventIsSetup =
          failSuiteEvent & \case
            Hook _ Setup -> True
            _ -> False
        {-
          if the fail event is a setup then it is morally a fail parent of a sibling teardown
          -- ie. if setup fails sibling teardown will not run
          baseHook . subHook . subsubHook setup
          ....
          ....
          baseHook . subHook . subsubHook teardown

          otherwise fails will propagate
        -}
        targetParent =
          failEventIsSetup
            && thisEventisTeardown
              ? (fromMaybe (ExePath []) (parentPath False failLoc)) -- can be sibling
            $ failLoc -- must be parent
       in
        isParentPath targetParent li.loc

    validateEvent :: ChkState -> LogItem -> IO ChkState
    validateEvent acc lgItm =
      let
        isFailChild = isFailChildLog lgItm
       in
        acc
          == DoneChecking
            ? pure DoneChecking
          $ lgItm
            & \case
              p@ParentFailure{} ->
                do
                  chkEq'
                    ( "Parent failure path is no a child path of failure path:\n"
                        <> "  Parent Failure is:\n"
                        <> "    "
                        <> toS (ppShow failLoc)
                        <> "  Child Failure is:\n"
                        <> "    "
                        <> toS (ppShow p)
                    )
                    True
                    isFailChild
                  pure ExpectParentFail
              f'@Failure{} ->
                -- TODO :: hide reinstate with test conversion
                fail $ "Failure when expect parent failure:\n" <> toS (ppShow f')
              s@Start{} ->
                do
                  -- TODO :: implement chkFalse'
                  -- TODO :: implement ppTxt
                  -- TODO :: chk' error mkessage prints to single line - chkEq' works properly
                  chkEq'
                    ( "This event should be a child failure:\n"
                        <> "  This Event is:\n"
                        <> "    "
                        <> (toS $ ppShow s)
                        <> "  Parent Failure is:\n"
                        <> "    "
                        <> (toS $ ppShow failLoc)
                    )
                    False
                    isFailChild
                  pure DoneChecking
              _ ->
                fail $
                  "Unexpected event in failStartTail - these events should have been filtered out:\n" <> toS (ppShow lgItm)

chkDiscreteFailsAreNotPropagated :: FailInfo -> IO ()
chkDiscreteFailsAreNotPropagated
  f@FailInfo
    { failStartTail
    } = when (isDiscrete f.suiteEvent) $ do
    whenJust
      (LE.head failStartTail)
      \case
        ParentFailure{} ->
          -- if a discrete item such as an after hook or test as failed the next item should not be
          -- a parent failure because discrete items can't be parents
          chkFail $ "Discrete failure propagated to next event:\n" <> toS (ppShow f.suiteEvent)
        _ -> pure ()

-- TODO :: REMOVE USER ERROR force to throw or reinterpret user error as failure or ...
-- captures
-- declares element details and has default plus bepoke validation
-- chkCapture - will log a soft exception and allow trace in place
-- property that includes assertions

failInfo :: [LogItem] -> (Maybe SuiteEvent, [FailInfo])
failInfo li =
  foldl' step (Nothing, []) $ tails failStarts
 where
  step :: (Maybe SuiteEvent, [FailInfo]) -> [LogItem] -> (Maybe SuiteEvent, [FailInfo])
  step (lastStartEvnt, result) =
    \case
      [] -> (lastStartEvnt, result)
      (l : ls) ->
        l & \case
          Start{suiteEvent = se} ->
            (Just se, result)
          f@Failure{idx, loc, threadId} ->
            lastStartEvnt
              & maybe
                (error $ "Failure encountered before start:\n" <> toS (ppShow f))
                ( \s ->
                    ( Nothing
                    , FailInfo
                        { idx
                        , threadId
                        , suiteEvent = s
                        , -- fail loc is loc of active event denoted by previous start
                          -- checked in chkFailureLocEqualsLastStartLoc
                          loc
                        , failStartTail = ls
                        }
                        : result
                    )
                )
          ParentFailure{} -> passThrough
          StartExecution{} -> passThrough
          EndExecution{} -> passThrough
          ApEvent{} -> passThrough
          End{} -> passThrough
   where
    passThrough = (lastStartEvnt, result)
  failStarts =
    filter
      ( \case
          Failure{} -> True
          ParentFailure{} -> True
          Start{} -> True
          _ -> False
      )
      li

-- TODO: do empty thread test case should not run anything (ie no thread events - cna happpen despite tree
-- shaking due to multiple threads)
chkFailureLocEqualsLastStartLoc :: [LogItem] -> IO ()
chkFailureLocEqualsLastStartLoc =
  void . foldl' step (pure Nothing)
 where
  step :: IO (Maybe ExePath) -> LogItem -> IO (Maybe ExePath)
  step mParentLoc li = do
    let newStart = startLoc li
    mpl <- mParentLoc
    newStart
      & maybe
        ( do
            chkParentFailureLoc mpl li
            mParentLoc
        )
        pure
      . Just

  startLoc :: LogItem -> Maybe ExePath
  startLoc l = isStart l ? startSuiteEventLoc l $ Nothing

  chkParentFailureLoc :: Maybe ExePath -> LogItem -> IO ()
  chkParentFailureLoc mParentLoc li =
    case li of
      ParentFailure{loc} -> chkEq' ("Parent failure loc for " <> toS (ppShow li)) mParentLoc (Just loc)
      _ -> pure ()

chkSubsequentSuiteEventAsExpected :: Map T.SuiteEventPath T.SuiteEventPath -> [LogItem] -> IO ()
chkSubsequentSuiteEventAsExpected =
  chkForMatchedParents
    "subsequent parent event"
    False -- do not reverse list so we are forward through subsequent events
    isAfterSuiteEvent

-- isAnyHookSuiteEvent

-- isAfterSuiteEvent -- I think this logic is wrong shoud be checking every event

chkPrecedingSuiteEventAsExpected :: Map T.SuiteEventPath T.SuiteEventPath -> [LogItem] -> IO ()
chkPrecedingSuiteEventAsExpected =
  chkForMatchedParents
    "preceding parent event"
    True -- reverse list so we are searching back through preceding events
    isBeforeSuiteEvent

chkForMatchedParents :: Text -> Bool -> (LogItem -> Bool) -> Map T.SuiteEventPath T.SuiteEventPath -> [LogItem] -> IO ()
chkForMatchedParents message wantReverseLog parentEventPredicate expectedChildParentMap thrdLog =
  traverse_ chkParent actualParents
 where
  chkParent :: (T.SuiteEventPath, Maybe T.SuiteEventPath) -> IO ()
  chkParent (childPath, actualParentPath) =
    chkEq' (message <> " for:\n" <> (toS $ ppShow childPath)) expectedParentPath actualParentPath
   where
    expectedParentPath = M.lookup childPath expectedChildParentMap

  actualParents :: [(T.SuiteEventPath, Maybe T.SuiteEventPath)]
  actualParents = mapMaybe extractChildParent actualParent

  actualParent :: [[LogItem]]
  actualParent =
    tails . (\l -> wantReverseLog ? reverse l $ l) $
      thrdLog

  extractChildParent :: [LogItem] -> Maybe (T.SuiteEventPath, Maybe T.SuiteEventPath)
  extractChildParent evntLog =
    (,actulaParentPath) <$> targetPath
   where
    logSuiteEventPath :: LogItem -> Maybe T.SuiteEventPath
    logSuiteEventPath l = T.SuiteEventPath <$> (startSuiteEventLoc l >>= topPath) <*> suiteEvent l
    targEvnt = L.head evntLog
    targetPath = targEvnt >>= logSuiteEventPath
    actulaParentPath = do
      h <- targEvnt
      t <- L.tail evntLog -- all preceding / successive events
      fps <- findMathcingParent parentEventPredicate h t
      logSuiteEventPath fps

chkAllStartSuitEventsInThreadImmedialyFollowedByEnd :: [LogItem] -> IO ()
chkAllStartSuitEventsInThreadImmedialyFollowedByEnd =
  chkStartSuiteEventImmediatlyFollowedByEnd "thread suite elements" (hasSuiteEvent (const True))

threadLogChks :: [LogItem] -> [[LogItem] -> IO ()] -> IO ()
threadLogChks fullLog = traverse_ chkTls
 where
  tlgs = threadedLogs True fullLog
  chkTls = checkThreadLogs tlgs
  checkThreadLogs :: [[LogItem]] -> ([LogItem] -> IO ()) -> IO ()
  checkThreadLogs tls' lgChk = traverse_ lgChk tls'

chkThreadHooksStartedOnceInThread :: [LogItem] -> IO ()
chkThreadHooksStartedOnceInThread =
  chkStartsOnce "thread elements" (hasSuiteEvent threadHook)

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
  extra = S.difference logStartPaths tmplatePaths
  missing = S.difference tmplatePaths logStartPaths

  -- init to empty set
  tmplatePaths :: Set AE.Path
  tmplatePaths = fromList $ (.path) <$> (ts >>= T.eventPaths)

  logStartPaths :: Set AE.Path
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

{-
 TODO: when implementing log parsing need a threadView which includes all thread events
 and all parent OnceEvents - should probably log OnceEvents in a separate log
 as well as main log to so don't have to read whole log for once events

 same goes for filter log
-}

threadVisible :: Bool -> ThreadId -> [LogItem] -> [LogItem]
threadVisible onceHookInclude tid =
  filter (\l -> tid == l.threadId || onceHookInclude && hasSuiteEvent onceHook l)

threadIds :: [LogItem] -> [ThreadId]
threadIds = nub . fmap (.threadId)

threadedLogs :: Bool -> [LogItem] -> [[LogItem]]
threadedLogs onceHookInclude l =
  (\tid -> threadVisible onceHookInclude tid l) <$> threadIds l

shouldOccurOnce :: LogItem -> Bool
shouldOccurOnce = hasSuiteEvent onceSuiteEvent

chkStartEndExecution :: [ThreadEvent ExePath AE.ApEvent] -> IO ()
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
-- convert to to non-empty and use groupby from relude??
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

logging :: Bool
logging = False

data ExeResult = ExeResult
  { expandedTemplate :: [T.Template]
  , log :: [ThreadEvent ExePath AE.ApEvent]
  }

runTest :: Int -> [Template] -> IO ()
runTest = runTest' logging

runTest' :: Bool -> Int -> [Template] -> IO ()
runTest' wantConsole maxThreads templates = do
  r <- execute wantConsole maxThreads templates
  chkProperties r.expandedTemplate r.log

execute :: Bool -> Int -> [Template] -> IO ExeResult
execute wantConsole maxThreads templates = do
  let fullTs = setPaths "" templates
  lg <- exeTemplate wantConsole (ThreadCount maxThreads) fullTs
  pure $ ExeResult fullTs lg

exeTemplate :: Bool -> ThreadCount -> [T.Template] -> IO [ThreadEvent ExePath AE.ApEvent]
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
    newPath = AE.SuiteElmPath newAdd
    newAdd = nxtAdd idx
    newNodes = setPaths newAdd tp.subNodes

{-
todo - trace like
  db == debug'
  dbNoLabel
  dbCondional
  dbCondionalNoLabel
-}

findMathcingParent :: (LogItem -> Bool) -> LogItem -> [LogItem] -> Maybe LogItem
findMathcingParent evntPredicate targEvnt =
  find (fromMaybe False . matchesParentPath)
 where
  targEvntSubPath =
    startSuiteEventLoc targEvnt >>= parentPath (isTestEventOrTestParentFailure targEvnt)
  matchesParentPath :: LogItem -> Maybe Bool
  matchesParentPath thisEvt = do
    targPath <- targEvntSubPath
    thisParentCandidate <- startSuiteEventLoc thisEvt
    pure $ evntPredicate thisEvt && (thisParentCandidate.un) `isSuffixOf` (targPath.un)

isParentPath :: ExePath -> ExePath -> Bool
isParentPath (ExePath parent) (ExePath child) =
  LE.tail child & maybe False (parent `isSuffixOf`)

eventMatchesHookPos :: [HookPos] -> LogItem -> Bool
eventMatchesHookPos hookPoses lg =
  suiteEventOrParentFailureSuiteEvent lg
    & maybe
      False
      ( \case
          -- TODO: sort out imports see LE.elem
          Hook _frq pos -> pos `LE.elem` hookPoses
          TE.Test -> False
      )

isBeforeSuiteEvent :: LogItem -> Bool
isBeforeSuiteEvent = eventMatchesHookPos [Before, Setup]

isAnyHookSuiteEvent :: LogItem -> Bool
isAnyHookSuiteEvent = eventMatchesHookPos [After, Teardown, Before, Setup]

isAfterSuiteEvent :: LogItem -> Bool
isAfterSuiteEvent = eventMatchesHookPos [After, Teardown]

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
