module SuiteRuntimeTest where

import Core qualified
import DSL.Internal.ApEvent qualified as AE
import Data.Aeson (ToJSON)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import FullSuiteTestTemplate (ManySpec (..), Result (..), Spec (..))
import FullSuiteTestTemplate qualified as T
import Internal.RunTimeLogging (ExePath (..), parentPath, testLogControls, topPath)
import Internal.SuiteRuntime (ThreadCount (..), executeNodeList)
import Internal.ThreadEvent as TE (
  HookPos (..),
  Hz (..),
  SuiteEvent (..),
  ThreadEvent (..),
  ThreadId,
  getSuiteEvent,
  hasSuiteEvent,
  isEnd,
  isHook,
  isHookParentFailure,
  isStart,
  isTestEventOrTestParentFailure,
  onceHook,
  onceSuiteEvent,
  startSuiteEventLoc,
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
import UnliftIO.STM (TQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import Prelude hiding (All, id)

import Data.Hashable qualified as H
import System.Random qualified as R
import System.Random.Stateful qualified as RS
import Test.Falsify.Generator qualified as FG
import Test.Falsify.Predicate qualified as FP
import Test.Falsify.Range qualified as FR
import Test.Tasty.Falsify qualified as F

{-
 - each fail
 - thread fail
-}

--  todo :: simple random api / effect

-- $ > genPlay
genPlay :: IO ()
genPlay = do
  i <- RS.uniformM RS.globalStdGen :: IO Int
  pPrint i
  rg <- RS.randomRIO (1, 100)
  pPrint rg

defaultSeed :: Int
defaultSeed = 13579

-- $> unit_simple_pass
unit_simple_pass :: IO ()
unit_simple_pass = runTest defaultSeed 1 [onceAround Pass Pass [test [testItem Pass, testItem Fail]]]

-- $> unit_simple_fail
unit_simple_fail :: IO ()
unit_simple_fail = runTest defaultSeed 1 [onceAround Fail Pass [test [testItem Pass, testItem Fail]]]

-- $> unit_nested_pass_fail
unit_nested_pass_fail :: IO ()
unit_nested_pass_fail =
  runTest
    defaultSeed
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

allSpec :: Int -> Result -> ManySpec
allSpec delay rslt = All $ Spec delay rslt

-- $ > unit_nested_threaded_chk_thread_count
unit_nested_threaded_chk_thread_count :: IO ()
unit_nested_threaded_chk_thread_count =
  do
    let mxThrds = 10
        logging' = NoLog
    r <-
      execute
        logging'
        defaultSeed
        mxThrds
        [ OnceAround
            (Spec 1000 Pass)
            (Spec 0 Pass)
            [ ThreadAround
                (allSpec 0 Pass)
                (allSpec 0 Pass)
                [ EachAfter
                    (allSpec 50 Pass)
                    [ test
                        [ Spec 0 Pass
                        , Spec 1 Fail
                        ]
                    ]
                ]
            , ThreadAround
                (allSpec 100 Pass)
                (allSpec 0 Fail)
                [ ThreadAround
                    (allSpec 300 Pass)
                    (allSpec 300 Pass)
                    [ threadAround Pass Pass [eachAfter Pass [test [Spec 3000 Fail, Spec 1000 Pass]]]
                    , ThreadAround (All $ Spec 50 Pass) (allSpec 0 Pass) [eachAfter Fail [test [Spec 1000 Pass, Spec 1000 Fail]]]
                    , threadAround Fail Pass [eachAfter Pass [test [Spec 1000 Fail, Spec 1000 Pass]]]
                    , threadAround Pass Pass [EachBefore (allSpec 300 Fail) [test [Spec 1000 Fail, Spec 3000 Pass]]]
                    , eachAround Fail Pass [test [Spec 40 Fail, Spec 10 Pass]]
                    , eachBefore
                        Fail
                        [ test [Spec 300 Pass, Spec 10 Pass]
                        , EachAround
                            (allSpec 50 Pass)
                            (allSpec 0 Pass)
                            [ test
                                [ Spec 1000 Pass
                                , Spec 200 Pass
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    chkProperties r.expandedTemplate r.log
    chkThreadCount mxThrds r.log

{- each and once hooks will always run but thread hooks may be empty
   due to subitems being stolen by another thread. We need to ensure
   that empty thread TestTrees are not executed
-}

ppTxt :: (Show a) => a -> Text
ppTxt = toS . ppShow

-- $ > unit_empty_thread_around
unit_empty_thread_around :: IO ()
unit_empty_thread_around =
  do
    exe [threadAround Pass Pass []] >>= chkEmptyLog
    exe [threadBefore Pass []] >>= chkEmptyLog
    exe [threadAfter Pass []] >>= chkEmptyLog
    exe [threadAround Pass Pass [threadBefore Pass [threadAfter Pass []]]] >>= chkEmptyLog
    exe [threadAround Pass Pass [threadBefore Pass [threadAfter Pass [test [testItem Pass]]]]] >>= chkLogLength
 where
  exe = execute NoLog defaultSeed 1
  chkEmptyLog r = chkEq' ("Log should only have start and end log:\n" <> (ppTxt r.log)) 2 (length r.log)
  chkLogLength r = chkEq' ("Log length not as expected:\n" <> (ppTxt r.log)) 12 (length r.log)

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
    , chkTemplateAllFailsAndPassesLogged ts
    , chkTemplateSomeFailsAndPassesLogged ts
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

data Summary = Summary
  { fails :: Set (AE.Path, SuiteEvent)
  , parentFails :: Set (AE.Path, SuiteEvent)
  , passes :: Set (AE.Path, SuiteEvent)
  }
  deriving (Show)

data LogResult
  = Actual Result
  | ParentFailed
  deriving (Ord, Eq, Show)

data ResultInfo = ResultInfo
  { path :: AE.Path
  , suiteEvent :: SuiteEvent
  , result :: LogResult
  }
  deriving (Ord, Eq, Show)

-- a result accumulator function to be used across a logs grouped by thread
-- this will fail if run across logs due to duplicate thread hooks
logAccum :: Set ResultInfo -> LogItem -> Set ResultInfo
logAccum acc =
  \case
    End{} -> acc
    f@Failure{loc, suiteEvent} ->
      S.member (passResult loc suiteEvent) acc
        ? (S.insert (ResultInfo (topPath' loc) suiteEvent $ Actual Fail) $ S.delete (passResult loc suiteEvent) acc)
        $ error
        $ "Failure event not started or failed twice\n" <> ppTxt f
    pf@ParentFailure{loc, suiteEvent} ->
      eventExists loc suiteEvent acc
        ? (error $ "parent failure encountered for already started event\n" <> ppTxt pf)
        $ S.insert (ResultInfo (topPath' loc) suiteEvent ParentFailed) acc
    s@Start{loc, suiteEvent} ->
      eventExists loc suiteEvent acc
        ? (error $ "start found for already started event\n" <> ppTxt s)
        $ S.insert (passResult loc suiteEvent) acc
    StartExecution{} -> acc
    ApEvent{} -> acc
    EndExecution{} -> acc
 where
  eventExists :: ExePath -> SuiteEvent -> Set ResultInfo -> Bool
  eventExists loc suiteEvent set' = S.filter (\ri -> ri.suiteEvent == suiteEvent && ri.path == topPath' loc) set' /= S.empty
  passResult loc suiteEvent = ResultInfo (topPath' loc) suiteEvent $ Actual Pass
  topPath' p =
    topPath p
      & fromMaybe (error $ "Empty event path ~ bad template setup " <> txt p)

--  note this test will only work if there are enough delays in the template
--  and the template is big enough to ensure that the threads are used
chkThreadCount :: Int -> [LogItem] -> IO ()
chkThreadCount mxThrds evts =
  chkEq'
    "Thread count"
    (mxThrds + 1) -- main thread + number of threads specified
    (length $ threadIds evts)

-- TODO :: add test for threaded (Some failures) - ie. some failures in a thread
-- count should match number of result instance in log (allow for parent failures)
chkTemplateSomeFailsAndPassesLogged :: [T.Template] -> [LogItem] -> IO ()
chkTemplateSomeFailsAndPassesLogged ts lgs = pure ()

--  where
--   accSums :: [(AE.Path, [Result])] -> LogItem -> [(AE.Path, [Result])]
--   accSums acc = \case {}

--   threadedActuals = foldl' accSums [] <$> threadedLogs False lgs

--   expectedSomeResults = M.fromList $ (\ep -> (ep.path, someResults ep)) <$> filteredEvntPaths isSome ts

--   someResults :: T.EventPath -> [Result]
--   someResults ep =
--     ep.evntSpec
--       & \case
--         All (Spec _ _) -> error "this should not happen"
--         Some s -> (.result) <$> s

--   isSome :: ManySpec -> Bool
--   isSome =
--     \case
--       All (Spec _ _) -> False
--       Some _ -> True

chkTemplateAllFailsAndPassesLogged :: [T.Template] -> [LogItem] -> IO ()
chkTemplateAllFailsAndPassesLogged ts lgs =
  do
    -- existence checks are in another test. just checking results correspond to expected
    -- this test will need to be updated when non-deterministic thread templates results are added
    chkEq' "All expected failures should actually fail" S.empty (expectedFail S.\\ (actual.fails <> actual.parentFails))
    -- for threaded events that can occur more than once
    chkEq' "Nothing expected to fail should pass" S.empty (S.intersection expectedFail actual.passes)
    chkEq' "All expected passes should actually pass" S.empty (expectedPass S.\\ (actual.passes <> actual.parentFails))
    -- for threaded events that can occur more than once
    chkEq' "Nothing expected to pass should fail" S.empty (S.intersection expectedPass actual.fails)
 where
  expectedResults :: (ManySpec -> Bool) -> Set (AE.Path, SuiteEvent)
  expectedResults pred' = fromList $ (\ep -> (ep.path, ep.suiteEvent)) <$> filteredEvntPaths pred' ts

  expectedPass :: Set (AE.Path, SuiteEvent)
  expectedPass =
    expectedResults
      ( \case
          All (Spec _ Pass) -> True
          All (Spec _ Fail) -> False
          PassProb{} -> False
      )

  expectedFail :: Set (AE.Path, SuiteEvent)
  expectedFail =
    expectedResults
      ( \case
          All (Spec _ Pass) -> False
          All (Spec _ Fail) -> True
          PassProb{} -> False
      )

  actual :: Summary
  actual =
    foldl' aggregate (Summary S.empty S.empty S.empty) $ summarise <$> allResultInfo
   where
    allResultInfo = actuals <$> threadedLogs False lgs

    -- results :: (LogResult -> Bool) ->  Set (AE.Path, SuiteEvent)
    summarise :: Set ResultInfo -> Summary
    summarise ri =
      Summary
        { passes =
            results
              ( \case
                  Actual Pass -> True
                  _ -> False
              )
              ri
        , fails =
            results
              ( \case
                  Actual Fail -> True
                  _ -> False
              )
              ri
        , parentFails =
            results
              ( \case
                  ParentFailed -> True
                  _ -> False
              )
              ri
        }

    actuals :: [LogItem] -> Set ResultInfo
    actuals = foldl' logAccum S.empty

    results :: (LogResult -> Bool) -> Set ResultInfo -> Set (AE.Path, SuiteEvent)
    results prd rslts =
      S.map (\ri -> (ri.path, ri.suiteEvent)) $ S.filter (\ri -> prd ri.result) rslts

    aggregate :: Summary -> Summary -> Summary
    aggregate a1 a2 =
      Summary
        { passes = a1.passes <> a2.passes
        , fails = a1.fails <> a2.fails
        , parentFails = a1.parentFails <> a2.parentFails
        }

filteredEvntPaths :: (ManySpec -> Bool) -> [T.Template] -> [T.EventPath]
filteredEvntPaths pred' templates = filter (pred' . (.evntSpec)) (templates >>= T.eventPaths)

chkFailurePropagation :: [LogItem] -> IO ()
chkFailurePropagation lg =
  do
    traverse_ chkDiscreteFailsAreNotPropagated failTails
    traverse_ chkParentFailsPropagated failTails
 where
  failTails = snd $ failInfo lg

-- discrete events no child
-- parent events expect all chidren to fail
-- pall the way to last sibling including last sibling when setup / teardown

isChildless :: SuiteEvent -> Bool
isChildless = \case
  Hook _hz pos -> pos == After
  TE.Test{} -> True

data ChkState = ExpectParentFail | DoneChecking
  deriving (Show, Eq)

chkParentFailsPropagated :: FailInfo -> IO ()
chkParentFailsPropagated
  f@FailInfo
    { failStartTail
    , loc = failLoc
    , suiteEvent = failSuiteEvent
    } =
    unless (isChildless f.suiteEvent) $ do
      void $ foldlM validateEvent ExpectParentFail failStartTail
   where
    isFailChildLog :: LogItem -> Bool
    isFailChildLog li =
      let
        mSuitEvnt = getSuiteEvent li
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
                        <> (ppTxt s)
                        <> "  Parent Failure is:\n"
                        <> "    "
                        <> (ppTxt failLoc)
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
    } = when (isChildless f.suiteEvent) $ do
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
    chkEq' (message <> " for:\n" <> (ppTxt childPath)) expectedParentPath actualParentPath
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
    logSuiteEventPath l = T.SuiteEventPath <$> (startSuiteEventLoc l >>= topPath) <*> getSuiteEvent l
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
onceBefore = OnceBefore . Spec 0

onceAfter :: Result -> [Template] -> Template
onceAfter = OnceAfter . Spec 0

onceAround :: Result -> Result -> [Template] -> Template
onceAround suRslt tdRslt = OnceAround (Spec 0 suRslt) (Spec 0 tdRslt)

threadBefore :: Result -> [Template] -> Template
threadBefore r = ThreadBefore (All $ Spec 0 r)

threadAfter :: Result -> [Template] -> Template
threadAfter r = ThreadAfter (All $ Spec 0 r)

threadAround :: Result -> Result -> [Template] -> Template
threadAround suRslt tdRslt = ThreadAround (All $ Spec 0 suRslt) (All $ Spec 0 tdRslt)

eachBefore :: Result -> [Template] -> Template
eachBefore = EachBefore . allSpec 0

eachAfter :: Result -> [Template] -> Template
eachAfter = EachAfter . allSpec 0

eachAround :: Result -> Result -> [Template] -> Template
eachAround suRslt tdRslt = EachAround (allSpec 0 suRslt) (allSpec 0 tdRslt)

test :: [Spec] -> Template
test = SuiteRuntimeTest.Test

testItem :: Result -> Spec
testItem = Spec 0

data ExeResult = ExeResult
  { expandedTemplate :: [T.Template]
  , log :: [ThreadEvent ExePath AE.ApEvent]
  }

runTest :: Int -> Int -> [Template] -> IO ()
runTest = runTest' logging

data Logging = Logging | NoLog deriving (Show, Eq)

logging :: Logging
logging = NoLog

runTest' :: Logging -> Int -> Int -> [Template] -> IO ()
runTest' wantLog baseRandomSeed maxThreads templates = do
  r <- execute wantLog baseRandomSeed maxThreads templates
  chkProperties r.expandedTemplate r.log

execute :: Logging -> Int -> Int -> [Template] -> IO ExeResult
execute wantLog baseRandomSeed maxThreads templates = do
  let fullTs = setPaths "" templates
  lg <- exeTemplate wantLog baseRandomSeed (ThreadCount maxThreads) fullTs
  pure $ ExeResult fullTs lg

exeTemplate :: Logging -> Int -> ThreadCount -> [T.Template] -> IO [ThreadEvent ExePath AE.ApEvent]
exeTemplate wantLog baseRandomSeed maxThreads templates = do
  let wantLog' = wantLog == Logging
  (lc, logQ) <- testLogControls wantLog'
  when wantLog' $ do
    putStrLn ""
    pPrint templates
    putStrLn "========="
  nodes <- mkNodes baseRandomSeed maxThreads templates
  executeNodeList maxThreads lc nodes
  atomically $ q2List logQ

q2List :: TQueue a -> STM [a]
q2List qu = reverse <$> recurse [] qu
 where
  recurse :: [a] -> TQueue a -> STM [a]
  recurse l q =
    tryReadTQueue q
      >>= maybe (pure l) (\e -> recurse (e : l) q)

loadTQueue :: TQueue a -> [a] -> STM ()
loadTQueue q = traverse_ (writeTQueue q) . reverse

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
          , testItems = zip [0 ..] testItems <&> \(idx', spec) -> T.TestItem{title = newAdd <> " TestItem", id = idx', ..}
          }
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
todo - trace like with pretty printing
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

mkQueAction :: forall path. (Show path) => TQueue Spec -> path -> IO ()
mkQueAction q path =
  do
    s <- atomically $ tryReadTQueue q
    s
      & maybe
        (error $ "spec queue is empty - either the test template has been misconfigured or a thread hook is being called more than once in a thread (which should not happen) at path: " <> ppTxt path)
        (mkVoidAction path)

data ManyParams = ManyParams
  { baseSeed :: Int
  , subSeed :: Int
  , path :: Text
  , passPcnt :: Int8
  , minDelay :: Int
  , maxDelay :: Int
  }

mkManySpec :: ManyParams -> Spec
mkManySpec
  ManyParams
    { baseSeed
    , subSeed
    , path
    , passPcnt
    , minDelay
    , maxDelay
    } =
    Spec delay result
   where
    seed = H.hash $ txt baseSeed <> path <> txt subSeed
    delay = minDelay + (seed `mod` (maxDelay - minDelay))
    result = seed `mod` 100 < fromIntegral passPcnt ? Pass $ Fail

generateSpecs :: (Show pth) => Int -> Int -> pth -> Int8 -> Int -> Int -> [Spec]
generateSpecs baseSeed qLength pth passPcnt minDelay maxDelay =
  manySpec <$> [1 .. qLength]
 where
  manySpec :: Int -> Spec
  manySpec subSeed =
    mkManySpec
      ManyParams
        { baseSeed
        , subSeed
        , path = txt pth
        , passPcnt
        , minDelay
        , maxDelay
        }

loadQIfPregen :: (Show pth) => Int -> Int -> pth -> TQueue Spec -> ManySpec -> IO ()
loadQIfPregen baseSeed qLength pth q = \case
  All _ -> pure ()
  PassProb
    { preGenerate
    , passPcnt
    , minDelay
    , maxDelay
    } ->
      preGenerate
        ? (atomically . loadTQueue q $ generateSpecs baseSeed qLength pth passPcnt minDelay maxDelay)
        $ pure ()

-- do
--   s <- mkManySpec <$> manySpec
--   atomically $ writeTQueue q s
-- where
--   manySpec subSeed = mkManySpec ManyParams {
--         baseSeed,
--         subSeed,
--         path = toS pth,
--         passPcnt,
--         minDelay,
--         maxDelay
--       }

-- assumes th queue is preloaded if pregen is true see loadQIfPregen
mkManyAction :: forall pth. (Show pth) => Int -> TQueue Spec -> pth -> ManySpec -> IO ()
mkManyAction baseSeed q pth = \case
  All s -> mkVoidAction pth s
  PassProb
    { preGenerate
    , passPcnt
    , minDelay
    , maxDelay
    } ->
      preGenerate
        ? mkQueAction q pth
        $ do
          subSeed <- RS.uniformM RS.globalStdGen :: IO Int
          mkVoidAction pth $
            mkManySpec
              ManyParams
                { baseSeed
                , subSeed
                , path = txt pth
                , passPcnt
                , minDelay
                , maxDelay
                }

mkVoidAction :: forall pth. (Show pth) => pth -> Spec -> IO ()
mkVoidAction path spec =
  do
    C.threadDelay spec.delay
    unless (spec.result == Pass) $
      error . toS $
        txt path <> " failed"

-- TODO: make bug / error functions that uses text instead of string
-- TODO: check callstack
mkAction :: forall hi pth. (Show pth) => pth -> Spec -> P.ApEventSink -> hi -> IO ()
mkAction path s _sink _in = mkVoidAction path s

mkNodes :: Int -> ThreadCount -> [T.Template] -> IO [P.PreNode IO [] ()]
mkNodes baseRandomSeed mxThreads = sequence . fmap mkNode
 where
  afterAction :: (Show pth) => pth -> Spec -> b -> IO ()
  afterAction path spec = const $ mkVoidAction path spec

  mkNodes' = mkNodes baseRandomSeed mxThreads
  mkNode :: T.Template -> IO (P.PreNode IO [] ())
  mkNode t = case t of
    T.Test
      { path
      , testItems
      } ->
        pure $
          P.Test
            { config = tc
            , path
            , tests = mkTestItem <$> testItems
            }
    _ ->
      do
        nds <- mkNodes' $ t.subNodes
        b4Q <- newTQueueIO
        afterQ <- newTQueueIO
        let mxThrds = mxThreads.maxThreads
            tstItemCount = T.countTestItems t
        case t of
          T.OnceBefore
            { path
            , spec
            } ->
              pure $ do
                P.Before
                  { path
                  , frequency = Once
                  , action = mkAction path spec
                  , subNodes = nds
                  }
          T.OnceAfter
            { path
            , spec
            } ->
              pure $
                P.After
                  { path
                  , frequency = Once
                  , after = afterAction path spec
                  , subNodes' = nds
                  }
          T.OnceAround
            { path
            , setupSpec
            , teardownSpec
            } ->
              pure $
                P.Around
                  { path
                  , frequency = Once
                  , setup = mkAction path setupSpec
                  , teardown = mkAction path teardownSpec
                  , subNodes = nds
                  }
          T.EachBefore
            { path
            , eachSpec
            } ->
              do
                loadQIfPregen baseRandomSeed tstItemCount path b4Q eachSpec
                pure $
                  P.Before
                    { path
                    , frequency = Each
                    , action = const . const $ mkManyAction baseRandomSeed b4Q path eachSpec
                    , subNodes = nds
                    }
          T.EachAfter
            { path
            , eachSpec
            } ->
              do
                loadQIfPregen baseRandomSeed tstItemCount path afterQ eachSpec
                pure $
                  P.After
                    { path
                    , frequency = Each
                    , after = const $ mkManyAction baseRandomSeed b4Q path eachSpec
                    , subNodes' = nds
                    }
          T.EachAround
            { path
            , eachSetupSpec
            , eachTeardownSpec
            } ->
              do
                loadQIfPregen baseRandomSeed tstItemCount path b4Q eachSetupSpec
                loadQIfPregen baseRandomSeed tstItemCount path afterQ eachTeardownSpec
                pure $
                  P.Around
                    { path
                    , frequency = Each
                    , setup = const . const $ mkManyAction baseRandomSeed b4Q path eachSetupSpec
                    , teardown = const . const $ mkManyAction baseRandomSeed afterQ path eachTeardownSpec
                    , subNodes = nds
                    }
          _ -> case t of
            T.ThreadBefore
              { path
              , threadSpec
              } ->
                do
                  loadQIfPregen baseRandomSeed mxThrds path b4Q threadSpec
                  pure $
                    P.Before
                      { path
                      , frequency = Thread
                      , action = const . const $ mkManyAction baseRandomSeed b4Q path threadSpec
                      , subNodes = nds
                      }
            T.ThreadAfter
              { path
              , threadSpec
              } ->
                do
                  loadQIfPregen baseRandomSeed mxThrds path afterQ threadSpec
                  pure $
                    P.After
                      { path
                      , frequency = Each
                      , after = const $ mkManyAction baseRandomSeed b4Q path threadSpec
                      , subNodes' = nds
                      }
            T.ThreadAround
              { path
              , setupThreadSpec
              , teardownThreadSpec
              } -> do
                loadQIfPregen baseRandomSeed mxThrds path b4Q setupThreadSpec
                loadQIfPregen baseRandomSeed mxThrds path afterQ teardownThreadSpec
                pure $
                  P.Around
                    { path
                    , frequency = Each
                    , setup = const . const $ mkManyAction baseRandomSeed b4Q path setupThreadSpec
                    , teardown = const . const $ mkManyAction baseRandomSeed afterQ path teardownThreadSpec
                    , subNodes = nds
                    }

-- P.Around
--   { path
--   , frequency = Thread
--   , setup = mkB4ThrdAction setupThreadSpec path
--   , teardown = mkTeardownThrdAction teardownThreadSpec path
--   , subNodes = nds
--   }
data Template
  = OnceBefore
      { spec :: Spec
      , subNodes :: [Template]
      }
  | OnceAfter
      { spec :: Spec
      , subNodes :: [Template]
      }
  | OnceAround
      { setupSpec :: Spec
      , teardownSpec :: Spec
      , subNodes :: [Template]
      }
  | ThreadBefore
      { threadSpec :: ManySpec
      , subNodes :: [Template]
      }
  | ThreadAfter
      { threadSpec :: ManySpec
      , subNodes :: [Template]
      }
  | ThreadAround
      { setupThreadSpec :: ManySpec
      , teardownThreadSpec :: ManySpec
      , subNodes :: [Template]
      }
  | EachBefore
      { eachSpec :: ManySpec
      , subNodes :: [Template]
      }
  | EachAfter
      { eachSpec :: ManySpec
      , subNodes :: [Template]
      }
  | EachAround
      { eachSetupSpec :: ManySpec
      , eachTeardownSpec :: ManySpec
      , subNodes :: [Template]
      }
  | Test
      { testItems :: [Spec]
      }
  deriving (Show, Eq)

mkTestItem :: T.TestItem -> P.TestItem IO ()
mkTestItem T.TestItem{id, title, spec} = P.TestItem id title (mkAction title spec)
