module SuiteRuntimeTestBase where

-- TODO review PyrethrumExtras.Test remove hedgehog in favour of falsify
-- don't use chkFail it does not format properly

import Chronos (Time, now)
import Core (DataSource (ItemList))
import Core qualified
import CoreUtils (Hz (..), ThreadId)
import DSL.Internal.NodeLog qualified as AE
import Data.Aeson (ToJSON)
import Data.Hashable qualified as H
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import FullSuiteTestTemplate (Directive, EventPath (..), NSpec (PassProb), Spec (..), SpecGen (..), isPreload)
import FullSuiteTestTemplate qualified as T
import Internal.LogQueries
  ( getHookInfo,
    getSuiteEvent,
    isChildless,
    isEnd,
    isHook,
    isHookParentFailure,
    isOnceHookParentFailure,
    isStart,
    isTestEventOrTestParentFailure,
    onceHook,
    onceSuiteEvent,
    startEndNodeMatch,
    startOrParentFailure,
    startSuiteEventLoc,
    suitEvntToBool,
    suiteEventOrParentFailureSuiteEvent,
    threadEventToBool,
    threadHook,
  )
import Internal.Logging as L
  ( ExePath (..),
    FLog,
    FullLog (..),
    HookPos (..),
    LineInfo (..),
    Log (..),
    NodeType (..),
    parentPath,
    testLogActions,
    topPath,
  )
import Internal.SuiteRuntime (ThreadCount (..), executeWithoutValidation)
import Prepare qualified as P
import PyrethrumExtras (ConvertString, onError, toS, txt, (?), db)
import PyrethrumExtras qualified as PE
import PyrethrumExtras.Test (chk')
import System.Random.Stateful qualified as RS
import Text.Show.Pretty (pPrint, ppShow)
import UnliftIO.Concurrent as C
  ( threadDelay,
  )
import UnliftIO.STM (TQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import Prelude hiding (All, bug, id)
import Prelude qualified as PR

defaultSeed :: Int
defaultSeed = 13579

putTxt :: (ConvertString a String) => a -> IO ()
putTxt = putStrLn . toS

printTime :: Text -> Time -> IO ()
printTime msg t = putTxt $ msg <> ":: " <> toS (show t)

printNow :: Text -> IO ()
printNow lbl = do
  t <- now
  printTime lbl t

-- TODO : change list items to data nad add a constructor in preparation for other kinds of tests (eg. property tests)
-- TODO: other collection types generator / shrinker

-- todo :: add repeatedly to Pyrelude
{-
-- https://github.com/well-typed/unfolder/blob/main/episode019-repeatedly/app/Main.hs

-- | Generalization to 'Foldable'
repeatedly :: Foldable t => (a -> b -> b) -> (t a -> b -> b)
repeatedly = flip . foldl' . flip

-- make foldable not array
repeatedlyM1 :: Monad m => (a -> b -> m b) -> ([a] -> b -> m b)
repeatedlyM1 _ []     !b = return b
repeatedlyM1 f (a:as) !b = do b' <- f a b
                              repeatedlyM1 f as b'

-}
-- todo :: remap in Pyrelude bug' ~ use exception // bug ~ use text
-- bug :: Text -> c
bug :: Text -> a
bug t = PR.bug (error t :: SomeException)

logging :: Logging
logging = LogFails

{- each and once hooks will always run but thread hooks may be empty
   due to subitems being stolen by another thread. We need to ensure
   that empty thread TestTrees are not executed
-}

allSpec :: Int -> Directive -> NSpec
allSpec delay rslt = T.All $ Spec delay rslt

{-
  todo:

  find out about: +optimise-heavily -f +enable-cluster-counting
  and other compile options

  1. Getting the release candidate

       $ cabal get
https://hackage.haskell.org/package/Agda-2.6.4.3/Agda-2.6.4.3.tar.gz
       $ cd Agda-2.6.4.3

2. a. Using cabal-install

       $ cabal install -f +optimise-heavily -f +enable-cluster-counting
  -}

type LogItem = FLog ExePath AE.NodeLog

getThreadId :: LogItem -> ThreadId
getThreadId (MkLog (MkLineInfo {threadId}) _) = threadId

logItemtoBool :: (NodeType -> Bool) -> LogItem -> Bool
logItemtoBool = threadEventToBool

chkProperties :: Int -> ThreadCount -> [T.Template] -> [LogItem] -> IO ()
chkProperties baseSeed threadLimit ts evts = do
  chkNoEmptyHooks evts
  -- these checks apply to the log as a whole
  traverse_
    (evts &)
    [ chkStartEndExecution,
      chkThreadLogsInOrder,
      chkAllTemplateItemsLogged ts,
      chkStartsOnce "once hooks and tests" shouldOccurOnce,
      chkResults baseSeed threadLimit ts
    ]
  -- these checks apply to each thread log (events with the same thread id)
  threadLogChks
    False
    evts
    [ chkThreadHooksStartedOnceInThread,
      chkAllStartSuitEventsInThreadImmedialyFollowedByEnd
    ]
  -- these checks apply to each thread log (ie. Once events + events with the same thread id)
  threadLogChks
    True
    evts
    [ chkPrecedingSuiteEventAsExpected (T.expectedParentPrecedingEvents ts),
      chkAfterTeardownParents (T.expectedParentSubsequentEvents ts),
      chkFailureLocEqualsLastStartLoc,
      chkFailurePropagation
    ]

chkNoEmptyHooks :: [LogItem] -> IO ()
chkNoEmptyHooks evts = do
  -- these checks apply to the log as a whole
  traverse_
    (evts &)
    [ chkNoEmptyPreHooks [Once],
      chkNoEmptyPostHooks [Once]
    ]
  -- these checks apply to each thread log (events with the same thread id)
  threadLogChks
    False
    evts
    [ chkNoEmptyPreHooks [Thread, Each],
      chkNoEmptyPostHooks [Thread, Each]
    ]

data FailInfo = FailInfo
  { --   idx :: Int
    -- , threadId :: ThreadId
    -- , nodeType :: NodeType
    -- , loc :: ExePath
    -- ,
    failLog :: LogItem,
    failStartTail :: [LogItem]
  }
  deriving (Show)

-- TODO: logging options ~ only log failures - need a Pass summary log Object

data Summary = Summary
  { fails :: Set (AE.Path, NodeType),
    parentFails :: Set (AE.Path, NodeType),
    passes :: Set (AE.Path, NodeType)
  }
  deriving (Show)

data LogResult
  = Pass
  | Fail
  | ParentFailed
  deriving (Ord, Eq, Show)

data ResultInfo = ResultInfo
  { path :: AE.Path,
    nodeType :: NodeType,
    result :: LogResult
  }
  deriving (Ord, Eq, Show)

-- a result accumulator function to be used across a logs grouped by thread
actualResults :: [LogItem] -> Map EventPath [LogResult]
actualResults = snd . foldl' logAccum (Nothing, M.empty)

-- a result accumulator function to be used across a logs grouped by thread
logAccum :: (Maybe (ExePath, NodeType), Map EventPath [LogResult]) -> LogItem -> (Maybe (ExePath, NodeType), Map EventPath [LogResult])
logAccum acc@(passStart, rMap) (MkLog {event}) =
  event & \case
    End {loc, nodeType} ->
      passStart
        & maybe
          acc
          (\(_l, _s) -> (Nothing, insert' loc nodeType Pass))
    f@Failure {loc, nodeType} ->
      isJust passStart
        ? (Nothing, insert' loc nodeType Fail)
        $ error ("Failure event not started\n" <> txt f)
    -- TODO this is probably wrong fix this will cause failures when tests generate these
    -- need to think about cortrect expected results in this case
    InitialisationFailure {} -> acc
    pf@ParentFailure {loc, nodeType} ->
      isJust passStart
        ? error ("parent failure encountered when parent event not ended\n" <> txt pf)
        $ (Nothing, insert' loc nodeType ParentFailed)
    s@Start {loc, nodeType} ->
      isJust passStart
        ? error ("start found for already started event\n" <> txt s)
        $ (Just (loc, nodeType), rMap)
    FilterLog {} -> acc
    SuiteInitFailure {} -> acc
    StartExecution {} -> acc
    NodeLog {} -> acc
    EndExecution {} -> acc
  where
    insert' :: ExePath -> NodeType -> LogResult -> Map EventPath [LogResult]
    insert' l se r = M.insertWith (<>) (MkEventPath (topPath' l) se) [r] rMap
    topPath' p =
      fromMaybe (bug $ "Empty event path ~ bad template setup " <> txt p) $ topPath p

--  note this fixture will only work if there are enough delays in the template
--  and the template is big enough to ensure that the threads are used
chkThreadCount :: ThreadCount -> [LogItem] -> IO ()
chkThreadCount threadLimit evts =
  chkEq'
    "Thread count"
    (threadLimit.maxThreads + 1) -- main thread + number of threads specified
    (length $ threadIds evts)

data ExpectedResult
  = All LogResult
  | NonDeterministic
  | NResult [LogResult]
  deriving (Show, Eq)

-- todo add to pyrelude
groupCount :: (Ord a) => [a] -> M.Map a Int
groupCount = M.fromListWith (+) . fmap (,1)

{-
defects found in testing::
 - unit testing
  - incorrect label on thread hook - labeld as each
  - hook events out of order - due to bracket and laziness
  - missing thread around events
    - chkResults failing but not chkAllTemplateItemsLogged
    - chkAllTemplateItemsLogged was incomplete + prenode generator (fixture) bug

 - property testing
  - test function error causing tests to fail even though suite results were correct
    specifically - mkManyAction implementations flipped for construcots
    - T.All s -> had implementation meant for PassProb
    - PassProb -> had implementation meant for All
    - https://github.com/theGhostJW/pyrethrum/commit/ef50961

  - framework error - nested each after hooks executed out of order
-}
resultsEqual :: Directive -> LogResult -> Bool
resultsEqual expt act =
  expt == T.Pass && act == Pass || expt == T.Fail && act == Fail

chkResults :: Int -> ThreadCount -> [T.Template] -> [LogItem] -> IO ()
chkResults baseSeed threadLimit ts lgs =
  do
    -- fail missing expected results or different to expected
    chkResults'
    -- fail extra results
    let extrActual = M.keysSet actuals S.\\ M.keysSet (db "Expected" $ expectedResults)
    chkEq' "Extra results found in actual that are not expected" S.empty extrActual
  where
    chkResults' :: IO ()
    chkResults' = traverse_ chkResult $ M.toList expectedResults
      where
        chkResult :: (EventPath, ExpectedResult) -> IO ()
        chkResult (k, expected) =
          M.lookup k actuals
            & maybe
              --  todo: this doesn't format as expected
              (fail $ "Expected result for " <> ppShow k <> " not found in actual")
              ( \actual ->
                  case expected of
                    All e ->
                      chk' ("Unexpected result for:\n " <> txt k <> "\n   expected: " <> txt expected) $
                        all (\r -> r == e || r == ParentFailed) actual
                    NonDeterministic -> pure ()
                    NResult expLst -> case k.nodeType of
                      Test {} -> bug "Test not expected to have NResult result"
                      Hook Once _ -> bug "Once not expected to have NResult result"
                      Hook Thread _ -> do
                        chk'
                          ("actual thread events: " <> txt actualCount <> " more than max threads: " <> txt threadLimit.maxThreads)
                          $ actualCount <= threadLimit.maxThreads
                        countChks $ take actualCount expLst
                      Hook Each _ -> countChks expLst
                      where
                        failMsg law = "Property failed for:\n  " <> txt k <> "\n  " <> law
                        countExpected r rList = M.findWithDefault 0 r $ groupCount rList
                        expectedPasses = countExpected Pass
                        expectedFails = countExpected Fail
                        -- TODO: an infix high precedence operator for debugging
                        actualCount = length actual
                        actuals' = groupCount actual
                        actualPasses = M.findWithDefault 0 Pass actuals'
                        actualFails = M.findWithDefault 0 Fail actuals'
                        actualParentFails = M.findWithDefault 0 ParentFailed actuals'
                        countChks lstExpected = do
                          let expectedPassCount = expectedPasses lstExpected
                              expectedFailCount = expectedFails lstExpected
                          chk'
                            (failMsg "Pass Count: expectedPassCount: " <> txt expectedPassCount <> " <= actualPasses: " <> txt actualPasses <> " + actualParentFails: " <> txt actualParentFails)
                            (expectedPassCount <= actualPasses + actualParentFails)
                          chk'
                            (failMsg "Pass Count: expectedPassCount: " <> txt expectedPassCount <> " >= actualPasses: " <> txt actualPasses)
                            (expectedPassCount >= actualPasses)
                          chk'
                            (failMsg "Fail Count: expectedFailCount: " <> txt expectedPassCount <> " <= actualPasses: " <> txt actualPasses <> " + actualParentFails: " <> txt actualParentFails)
                            (expectedFailCount <= actualFails + actualParentFails)
                          chk'
                            (failMsg "Fail Count: expectedFailCount: " <> txt expectedFailCount <> " >= actualFails: " <> txt actualFails)
                            (expectedFailCount >= actualFails)
              )

    actuals :: Map EventPath [LogResult]
    actuals =
      foldl' (M.unionWith (<>)) M.empty allResults
      where
        allResults = actualResults <$> threadedLogs False lgs

    expectedResults :: Map EventPath ExpectedResult
    expectedResults =
      foldl' M.union M.empty $ (.resultMap) . expectedResultsRecursive' initAccum <$> ts
      where
        initAccum = Accum False (All Pass) M.empty

    expectedResultsRecursive' :: ResultAccum -> T.Template -> ResultAccum
    expectedResultsRecursive' accum = 
      let 
        thisResult = expectedResults' accum
      in 
        \case 
         T.OnceBefore _ _ _
         T.OnceAfter _ _ _
         T.OnceAround _ _ _ _
         T.ThreadBefore _ _ _

    expectedResults' :: ResultAccum -> T.Template -> ResultAccum
    expectedResults' Accum {poisoned, parentResult, resultMap} template =
      case template of
        T.OnceBefore {spec} ->
          let (nxtPoisoned, nxtParentResult) = singleSpecToExpected spec
              nxtMap = M.insert (MkEventPath template.path (Hook Once Before)) nxtParentResult resultMap
           in Accum nxtPoisoned nxtParentResult nxtMap
        T.OnceAfter {spec} ->
          let (_nxtPoisoned, nxtParentResult) = singleSpecToExpected spec
              nxtMap = M.insert (MkEventPath template.path (Hook Once After)) nxtParentResult resultMap
           in -- because happens after we just pass through poisoned parentResult
              Accum poisoned parentResult nxtMap
        T.OnceAround {setupSpec, teardownSpec} ->
          let -- create nxt result from setup
              (nxtPoisoned, nxtParentResult) = singleSpecToExpected setupSpec
              nxtMap' = M.insert (MkEventPath template.path $ Hook Once Setup) nxtParentResult resultMap
              -- teardown depends on result of setup
              (_tdPoisoned, tdResult) = specToExpected baseSeed 1 (nxtPoisoned, nxtParentResult) $ T.All teardownSpec
              nxtMap = M.insert (MkEventPath template.path $ Hook Once Teardown) tdResult nxtMap'
           in Accum nxtPoisoned nxtParentResult nxtMap
        T.ThreadBefore {threadSpec} ->
          let (nxtPoisoned, nxtParentResult) = specToExpected' threadSpec
              nxtMap = M.insert (MkEventPath template.path (Hook Thread Before)) nxtParentResult resultMap
           in Accum nxtPoisoned nxtParentResult nxtMap
        T.ThreadAfter {threadSpec} ->
          let (_nxtPoisoned, nxtParentResult) = specToExpected' threadSpec
              nxtMap = M.insert (MkEventPath template.path (Hook Thread After)) nxtParentResult resultMap
           in -- because happens after we just pass through poisoned parentResult
              Accum poisoned parentResult nxtMap
        T.ThreadAround {setupThreadSpec, teardownThreadSpec} ->
          let -- create nxt result from setup
              (nxtPoisoned, nxtParentResult) = specToExpected' setupThreadSpec
              nxtMap' = M.insert (MkEventPath template.path $ Hook Once Setup) nxtParentResult resultMap
              -- teardown depends on result of setup
              (_tdPoisoned, tdResult) = specToExpected baseSeed instanceCount (nxtPoisoned, nxtParentResult) teardownThreadSpec
              nxtMap = M.insert (MkEventPath template.path $ Hook Once Teardown) tdResult nxtMap'
           in Accum nxtPoisoned nxtParentResult nxtMap
        T.EachBefore {eachSpec} ->
          let (nxtPoisoned, nxtParentResult) = specToExpected' eachSpec
              nxtMap = M.insert (MkEventPath template.path (Hook Each Before)) nxtParentResult resultMap
           in Accum nxtPoisoned nxtParentResult nxtMap
        T.EachAfter {eachSpec} ->
          let (_nxtPoisoned, nxtParentResult) = specToExpected' eachSpec
              nxtMap = M.insert (MkEventPath template.path (Hook Each After)) nxtParentResult resultMap
           in -- because happens after we just pass through poisoned parentResult
              Accum poisoned parentResult nxtMap
        T.EachAround {eachSetupSpec, eachTeardownSpec} ->
          let -- create nxt result from setup
              (nxtPoisoned, nxtParentResult) = specToExpected' eachSetupSpec
              nxtMap' = M.insert (MkEventPath template.path $ Hook Each Setup) nxtParentResult resultMap
              -- teardown depends on result of setup
              (_tdPoisoned, tdResult) = specToExpected baseSeed instanceCount (nxtPoisoned, nxtParentResult) eachTeardownSpec
              nxtMap = M.insert (MkEventPath template.path $ Hook Each Teardown) tdResult nxtMap'
           in Accum nxtPoisoned nxtParentResult nxtMap
        T.Fixture {tests} ->
          let addTest m ti@T.TestItem {spec} =
                let (_poisend, rslt) = singleSpecToExpected spec
                 in M.insert (MkEventPath (T.testItemPath ti) Test) rslt m
              -- assuming fixture templates are non-empty
              nxtMap = foldl' addTest resultMap tests
           in -- at te end of the branch here so just pass on poisoned and parentResult
              -- they wont be used anyway
              Accum poisoned parentResult nxtMap
      where
        thrdCount = threadLimit.maxThreads
        tstCount = T.countTests template
        instanceCount =
          case template of
            -- once hooks produce one result
            T.OnceBefore {} -> 1
            T.OnceAfter {} -> 1
            T.OnceAround {} -> 1
            -- thread hooks will need n results up to the max number of threads
            T.ThreadBefore {} -> thrdCount
            T.ThreadAfter {} -> thrdCount
            T.ThreadAround {} -> thrdCount
            -- fixtures and each hooks will need one result for each test
            T.EachBefore {} -> tstCount
            T.EachAfter {} -> tstCount
            T.EachAround {} -> tstCount
            -- with fixtures each test spec is mapped individually
            T.Fixture {} -> 1
        specToExpected' = specToExpected baseSeed instanceCount (poisoned, parentResult)
        singleSpecToExpected = specToExpected' . T.All

specToExpected :: Int -> Int -> (Bool, ExpectedResult) -> NSpec -> (Bool, ExpectedResult)
specToExpected
  baseSeed
  resultInstanceCount -- thread nodes and fixtures have more than one expected result per fixture
  (poisoned, parentResult)
  nspc =
    case parentResult of
      All lr -> case lr of
        Pass -> case nspc of
          T.All {spec} -> directiveToExpected poisoned spec.directive
          T.PassProb
            { genStrategy,
              passPcnt,
              hookPassThroughErrPcnt,
              minDelay,
              maxDelay
            } ->
              case genStrategy of
                T.Preload ->
                  ( False,
                    NResult $
                      directiveToLogResult poisoned . (.directive)
                        <$> generateSpecs baseSeed resultInstanceCount "path not used" passPcnt hookPassThroughErrPcnt minDelay maxDelay
                  )
                T.Runtime -> (False, NonDeterministic)
        Fail -> (False, All ParentFailed)
        ParentFailed -> (False, All ParentFailed)
      NonDeterministic -> (False, NonDeterministic)
      -- if the parent is an NResult the result of the child spec will be NonDeterministic because
      -- there is no way of determining which instance will provide the input which may in turn cause the
      -- spec to fail if the parent is PassThroughFail
      NResult {} -> (False, NonDeterministic)

data ResultAccum = Accum
  { poisoned :: Bool,
    parentResult :: ExpectedResult,
    resultMap :: Map EventPath ExpectedResult
  }

directiveToLogResult :: Bool -> Directive -> LogResult
directiveToLogResult poisoned directive =
  poisoned ? Fail $
    directive & \case
      T.Pass -> Pass
      T.Fail -> Fail
      T.PassThroughFail -> Pass

directiveToExpected :: Bool -> Directive -> (Bool, ExpectedResult)
directiveToExpected poisoned d =
  (d == T.PassThroughFail, All $ directiveToLogResult poisoned d)

{- TODO:
1. reinstate chkResult
3. check generating Alls
2. fix check propagation => expect to fail on OnceHook

-}

{-
expectedResults :: Map EventPath ExpectedResult
expectedResults = foldl' calcExpected M.empty $ ts >>= T.allEventPaths

calcExpected :: Map EventPath ExpectedResult -> T.TemplatePath -> Map EventPath ExpectedResult
calcExpected acc T.MkTemplatePath {path, nodeType, evntSpec, template} =
  M.insert (ensureUnique key) expected acc
  where
    key = MkEventPath path nodeType
    ensureUnique k =
      M.member k acc
        ? bug ("duplicate key should not happen. Template paths should be unique: " <> txt k)
        $ k

    expected :: ExpectedResult
    expected =
      case evntSpec of
        T.All Spec {result} -> SuiteRuntimeTestBase.All result
        PassProb {genStrategy, passPcnt, minDelay, maxDelay} ->
          case genStrategy of
            T.Preload -> NResult $ (.result) <$> y generateSpecs baseSeed rLength path passPcnt minDelamaxDelay
            T.Runtime -> NonDeterministic
          where
            rLength = case nodeType of
              Test {} -> bug "Test not expected to have PassProb spec"
              Hook Once _ -> bug "Once  not expected to have PassProb spec"
              Hook Thread _ -> threadLimit.maxThreads -- the most results we will get is the number of threads
              Hook Each _ -> T.countTests template -- expect a result for each test item
              -}

{- NEW
expectedResults :: SpecGen -> Int -> T.Template -> Map EventPath ExpectedResult
expectedResults gen mxThrds =
  expectedResults' M.empty (Hook Once Before) (Just T.Pass)
  where
    isPreload = gen == Preload

    logResult :: NodeType -> Directive -> Directive -> LogResult
    logResult parentNodeType parentDirective directive
      | parentNodeType `elem` [Hook Once Before, Hook Once Setup] && parentDirective == T.PassThroughFail = ParentFailed
      | parentDirective == T.Fail = ParentFailed
      | parentDirective `elem` [T.PassThroughFail, T.Fail] = Fail
      | directive `elem` [T.PassThroughFail, T.Pass] = Pass
      | otherwise = bug "Incomplete pattern match - this should not happen"

    expectedResults' :: Map EventPath ExpectedResult -> NodeType -> Maybe Directive -> T.Template -> Map EventPath ExpectedResult
    -- parent directive of Nothing indicates parent result is NonDeterministic
    expectedResults' accum _parentNodeType Nothing template =
      foldl' (\a p -> M.insert p NonDeterministic a) accum $ T.allPaths template
    expectedResults' accum parentNodeType (Just pDirective) template =
      let mkAllResult thisDirective = All $ logResult parentNodeType pDirective thisDirective
       in case template of
            T.Fixture {tests} ->
              foldl' (\a t -> M.insert (testEventPath t) (mkAllResult t.spec.directive) a) accum tests
            T.OnceBefore {spec, subNodes, path} ->
              let thisDir = spec.directive
                  acc' = M.insert (MkEventPath path (Hook Once Before)) (mkAllResult thisDir) accum
               in foldl' (\a t -> expectedResults' a (Hook Once Before) (Just thisDir) t) acc' subNodes
            T.OnceAround
              { setupSpec,
                teardownSpec,
                subNodes,
                path
              } ->
                let acc' =
                      M.insert (MkEventPath path (Hook Once Teardown)) (mkAllResult setupSpec.directive) $
                        M.insert (MkEventPath path (Hook Once Setup)) (mkAllResult teardownSpec.directive) accum
                 in foldl' (\a t -> expectedResults' a (Hook Once Setup) (Just setupSpec.directive) t) acc' subNodes
            T.ThreadBefore {threadSpec, path} ->
              let thisDir = threadSpec
                  acc' = M.insert (MkEventPath path (Hook Thread Before)) (mkAllResult thisDir) accum
               in foldl' (\a t -> expectedResults' a (Hook Thread Before) (Just thisDir) t) acc' subNodes
--       mkEvnt Thread Before threadSpec : recurse
            _ -> uu

-}
chkFailurePropagation :: [LogItem] -> IO ()
chkFailurePropagation lg =
  do
    traverse_ chkLeafFailsAreNotPropagated failTails
    traverse_ chkParentFailsPropagated failTails
  where
    failTails = snd $ failInfo lg

data ChkState = ExpectParentFail | DoneChecking
  deriving (Show, Eq)

isFailChildEventOf :: LogItem -> LogItem -> Bool
isFailChildEventOf c p =
  (cIsSubpathOfp || samePath && pIsSetupFailure && cIsTeardown) && (sameThread || pIsOnceHook)
  where
    sameThread = p.lineInfo.threadId == c.lineInfo.threadId
    hasHookPos hp = \case
      Hook _ hp' -> hp == hp'
      _ -> False
    cIsTeardown = logItemtoBool (hasHookPos Teardown) c
    pFailEvent = case p.event of
      Failure {nodeType} -> Just nodeType
      _ -> Nothing
    pIsSetupFailure = suitEvntToBool (hasHookPos Setup) pFailEvent

    ploc = p.event.loc
    cloc = c.event.loc
    samePath = ploc == cloc
    cIsSubpathOfp = isParentPath ploc cloc
    pIsOnceHook = suitEvntToBool (\case Hook hz _ -> hz == Once; _ -> False) pFailEvent

chkParentFailsPropagated :: FailInfo -> IO ()
chkParentFailsPropagated
  f@FailInfo
    { failStartTail,
      failLog
    } =
    unless (isChildless f.failLog) $ do
      void $ foldlM chkEvent ExpectParentFail failStartTail
    where
      isFailChildLog :: LogItem -> Bool
      isFailChildLog = flip isFailChildEventOf failLog

      chkEvent :: ChkState -> LogItem -> IO ChkState
      chkEvent acc lgItm =
        let isFailChild = isFailChildLog lgItm
         in acc
              == DoneChecking
                ? pure DoneChecking
              $ lgItm.event
                & \case
                  p@ParentFailure {} ->
                    do
                      -- TODO fix chk' so it prettyprints properly
                      -- that is why chkEq' was used here
                      chkEq'
                        ( "ParentFailure event does not have failure path that is a sub-path of the actual failed event:\n"
                            <> "Parent Failure is:\n"
                            <> "FaileEvent: \n"
                            <> txt failLog
                            <> "\n"
                            <> "\n"
                            <> "Child Failure is:\n"
                            <> txt p
                        )
                        True
                        isFailChild
                      pure ExpectParentFail
                  f'@Failure {} ->
                    -- TODO :: hide reinstate with test conversion
                    fail $ "Failure when expect parent failure:\n" <> ppShow f'
                  s@Start {} ->
                    do
                      -- TODO :: implement chkFalse'
                      -- TODO :: implement txt
                      -- TODO :: chk' error mkessage prints to single line - chkEq' works properly
                      chkEq'
                        ( "This event should be a child failure:\n"
                            <> "  This Event is:\n"
                            <> "    "
                            <> txt s
                            <> "  Parent Failure is:\n"
                            <> "    "
                            <> txt failLog
                        )
                        False
                        isFailChild
                      pure DoneChecking
                  _ ->
                    fail $
                      "Unexpected event in failStartTail - these events should have been filtered out:\n" <> ppShow lgItm

chkLeafFailsAreNotPropagated :: FailInfo -> IO ()
chkLeafFailsAreNotPropagated
  FailInfo
    { failStartTail,
      failLog
    } = when (isChildless failLog) $ do
    whenJust
      (PE.head failStartTail)
      -- TODO: ()may be fixed) chkFail does not work here it escapes new lines - fix and check all chk functions format properly
      ( \l ->
          l.event & \case
            c@ParentFailure {failSuiteEvent} ->
              -- this is wrong can pick up failed elements from another branch
              unless (onceSuiteEvent failSuiteEvent) $ do
                {-
                if a leaf item such as an after hook or test fails then the next item
                should not be a parent failure because leaf items can't be parents.
                This does not apply if the parent failure was caused by a once event
                because such failures can be generated when the thread picks up nodes from
                a different branch
                -}
                fail $
                  "Leaf failure propagated to next event.\nLeaf event was:\n"
                    <> ppShow failLog
                    <> "\nNext event was:\n"
                    <> ppShow c
            _ -> pure ()
      )

-- TODO :: REMOVE USER ERROR force to throw or reinterpret user error as failure or ...
-- captures
-- declares element details and has default plus bepoke validation
-- chkCapture - will log a soft exception and allow trace in place
-- property that includes assertions

failInfo :: [LogItem] -> (Maybe NodeType, [FailInfo])
failInfo ls =
  foldl' step (Nothing, []) $ tails failStarts
  where
    step :: (Maybe NodeType, [FailInfo]) -> [LogItem] -> (Maybe NodeType, [FailInfo])
    step (lastStartEvnt, result) =
      \case
        [] -> (lastStartEvnt, result)
        (l : ls') ->
          l.event & \case
            FilterLog {} -> passThrough
            SuiteInitFailure {} -> passThrough
            Start {nodeType = se} ->
              (Just se, result)
            Failure {} ->
              lastStartEvnt
                & maybe
                  (error $ "Failure encountered before start:\n" <> toS (ppShow l))
                  (const (Nothing, FailInfo l ls' : result))
            -- todo think about logic here
            InitialisationFailure {} -> passThrough
            ParentFailure {} -> passThrough
            StartExecution {} -> passThrough
            EndExecution {} -> passThrough
            NodeLog {} -> passThrough
            End {} -> passThrough
      where
        passThrough = (lastStartEvnt, result)
    failStarts =
      filter
        ( \li ->
            li.event & \case
              Failure {} -> True
              ParentFailure {} -> True
              Start {} -> True
              _ -> False
        )
        ls

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
      case li.event of
        ParentFailure {loc} -> chkEq' ("Parent failure loc for " <> toS (ppShow li)) mParentLoc (Just loc)
        _ -> pure ()

chkAfterTeardownParents :: Map T.EventPath T.EventPath -> [LogItem] -> IO ()
chkAfterTeardownParents =
  chkForMatchedParents
    "After / Teardown parent event"
    False -- leave the list in reverse order so we are forward through subsequent events
    isAfterSuiteEvent

-- isAnyHookSuiteEvent

-- isAfterSuiteEvent -- I think this logic is wrong shoud be checking every event

chkPrecedingSuiteEventAsExpected :: Map T.EventPath T.EventPath -> [LogItem] -> IO ()
chkPrecedingSuiteEventAsExpected =
  chkForMatchedParents
    "preceding parent event"
    True -- reverse list so we are searching back through preceding events
    isBeforeSuiteEvent

chkForMatchedParents :: Text -> Bool -> (LogItem -> Bool) -> Map T.EventPath T.EventPath -> [LogItem] -> IO ()
chkForMatchedParents message wantReverseLog parentEventPredicate expectedChildParentMap thrdLog =
  traverse_ chkParent actualParents
  where
    chkParent :: (T.EventPath, Maybe T.EventPath) -> IO ()
    chkParent (childPath, actualParentPath) =
      chkEq' (message <> " for:\n" <> txt childPath) expectedParentPath actualParentPath
      where
        expectedParentPath = M.lookup childPath expectedChildParentMap

    actualParents :: [(T.EventPath, Maybe T.EventPath)]
    actualParents = mapMaybe extractHeadParent thrdLogTails

    thrdLogTails :: [[LogItem]]
    thrdLogTails = logTails wantReverseLog thrdLog

    extractHeadParent :: [LogItem] -> Maybe (T.EventPath, Maybe T.EventPath)
    extractHeadParent evntLog =
      (,actulaParentPath) <$> targetPath
      where
        logSuiteEventPath :: LogItem -> Maybe T.EventPath
        logSuiteEventPath l = MkEventPath <$> (startSuiteEventLoc l >>= topPath) <*> getSuiteEvent l
        targEvnt = PE.head evntLog
        targetPath = targEvnt >>= logSuiteEventPath
        actulaParentPath = do
          h <- targEvnt
          t <- PE.tail evntLog -- all preceding / successive events
          fps <- findMathcingParent parentEventPredicate h t
          logSuiteEventPath fps

logTails :: Bool -> [LogItem] -> [[LogItem]]
logTails wantReverse = tails . bool PR.id reverse wantReverse

startHook :: [Hz] -> [HookPos] -> LogItem -> Bool
startHook hzs poss l = startOrParentFailure l && (getHookInfo l & maybe False (\(hz', hkPos) -> hz' `PE.elem` hzs && hkPos `PE.elem` poss))

chkNoEmptyPostHooks :: [Hz] -> [LogItem] -> IO ()
chkNoEmptyPostHooks hzs =
  chkNoEmptyHooks'
    "Post Hook Empty"
    (startHook hzs [Teardown, After])
    True

chkNoEmptyPreHooks :: [Hz] -> [LogItem] -> IO ()
chkNoEmptyPreHooks hzs =
  chkNoEmptyHooks'
    "Post Hook Empty"
    (startHook hzs [Setup, Before])
    False

chkNoEmptyHooks' :: Text -> (LogItem -> Bool) -> Bool -> [LogItem] -> IO ()
chkNoEmptyHooks' message hookPredicate wantReverse =
  traverse_ chkHasTest . logTails wantReverse
  where
    chkHasTest :: [LogItem] -> IO ()
    chkHasTest = \case
      [] -> pure () -- wont happen
      x : xs ->
        when
          (hookPredicate x)
          $ chk'
            (message <> " \nEmpty Hook:\n" <> txt x)
            (findChildTest x xs)

    findChildTest :: LogItem -> [LogItem] -> Bool
    findChildTest hk =
      any (fromMaybe False . testMatchesParent)
      where
        testMatchesParent :: LogItem -> Maybe Bool
        testMatchesParent =
          parentMatchesTest (const True) hk

chkAllStartSuitEventsInThreadImmedialyFollowedByEnd :: [LogItem] -> IO ()
chkAllStartSuitEventsInThreadImmedialyFollowedByEnd =
  chkStartSuiteEventImmediatlyFollowedByEnd (startEndNodeMatch (const True))

chkStartSuiteEventImmediatlyFollowedByEnd :: (LogItem -> Bool) -> [LogItem] -> IO ()
chkStartSuiteEventImmediatlyFollowedByEnd p l = do
  unless (null startNotFollwedByEnd) $
    fail $
      "Thread suite elements - start not followed by end:\n" <> toS (ppShow startNotFollwedByEnd)
  where
    trgEvnts = filter p l
    startNotFollwedByEnd = filter (\(s, e) -> isStart s && (not (isEnd e) || s.event.loc /= e.event.loc)) . zip trgEvnts $ drop 1 trgEvnts

threadLogChks :: Bool -> [LogItem] -> [[LogItem] -> IO ()] -> IO ()
threadLogChks includeOnce fullLog = traverse_ chkTls
  where
    tlgs = threadedLogs includeOnce fullLog
    chkTls = checkThreadLogs tlgs
    checkThreadLogs :: [[LogItem]] -> ([LogItem] -> IO ()) -> IO ()
    checkThreadLogs tls' lgChk = traverse_ lgChk tls'

chkThreadHooksStartedOnceInThread :: [LogItem] -> IO ()
chkThreadHooksStartedOnceInThread =
  chkStartsOnce "thread elements" (startEndNodeMatch threadHook)

-- TODO:: reexport putStrLn et. al with text conversion

chkStartsOnce :: Text -> (LogItem -> Bool) -> [LogItem] -> IO ()
chkStartsOnce errSfx p l = do
  --  putStrLn $ ppShowList trgEvnts
  unless (null dupLocs) $
    fail $
      toS errSfx <> ":\n" <> toS (ppShow dupLocs)
  where
    trgEvnts = filter p l
    starts = filter isStart trgEvnts
    dupLocs = filter ((> 1) . length) . fmap (PE.head . fmap (.event.loc)) . groupOn' (.event.loc) $ starts

chkAllTemplateItemsLogged :: [T.Template] -> [LogItem] -> IO ()
chkAllTemplateItemsLogged ts lgs =
  unless (null errMissng || null errExtra) $
    fail (errMissng <> "\n" <> errExtra)
  where
    errMissng = null missing ? "" $ "template items not present in log:\n" <> ppShow missing
    errExtra = null extra ? "" $ "extra items in the log that are not in the template:\n" <> ppShow extra
    extra = S.difference logStartPaths tmplatePaths
    missing = S.difference tmplatePaths logStartPaths

    -- init to empty set
    tmplatePaths :: Set EventPath
    tmplatePaths = fromList $ ts >>= T.allEventPaths

    logStartPaths :: Set EventPath
    logStartPaths =
      fromList $
        Prelude.mapMaybe
          ( \lg ->
              do
                case lg.event of
                  ParentFailure {loc, nodeType} -> flip MkEventPath nodeType <$> topPath loc
                  Start {loc, nodeType} -> flip MkEventPath nodeType <$> topPath loc
                  _ -> Nothing
          )
          lgs

nxtHookLog :: [LogItem] -> Maybe LogItem
nxtHookLog = find (\l -> startEndNodeMatch isHook l || isHookParentFailure l)

{-
 TODO: when implementing log parsing need a threadView which includes all thread events
 and all parent OnceEvents - should probably log OnceEvents in a separate log
 as well as main log to so don't have to read whole log for once events

 same goes for filter log
-}

threadVisible :: Bool -> ThreadId -> [LogItem] -> [LogItem]
threadVisible onceHookInclude tid =
  filter (\l -> tid == l.lineInfo.threadId || onceHookInclude && (startEndNodeMatch onceHook l || isOnceHookParentFailure l))

threadIds :: [LogItem] -> [ThreadId]
threadIds = PE.nub . fmap (.lineInfo.threadId)

threadedLogs :: Bool -> [LogItem] -> [[LogItem]]
threadedLogs onceHookInclude l =
  (\tid -> threadVisible onceHookInclude tid l) <$> threadIds l

shouldOccurOnce :: LogItem -> Bool
shouldOccurOnce = startEndNodeMatch onceSuiteEvent

chkStartEndExecution :: [FLog ExePath AE.NodeLog] -> IO ()
chkStartEndExecution evts =
  (,)
    <$> PE.head evts
    <*> PE.last evts
      & maybe
        (fail "no events")
        ( \(s, e) -> do
            s.event & \case
              StartExecution {} -> pure ()
              _ -> fail $ "first event is not StartExecution:\n " <> toS (ppShow s)
            e.event & \case
              EndExecution {} -> pure ()
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
chkThreadLogsInOrder ls =
  do
    chk' "Nothing found in heads - groupOn error this should not happen" (all isJust heads)
    traverse_ (chkEq' "first index of thread should be 0" 0 . (.lineInfo.idx)) $ catMaybes heads
    traverse_ chkIds threads
  where
    threads = groupOn' getThreadId ls
    -- TODO: need to draw a line in the sand re maybe vs nonemptyList
    heads = PE.head <$> threads
    chkIds ls' =
      for_
        (zip ls' $ drop 1 ls')
        ( \(l1, l2) ->
            let idx1 = l1.lineInfo.idx
                idx2 = l2.lineInfo.idx
             in chkEqfmt' (succ idx1) idx2 $
                  "event idx not consecutive\n"
                    <> toS (ppShow l1)
                    <> "\n"
                    <> toS (ppShow l2)
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

onceBefore :: Directive -> [Template] -> Template
onceBefore = OnceBefore . Spec 0

onceAfter :: Directive -> [Template] -> Template
onceAfter = OnceAfter . Spec 0

onceAround :: Directive -> Directive -> [Template] -> Template
onceAround suRslt tdRslt = OnceAround (Spec 0 suRslt) (Spec 0 tdRslt)

threadBefore :: Directive -> [Template] -> Template
threadBefore r = ThreadBefore (allSpec 0 r)

threadAfter :: Directive -> [Template] -> Template
threadAfter r = ThreadAfter (allSpec 0 r)

threadAround :: Directive -> Directive -> [Template] -> Template
threadAround suRslt tdRslt = ThreadAround (allSpec 0 suRslt) (allSpec 0 tdRslt)

eachBefore :: Directive -> [Template] -> Template
eachBefore = EachBefore . allSpec 0

eachAfter :: Directive -> [Template] -> Template
eachAfter = EachAfter . allSpec 0

eachAround :: Directive -> Directive -> [Template] -> Template
eachAround suRslt tdRslt = EachAround (allSpec 0 suRslt) (allSpec 0 tdRslt)

fixture :: [Spec] -> Template
fixture = SuiteRuntimeTestBase.Fixture

test :: Directive -> Spec
test = Spec 0

data ExeResult = ExeResult
  { expandedTemplate :: [T.Template],
    log :: [FLog ExePath AE.NodeLog]
  }

runTest :: Int -> ThreadCount -> [Template] -> IO ()
runTest = runTest' logging

data Logging = Log | NoLog | LogTemplate | LogFails | LogFailsAndStartTest deriving (Show, Eq)

runTest' :: Logging -> Int -> ThreadCount -> [Template] -> IO ()
runTest' wantLog baseRandomSeed threadLimit templates = do
  when (wantLog == LogFailsAndStartTest) $
    printNow "start test"
  ExeResult expandedTemplate log <- execute wantLog baseRandomSeed threadLimit templates
  onError
    (chkProperties baseRandomSeed threadLimit expandedTemplate log)
    ( do
        when (wantLog `PE.elem` [LogFails, LogFailsAndStartTest]) $ do
          putStrLn "#### Template ####"
          pPrint expandedTemplate
          putStrLn "#### Log ####"
          pPrint log
          putStrLn "========="
    )

execute :: Logging -> Int -> ThreadCount -> [Template] -> IO ExeResult
execute wantLog baseRandomSeed threadLimit templates = do
  let fullTs = setPaths "" templates
  lg <- exeTemplate wantLog baseRandomSeed threadLimit fullTs
  pure $ ExeResult fullTs lg

exeTemplate :: Logging -> Int -> ThreadCount -> [T.Template] -> IO [FLog ExePath AE.NodeLog]
exeTemplate wantLog baseRandomSeed maxThreads templates = do
  let wantLog' = wantLog == Log
  (lc, logLst) <- testLogActions wantLog'
  when (wantLog' || wantLog == LogTemplate) $ do
    putStrLn "#### Template ####"
    pPrint templates
    putStrLn "========="
  nodes <- mkNodes baseRandomSeed maxThreads templates
  when wantLog' $ do
    putStrLn "#### (Indent, Node Path) After Prepare ####"
    pPrint $ P.listPaths <$> nodes
    putStrLn "========="
    putStrLn "#### Log ####"
  executeWithoutValidation maxThreads lc $ mkRootNode <$> nodes
  atomically logLst

loadTQueue :: TQueue a -> [a] -> STM ()
loadTQueue q = traverse_ (writeTQueue q)

setPaths :: Text -> [Template] -> [T.Template]
setPaths address ts =
  uncurry setPath <$> zip [0 ..] ts
  where
    nxtAdd idx =
      let txIdx = txt idx
          sfx = T.null address ? txIdx $ "." <> txIdx
       in address <> sfx

    setPath :: Int -> Template -> T.Template
    setPath idx tp =
      case tp of
        SuiteRuntimeTestBase.Fixture {tests} ->
          T.Fixture
            { path = newPath "Test",
              tests = zip [0 ..] tests <&> \(idx', spec) -> T.TestItem {title = newAdd <> ".Test #" <> txt idx', id = idx', ..}
            }
        OnceBefore {..} -> T.OnceBefore {path = newPath "OnceBefore", subNodes = newNodes, ..}
        OnceAfter {..} -> T.OnceAfter {path = newPath "OnceAfter", subNodes = newNodes, ..}
        OnceAround {..} -> T.OnceAround {path = newPath "OnceAround", subNodes = newNodes, ..}
        ThreadBefore {..} -> T.ThreadBefore {path = newPath "ThreadBefore", subNodes = newNodes, ..}
        ThreadAfter {..} -> T.ThreadAfter {path = newPath "ThreadAfter", subNodes = newNodes, ..}
        ThreadAround {..} -> T.ThreadAround {path = newPath "ThreadAround", subNodes = newNodes, ..}
        EachBefore {..} -> T.EachBefore {path = newPath "EachBefore", subNodes = newNodes, ..}
        EachAfter {..} -> T.EachAfter {path = newPath "EachAfter", subNodes = newNodes, ..}
        EachAround {..} -> T.EachAround {path = newPath "EachAround", subNodes = newNodes, ..}
      where
        newPath = AE.NodePath newAdd
        newAdd = nxtAdd idx
        newNodes = setPaths newAdd tp.subNodes

{-
todo - trace like with pretty printing
  db == debug'
  dbNoLabel
  dbCondional
  dbCondionalNoLabel
  dbRem
  dbf
-}

{-
 Look for a matching nested test
 Just True if:
  IF  parent item matches predicate
  AND parent is a start event
  AND testItem is a start test or test abandonned event
  AND testItem is a child of hook
-}
parentMatchesTest :: (LogItem -> Bool) -> LogItem -> LogItem -> Maybe Bool
parentMatchesTest parentPredicate parentHookItm testItm = do
  if parentPredicate parentHookItm
    then do
      startEvntPath <- startSuiteEventLoc testItm
      tstStartSubPath <- parentPath (isTestEventOrTestParentFailure testItm) startEvntPath
      parentPath' <- startSuiteEventLoc parentHookItm
      pure $ parentPath'.un `PE.isSuffixOf` tstStartSubPath.un
    else
      pure False

findMathcingParent :: (LogItem -> Bool) -> LogItem -> [LogItem] -> Maybe LogItem
findMathcingParent parentPredicate testStartEvnt =
  find (fromMaybe False . parentEvntMatches)
  where
    parentEvntMatches :: LogItem -> Maybe Bool
    parentEvntMatches parentCandidteEvt =
      parentMatchesTest parentPredicate parentCandidteEvt testStartEvnt

isParentPath :: ExePath -> ExePath -> Bool
isParentPath (ExePath parent) (ExePath child) =
  PE.tail child & maybe False (parent `PE.isSuffixOf`)

eventMatchesHookPos :: [HookPos] -> LogItem -> Bool
eventMatchesHookPos hookPoses lg =
  suiteEventOrParentFailureSuiteEvent lg
    & maybe
      False
      ( \case
          -- TODO: sort out imports see PE.elem
          Hook _frq pos -> pos `PR.elem` hookPoses
          Test -> False
      )

isBeforeSuiteEvent :: LogItem -> Bool
isBeforeSuiteEvent = eventMatchesHookPos [Before, Setup]

isAnyHookSuiteEvent :: LogItem -> Bool
isAnyHookSuiteEvent = eventMatchesHookPos [After, Teardown, Before, Setup]

isAfterSuiteEvent :: LogItem -> Bool
isAfterSuiteEvent = eventMatchesHookPos [After, Teardown]

newtype FixtureConfig = FxCfg
  {title :: Text}
  deriving (Generic, Show, Eq)

instance ToJSON FixtureConfig

instance Core.Config FixtureConfig

fc :: FixtureConfig
fc = FxCfg {title = "fixture config"}

data ManyParams = ManyParams
  { baseSeed :: Int,
    subSeed :: Int,
    path :: Text,
    passPcnt :: Int8,
    hookPassThroughErrPcnt :: Int8,
    minDelay :: Int,
    maxDelay :: Int
  }

mkManySpec :: ManyParams -> Spec
mkManySpec
  ManyParams
    { baseSeed,
      subSeed,
      path,
      passPcnt,
      hookPassThroughErrPcnt,
      minDelay,
      maxDelay
    } =
    Spec delay result
    where
      seed = H.hash $ txt baseSeed <> path <> txt subSeed
      delayRange = maxDelay - minDelay
      delay = delayRange > 0 ? minDelay + seed `mod` (maxDelay - minDelay) $ minDelay
      pcntMod = seed `mod` 100
      passPcnt' = fromIntegral passPcnt
      passThuFailPcnt = fromIntegral hookPassThroughErrPcnt
      result =
        pcntMod < passPcnt' ? T.Pass $
          pcntMod < passPcnt' + passThuFailPcnt ? T.PassThroughFail $
            T.Fail

-- assumes th queue is preloaded (ie loadQIfPrload has already been run) if genStrategy == Preload
mkManyAction :: forall pth. (Show pth) => Int -> TQueue Spec -> pth -> NSpec -> IO DummyHkResult
mkManyAction baseSeed q pth = \case
  T.All s -> mkAction' pth s
  PassProb
    { genStrategy,
      passPcnt,
      hookPassThroughErrPcnt,
      minDelay,
      maxDelay
    } ->
      case genStrategy of
        Preload -> mkQueAction q pth
        Runtime -> do
          subSeed <- RS.uniformM RS.globalStdGen :: IO Int
          mkAction' pth $
            mkManySpec
              ManyParams
                { baseSeed,
                  subSeed,
                  path = txt pth,
                  passPcnt,
                  hookPassThroughErrPcnt,
                  minDelay,
                  maxDelay
                }

-- TODO: make bug / error functions that uses text instead of string
-- TODO: check callstack
-- includes unused param for convenience below
mkAction :: forall pth. (Show pth) => pth -> Spec -> P.LogSink -> DummyHkResult -> IO DummyHkResult
mkAction path spec _sink = mkActionBase path spec

mkManyAfterAction :: forall pth. (Show pth) => Int -> TQueue Spec -> pth -> NSpec -> IO ()
mkManyAfterAction baseSeed q pth ms = void $ mkManyAction baseSeed q pth ms

mkQueAction :: forall path. (Show path) => TQueue Spec -> path -> IO DummyHkResult
mkQueAction q path =
  do
    s <- atomically $ tryReadTQueue q
    s
      & maybe
        (error $ "spec queue is empty - either the fixture template has been misconfigured or a thread hook is being called more than once in a thread (which should not happen) at path: " <> txt path)
        (mkAction' path)

-- used in both generating test run and validation
-- is pure ie. will always generate the same specs for same inputs
generateSpecs :: (Show pth) => Int -> Int -> pth -> Int8 -> Int8 -> Int -> Int -> [Spec]
generateSpecs baseSeed qLength pth passPcnt hookPassThroughErrPcnt minDelay maxDelay =
  manySpec <$> [1 .. qLength]
  where
    manySpec :: Int -> Spec
    manySpec subSeed =
      mkManySpec
        ManyParams
          { baseSeed,
            subSeed,
            path = txt pth,
            passPcnt,
            hookPassThroughErrPcnt,
            minDelay,
            maxDelay
          }

loadHookQIfPreload :: (Show pth) => Int -> Int -> pth -> TQueue Spec -> NSpec -> IO ()
loadHookQIfPreload baseSeed qLength pth q = \case
  T.All _ -> pure ()
  PassProb
    { genStrategy,
      passPcnt,
      hookPassThroughErrPcnt,
      minDelay,
      maxDelay
    } ->
      do
        when (isPreload genStrategy)
          . atomically
          . loadTQueue q
          $ generateSpecs baseSeed qLength pth passPcnt hookPassThroughErrPcnt minDelay maxDelay
        pure ()

mkNodes :: Int -> ThreadCount -> [T.Template] -> IO [P.PreNode IO DummyHkResult]
mkNodes baseSeed mxThreads = mapM mkNode
  where
    afterAction :: (Show pth) => pth -> Spec -> b -> IO ()
    afterAction path spec = void <$> const (mkAction' path spec)

    mkNodes' = mkNodes baseSeed mxThreads
    mkNode :: T.Template -> IO (P.PreNode IO DummyHkResult)
    mkNode t = case t of
      T.Fixture
        { path,
          tests
        } ->
          pure $
            P.Fixture
              { config = fc,
                path,
                tests = ItemList $ mkTestItem <$> tests
              }
      _ ->
        do
          nds <- mkNodes' t.subNodes
          b4Q <- newTQueueIO
          afterQ <- newTQueueIO
          let mxThrds = mxThreads.maxThreads
              tstItemCount = T.countTests t
              loadHookQIfPreload' = loadHookQIfPreload baseSeed
          case t of
            T.OnceBefore
              { path,
                spec
              } ->
                pure $ do
                  P.Before
                    { path,
                      frequency = Once,
                      action = mkAction path spec,
                      subNodes = nds
                    }
            T.OnceAfter
              { path,
                spec
              } ->
                pure $
                  P.After
                    { path,
                      frequency = Once,
                      after = afterAction path spec,
                      subNodes' = nds
                    }
            T.OnceAround
              { path,
                setupSpec,
                teardownSpec
              } ->
                pure $
                  P.Around
                    { path,
                      frequency = Once,
                      setup = mkAction path setupSpec,
                      teardown = mkAction_ path teardownSpec,
                      subNodes = nds
                    }
            T.EachBefore
              { path,
                eachSpec
              } ->
                do
                  loadHookQIfPreload' tstItemCount path b4Q eachSpec
                  pure $
                    P.Before
                      { path,
                        frequency = Each,
                        action = const . const $ mkManyAction baseSeed b4Q path eachSpec,
                        subNodes = nds
                      }
            T.EachAfter
              { path,
                eachSpec
              } ->
                do
                  loadHookQIfPreload' tstItemCount path afterQ eachSpec
                  pure $
                    P.After
                      { path,
                        frequency = Each,
                        after = const $ mkManyAfterAction baseSeed afterQ path eachSpec,
                        subNodes' = nds
                      }
            T.EachAround
              { path,
                eachSetupSpec,
                eachTeardownSpec
              } ->
                do
                  loadHookQIfPreload' tstItemCount path b4Q eachSetupSpec
                  loadHookQIfPreload' tstItemCount path afterQ eachTeardownSpec
                  pure $
                    P.Around
                      { path,
                        frequency = Each,
                        setup = const . const $ mkManyAction baseSeed b4Q path eachSetupSpec,
                        teardown = const . const $ mkManyAfterAction baseSeed afterQ path eachTeardownSpec,
                        subNodes = nds
                      }
            _ -> case t of
              T.ThreadBefore
                { path,
                  threadSpec
                } ->
                  do
                    loadHookQIfPreload' mxThrds path b4Q threadSpec
                    pure $
                      P.Before
                        { path,
                          frequency = Thread,
                          action = const . const $ mkManyAction baseSeed b4Q path threadSpec,
                          subNodes = nds
                        }
              T.ThreadAfter
                { path,
                  threadSpec
                } ->
                  do
                    loadHookQIfPreload' mxThrds path afterQ threadSpec
                    pure $
                      P.After
                        { path,
                          frequency = Thread,
                          after = const $ mkManyAfterAction baseSeed afterQ path threadSpec,
                          subNodes' = nds
                        }
              T.ThreadAround
                { path,
                  setupThreadSpec,
                  teardownThreadSpec
                } -> do
                  loadHookQIfPreload' mxThrds path b4Q setupThreadSpec
                  loadHookQIfPreload' mxThrds path afterQ teardownThreadSpec
                  pure $
                    P.Around
                      { path,
                        frequency = Thread,
                        setup = const . const $ mkManyAction baseSeed b4Q path setupThreadSpec,
                        teardown = const . const $ mkManyAfterAction baseSeed afterQ path teardownThreadSpec,
                        subNodes = nds
                      }

data Template
  = OnceBefore
      { spec :: Spec,
        subNodes :: [Template]
      }
  | OnceAfter
      { spec :: Spec,
        subNodes :: [Template]
      }
  | OnceAround
      { setupSpec :: Spec,
        teardownSpec :: Spec,
        subNodes :: [Template]
      }
  | ThreadBefore
      { threadSpec :: NSpec,
        subNodes :: [Template]
      }
  | ThreadAfter
      { threadSpec :: NSpec,
        subNodes :: [Template]
      }
  | ThreadAround
      { setupThreadSpec :: NSpec,
        teardownThreadSpec :: NSpec,
        subNodes :: [Template]
      }
  | EachBefore
      { eachSpec :: NSpec,
        subNodes :: [Template]
      }
  | EachAfter
      { eachSpec :: NSpec,
        subNodes :: [Template]
      }
  | EachAround
      { eachSetupSpec :: NSpec,
        eachTeardownSpec :: NSpec,
        subNodes :: [Template]
      }
  | Fixture
      { tests :: [Spec]
      }
  deriving (Show, Eq)

mkTestItem :: T.TestItem -> P.Test IO DummyHkResult
mkTestItem T.TestItem {id, title, spec} = P.MkTest id title (mkAction_ title spec)

newtype DummyHkResult = DummyHkResult
  {dummyHkResult :: Int}
  deriving (Show, Eq, Ord)

mkRootAction :: (P.LogSink -> DummyHkResult -> IO o) -> P.LogSink -> () -> IO o
mkRootAction action ls _hr = action ls (DummyHkResult 0)

mkRootNode :: P.PreNode IO DummyHkResult -> P.PreNode IO ()
mkRootNode = \case
  P.Before {path, frequency, action, subNodes} ->
    P.Before
      { path,
        frequency,
        action = mkRootAction action,
        subNodes
      }
  P.After {path, frequency, after, subNodes'} ->
    P.After
      { path,
        frequency,
        after = after,
        subNodes' = mkRootNode <$> subNodes'
      }
  P.Around {path, frequency, setup, teardown, subNodes} ->
    P.Around
      { path,
        frequency,
        setup = mkRootAction setup,
        teardown = teardown,
        subNodes = subNodes
      }
  P.Fixture {config, path, tests} ->
    P.Fixture
      { config,
        path,
        tests = mkRootTest <$> tests
      }

mkRootTest :: P.Test IO DummyHkResult -> P.Test IO ()
mkRootTest P.MkTest {id, title, action} =
  P.MkTest
    { id,
      title,
      action = mkRootAction action
    }

mkAction' :: forall pth. (Show pth) => pth -> Spec -> IO DummyHkResult
mkAction' path spec = mkActionBase path spec $ DummyHkResult 0

mkAction_ :: forall pth. (Show pth) => pth -> Spec -> P.LogSink -> DummyHkResult -> IO ()
mkAction_ path spec sink hkIn = void (mkAction path spec sink hkIn)

mkActionBase :: forall pth. (Show pth) => pth -> Spec -> DummyHkResult -> IO DummyHkResult
mkActionBase path spec hi = do
  C.threadDelay spec.delay
  --  make sure the result is used to force any pas through errors
  let !hi' = hi {dummyHkResult = hi.dummyHkResult + 1}
  case spec.directive of
    T.Pass -> pure hi'
    T.Fail -> error . toS $ "FAIL RESULT @ " <> txt path
    T.PassThroughFail -> pure . error $ "Deferred error from " <> txt path