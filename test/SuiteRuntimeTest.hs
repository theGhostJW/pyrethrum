module SuiteRuntimeTest where

import Check (Checks)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import DSL.Interpreter
import Data.Aeson.Encoding (quarter)
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Set as ST
import Data.Yaml
import GHC.Records
import Internal.PreNode (PreNode (hookChild))
import Internal.PreNode as PN
import qualified Internal.SuiteRuntime as S
import Polysemy
import Pyrelude as P
  ( Bool (..),
    Eq (..),
    IO,
    Int,
    ListLike (foldl', head, null, unsafeHead, unsafeLast),
    Maybe (Just, Nothing),
    Num ((+)),
    Ord (..),
    Show,
    Text,
    Traversable (traverse),
    catMaybes,
    const,
    count,
    debug_,
    debugf,
    dropWhile,
    dropWhileEnd,
    error,
    filter,
    find,
    for_,
    fromJust,
    groupBy,
    isPrefixOf,
    last,
    length,
    maybe,
    maybef,
    not,
    nub,
    pure,
    replicateM_,
    reverse,
    singleton,
    toS,
    traverse_,
    txt,
    txtPretty,
    uncurry,
    uu,
    zip,
    ($),
    (&&),
    (-),
    (.),
    (<$>),
    (<>),
    (>>),
    (>>=),
    (?),
    (\\),
    (||),
  )
import Pyrelude.Test (chk', chkFail)
import Pyrelude.Test as T hiding (filter, maybe, singleton)
import TempUtils (debugLines)
import Text.Show.Pretty (PrettyVal (prettyVal), pPrint, pPrintList, ppShow, ppShowList)
import UnliftIO.Concurrent as C
  ( ThreadId,
    forkFinally,
    forkIO,
    myThreadId,
    threadDelay,
  )
import UnliftIO.STM
import Prelude (Ord, putStrLn)

data BoundaryType
  = Start
  | End
  deriving (Show, Eq, Ord)

data BranchType
  = Hook
  | Fixture
  deriving (Show, Eq, Ord)

data NodeStats
  = HookStats
      { id :: Text,
        parent :: Text,
        fixtureCount :: Int,
        hookCount :: Int
      }
  | FixtureStats
      { id :: Text,
        parent :: Text,
        iterationCount :: Int
      }
  deriving (Show)

isHookStats :: NodeStats -> Bool
isHookStats = \case
  HookStats {} -> True
  FixtureStats {} -> False

isFixtureStats :: NodeStats -> Bool
isFixtureStats = \case
  HookStats {} -> False
  FixtureStats {} -> True

countSubNodes :: (forall a b. PreNode a b -> Bool) -> PreNode c d -> Int
countSubNodes pred node =
  let countSubNodes' accum = \case
        b@Branch {subElms} ->
          foldl' countSubNodes' (pred b ? accum + 1 $ accum) subElms
        ah@AnyHook {} -> pred ah ? accum + 1 $ accum
        fx@PN.Fixture {} -> pred fx ? accum + 1 $ accum
   in countSubNodes' 0 node

getStats :: PN.PreNodeRoot a -> IO [NodeStats]
getStats PreNodeRoot {rootNode} =
  getStats' "Root" 0 <$> rootNode
  where
    nonEmptyFixture :: PreNode a b -> Bool
    nonEmptyFixture = \case
      PN.AnyHook {} -> False
      PN.Branch {} -> False
      f@PN.Fixture {} -> not $ nodeEmpty f

    nonEmptyHook :: PreNode a b -> Bool
    nonEmptyHook = \case
      h@PN.AnyHook {} -> not $ nodeEmpty h
      PN.Branch {} -> False
      PN.Fixture {} -> False

    getStats' :: Text -> Int -> PreNode a b -> [NodeStats]
    getStats' parentId subIndex =
      \case
        PN.Branch {subElms} ->
          let thisId = parentId <> ".Branch " <> txt subIndex
           in (zip [0 ..] subElms >>= uncurry (getStats' thisId))
        PN.AnyHook {hookChild} ->
          let thisId = parentId <> ".Hook " <> txt subIndex
              thisNode =
                HookStats
                  { id = thisId,
                    parent = parentId,
                    fixtureCount = countSubNodes nonEmptyFixture hookChild,
                    hookCount = countSubNodes nonEmptyHook hookChild
                  }
           in thisNode : getStats' thisId 0 hookChild
        PN.Fixture {iterations} ->
          [ FixtureStats
              { id = parentId <> ".Fixture " <> txt subIndex,
                parent = parentId,
                iterationCount = length iterations
              }
          ]

data BoundaryInfo = BoundaryInfo
  { id :: Text,
    childCount :: Int
  }

data RunEvent
  = Boundary
      { branchType :: BranchType,
        boundaryType :: BoundaryType,
        boundaryParentFix :: Text,
        threadId :: ThreadId,
        id :: Text
      }
  | IterationMessage
      { parentFix :: Text,
        index :: Int,
        message :: Text,
        threadId :: ThreadId
      }
  | Message Text ThreadId
  deriving (Show, Eq, Ord)

logEvent :: TQueue RunEvent -> (ThreadId -> RunEvent) -> IO ()
logEvent q ev = do
  i <- myThreadId
  atomically . writeTQueue q $ ev i

fullId :: Text -> Text -> Text
fullId parentId childId = parentId <> "." <> childId

logBoundary :: TQueue RunEvent -> BranchType -> BoundaryType -> Text -> Text -> IO ()
logBoundary q brt bdt parentId fullChildId =
  logEvent q $ \thrd ->
    Boundary
      { branchType = brt,
        boundaryType = bdt,
        boundaryParentFix = parentId,
        threadId = thrd,
        id = fullChildId
      }

hook :: TQueue RunEvent -> BoundaryType -> Text -> Text -> IO ()
hook q = logBoundary q SuiteRuntimeTest.Hook

hookStart :: TQueue RunEvent -> Text -> Text -> IO ()
hookStart q = SuiteRuntimeTest.hook q Start

hookEnd :: TQueue RunEvent -> Text -> Text -> IO ()
hookEnd q = SuiteRuntimeTest.hook q End

fixture :: TQueue RunEvent -> BoundaryType -> Text -> Text -> IO ()
fixture q = logBoundary q SuiteRuntimeTest.Fixture

fixtureStart :: TQueue RunEvent -> Text -> Text -> IO ()
fixtureStart q = SuiteRuntimeTest.fixture q Start

fixtureEnd :: TQueue RunEvent -> Text -> Text -> IO ()
fixtureEnd q = SuiteRuntimeTest.fixture q End

logIteration :: TQueue RunEvent -> Text -> Int -> Text -> IO ()
logIteration q fxTxt iidx itMsg =
  logEvent q (IterationMessage fxTxt iidx itMsg)

logMessage :: TQueue RunEvent -> Text -> IO ()
logMessage q txt' = logEvent q (Message txt')

-- remove when pyrelude updated
chkEq' t = assertEqual (toS t)

mkFixture :: TQueue RunEvent -> Text -> Text -> Int -> IO (PreNode () ())
mkFixture q parentId fxId itCount = do
  fs <- newTVarIO Pending
  pure $
    PN.Fixture
      { fixtureAddress = fid,
        fixtureStatus = fs,
        logStart = fixtureStart q parentId fid,
        iterations = mkIterations q fid itCount,
        logEnd = fixtureEnd q parentId fid
      }
  where
    fid = fullId parentId fxId

mkHook :: TQueue RunEvent -> Text -> Text -> PreNode () o2 -> IO (PreNode i ())
mkHook q parentId hkId nodeChild =
  do
    status <- atomically $ newTVar Unintialised
    rslt <- newEmptyTMVarIO
    pure
      PN.AnyHook
        { hookAddress = hid, -- used in testing
          hookStatus = status,
          hookResult = rslt,
          hook = const $ hookStart q parentId hid,
          hookChild = nodeChild,
          hookRelease = \_ -> hookEnd q parentId hid
        }
  where
    hid = fullId parentId hkId

iterationMessage :: Int -> Text
iterationMessage i = "iteration " <> txt i

mkIterations :: TQueue RunEvent -> Text -> Int -> [() -> IO ()]
mkIterations q fixFullId count' =
  let mkIt :: Int -> (() -> IO ())
      mkIt idx = const $ logIteration q fixFullId idx (iterationMessage idx)
   in mkIt <$> [0 .. count' - 1]

superSimplSuite :: TQueue RunEvent -> IO (PreNodeRoot ())
superSimplSuite q =
  pure . PreNodeRoot $ mkFixture q "Root" "Fixture 0" 1

simpleSuiteWithHook :: TQueue RunEvent -> IO (PreNodeRoot ())
simpleSuiteWithHook q = do
  fx <- mkFixture q "Root.Hook 0" "Fixture 0" 1
  pure . PreNodeRoot $ mkHook q "Root" "Hook 0" fx

-- simpleBranchedSuiteWithHook :: TQueue RunEvent -> IO (PreNodeRoot ())
-- simpleBranchedSuiteWithHook q = do
--   fx <- mkFixture q "Root.Hook 0" "Fixture 0" 1
--   pure . PreNodeRoot $ (: []) <$> mkHook q "Root" "Hook 0" fx

tQToList :: TQueue a -> IO [a]
tQToList q =
  reverse <$> recurse []
  where
    recurse l =
      atomically (tryReadTQueue q)
        >>= maybe (pure l) (\a -> recurse (a : l))

boundaryId :: BranchType -> BoundaryType -> RunEvent -> Maybe Text
boundaryId brt bnt = \case
  Boundary {branchType, boundaryType, id} -> branchType == brt && boundaryType == bnt ? Just id $ Nothing
  IterationMessage {} -> Nothing
  Message {} -> Nothing

boundaryId' :: RunEvent -> Maybe Text
boundaryId' = \case
  Boundary {id} -> Just id
  IterationMessage {} -> Nothing
  Message {} -> Nothing

isBoundary :: RunEvent -> Bool
isBoundary = \case
  Boundary {} -> True
  IterationMessage {} -> False
  Message {} -> False

isHook :: RunEvent -> Bool
isHook re = isBoundary re && branchType re == SuiteRuntimeTest.Hook

mboundaryType :: RunEvent -> Maybe BoundaryType
mboundaryType = \case
  Boundary {boundaryType = bt} -> Just bt
  IterationMessage {} -> Nothing
  Message {} -> Nothing

chkHooks :: [NodeStats] -> [RunEvent] -> IO ()
chkHooks stats evntLst =
  let hks = filter isHookStats stats
      expectedHkIds = (id :: NodeStats -> Text) <$> hks
      bndrys = filter isBoundary evntLst
      actualHooks = filter isHook bndrys
      actualHkIds = nub . catMaybes $ boundaryId' <$> actualHooks

      chkStartAndEnd :: Text -> [RunEvent] -> IO ()
      chkStartAndEnd msg = \case
        [] -> chkFail $ msg <> " - expected start and end boundary but no boundarry found"
        [bnd] -> chkFail $ msg <> " - expected start and end boundary but only one boundarry found " <> txt bnd
        [start, end] -> do
          -- hook should log a start before it ends
          chkEq' (msg <> " " <> txt start) (Just Start) (mboundaryType start)
          chkEq' (msg <> " " <> txt end) (Just End) (mboundaryType end)
        a -> chkFail $ msg <> " too many boundary events" <> txt a

      chkHkExists :: Text -> IO ()
      chkHkExists id' =
        let hkbds =
              filter
                ( \n ->
                    boundaryId SuiteRuntimeTest.Hook Start n == Just id'
                      || boundaryId SuiteRuntimeTest.Hook End n == Just id'
                )
                bndrys
         in chkStartAndEnd ("hook: " <> id') hkbds

      chkUnexpectedHooks :: IO ()
      chkUnexpectedHooks = chkEq' "Unexpected or empty hooks in actual" [] (actualHkIds \\ expectedHkIds)

      chkSubElements :: Text -> IO ()
      chkSubElements nId =
        let hooksStats = fromJust $ find (\s -> nId == (id :: NodeStats -> Text) s) hks
            actualSubElms =
              groupBy (\e1 e2 -> boundaryId' e1 == boundaryId' e2)
                . filter (\re -> ((\id' -> isPrefixOf nId id' && nId /= id') <$> boundaryId' re) == Just True)
                . dropWhileEnd (\re -> boundaryId' re /= Just nId)
                . dropWhile (\re -> boundaryId' re /= Just nId)
                $ bndrys
            actualSubHooks = filter (\g -> Just True == ((SuiteRuntimeTest.Hook ==) . branchType <$> head g)) actualSubElms
            actualSubFixtures = filter (\g -> Just True == ((SuiteRuntimeTest.Fixture ==) . branchType <$> head g)) actualSubElms
         in do
              chkEq' "expected hook count" (hookCount hooksStats) (length actualSubHooks)
              chkEq' "expected fixture count" (fixtureCount hooksStats) (length actualSubFixtures)
              traverse_ (chkStartAndEnd "sub-hooks") actualSubHooks
              traverse_ (chkStartAndEnd "sub-fixture") actualSubFixtures
   in do
        traverse_ chkHkExists expectedHkIds
        chkUnexpectedHooks
        traverse_ chkSubElements expectedHkIds

chkFixtures :: [NodeStats] -> [RunEvent] -> IO ()
chkFixtures stats evntLst =
  let fixIds bt = catMaybes $ boundaryId SuiteRuntimeTest.Fixture bt <$> evntLst
      fixStarts = fixIds Start
      fixEnds = fixIds End

      fixStatCount = count isFixtureStats stats

      chkFixture :: NodeStats -> IO ()
      chkFixture = \case
        HookStats {} -> pure ()
        FixtureStats {id, parent, iterationCount} ->
          let matchesFix :: RunEvent -> Bool
              matchesFix = \case
                Boundary {branchType, id = id'} -> branchType == SuiteRuntimeTest.Fixture && id' == id
                IterationMessage {parentFix} -> parentFix == id
                Message {} -> False

              evntsToChk :: [RunEvent]
              evntsToChk = P.filter matchesFix evntLst

              emptyFix = iterationCount == 0

              iternIdx :: RunEvent -> Maybe Int
              iternIdx = \case
                Boundary {} -> Nothing
                IterationMessage {index} -> Just index
                Message {} -> Nothing

              itrLst :: [Int]
              itrLst = catMaybes $ iternIdx <$> evntsToChk

              itrIdxs :: ST.Set Int
              itrIdxs = ST.fromList itrLst

              expectedIterationIdxs :: ST.Set Int
              expectedIterationIdxs = emptyFix ? ST.empty $ ST.fromList [0 .. iterationCount - 1]

              isIteration :: RunEvent -> Bool
              isIteration = \case
                Boundary {} -> False
                IterationMessage {} -> True
                Message {} -> False

              firstEv = head evntsToChk >>= mboundaryType
              lastEv = last evntsToChk >>= mboundaryType
           in emptyFix
                ? chk' ("Fixture: " <> id <> " has no iterations - no related run events should be logged") (null evntsToChk)
                $ do
                  null evntsToChk
                    ?
                    -- run events for fixture must exist
                    chkFail ("Fixture: " <> id <> " has " <> txt iterationCount <> " iterations but no run events ")
                    $ do
                      -- the first event must be
                      chkEq' "first event should be fixture start" (Just Start) firstEv
                      chkEq' "last event should be fixture end" (Just End) lastEv
                      -- iteration counts should match
                      chkEq' "iteration count" iterationCount $ length itrLst
                      chkEq' "iteration indexes" expectedIterationIdxs itrIdxs
   in do
        chkEq' "expected fixture start count" fixStatCount $ length fixStarts
        chkEq' "expected fixture end count" fixStatCount $ length fixEnds
        for_ stats chkFixture

exeSuiteTests :: (TQueue RunEvent -> IO (PreNodeRoot ())) -> Int -> IO ()
exeSuiteTests preSuite threadCount = do
  q <- atomically newTQueue
  preSuite' <- preSuite q
  stats <- getStats preSuite'
  S.execute threadCount preSuite'
  l <- tQToList q
  putStrLn ""
  putStrLn "============ Stats ============"
  pPrint stats
  putStrLn ""
  putStrLn "============ Logs ============"
  pPrint l
  chkFixtures stats l
  chkHooks stats l

-- $> unit_simple_single

unit_simple_single :: IO ()
unit_simple_single = do
  exeSuiteTests superSimplSuite 1

-- $> unit_simple_with_hook

unit_simple_with_hook :: IO ()
unit_simple_with_hook =
  replicateM_ 1 $
    exeSuiteTests simpleSuiteWithHook 1

{- TODO
  ~ DONE: chkHks
  ~ thread level hooks
    ~ add branch constructor
    ~ update tests
    ~ add thread level
    ~ update tests
    ~ add iteration level
    ~ update tests
  ~ test with empty:
    ~ fixtures
    ~ hooks
      ~ singleton
      ~ thread-level
      ~ iteration level
  ~ simple multiple hooks / iterations / threads
  ~ simple exceptions
    ~ iteration
    ~ singleton hook
    ~ thread-level hook
    ~ update stats expectations
  ~ simple exceptions
  ~ property based - inc differening times / hook types / hook fixture counts
   ~ write generators
   ~ update stats expectations
  ~ killing run
  ~ fixture thread limits
-}
