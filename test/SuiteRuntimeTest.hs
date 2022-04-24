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
import Internal.PreNode as PN
import qualified Internal.SuiteRuntime as S
import Polysemy
import Pyrelude as P
  ( Bool (..),
    Eq ((==)),
    IO,
    Int,
    ListLike (foldl', head, null, unsafeHead, unsafeLast),
    Maybe (Just, Nothing),
    Ord (..),
    Show,
    Text,
    Traversable (traverse),
    catMaybes,
    const,
    count,
    debug_,
    debugf,
    error,
    filter,
    for_,
    last,
    length,
    maybe,
    maybef,
    pure,
    reverse,
    singleton,
    toS,
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
    (?), replicateM_, (||), not, traverse_, nub, (\\),
  )
import Pyrelude.Test (chk', chkFail)
import Pyrelude.Test as T hiding (filter, maybe, singleton)
import TempUtils (debugLines)
import Text.Show.Pretty (pPrint, pPrintList, ppShow, ppShowList, PrettyVal (prettyVal))
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

getStats :: PreNodeRoot a -> IO [NodeStats]
getStats PreNodeRoot {children} =
  let nonEmptyFixture :: PreNode a b -> Bool
      nonEmptyFixture = \case
        PN.Hook {} -> False
        f@PN.Fixture {} -> not $ nodeEmpty f

      nonEmptyHook :: PreNode a b -> Bool
      nonEmptyHook = \case
        h@PN.Hook {} -> not $ nodeEmpty h
        PN.Fixture {} -> False

      getStats' :: Text -> Int -> PreNode a b -> [NodeStats]
      getStats' parentId subIndex =
        \case
          PN.Hook {hookChildren} ->
            let thisId = parentId <> ".Hook " <> txt subIndex
                thisNode =
                  HookStats
                    { id = thisId,
                      parent = parentId,
                      fixtureCount = count nonEmptyFixture hookChildren,
                      hookCount = count nonEmptyHook hookChildren
                    }
             in thisNode : (zip [0 ..] hookChildren >>= uncurry (getStats' thisId))
          PN.Fixture {iterations} ->
            [ FixtureStats
                { id = parentId <> ".Fixture " <> txt subIndex,
                  parent = parentId,
                  iterationCount = length iterations
                }
            ]
   in do
        children' <- children
        pure $ zip [0 ..] children' >>= uncurry (getStats' "Root")

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

mkHook :: TQueue RunEvent -> Text -> Text -> [PreNode () o2] -> IO (PreNode i ())
mkHook q parentId hkId nodeChildren =
  do
    status <- atomically $ newTVar Unintialised
    rslt <- newEmptyTMVarIO
    pure
      PN.Hook
        { hookAddress = hid, -- used in testing
          hookStatus = status,
          hookResult = rslt,
          hook = const $ hookStart q parentId hid,
          hookChildren = nodeChildren,
          hookRelease = \_ _ -> hookEnd q parentId hid
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
  pure . PreNodeRoot $ (: []) <$> mkFixture q "Root" "Fixture 0" 1


simpleSuiteWithHook :: TQueue RunEvent -> IO (PreNodeRoot ())
simpleSuiteWithHook q = do
      fx <- mkFixture q "Root.Hook 0" "Fixture 0" 1
      pure . PreNodeRoot $ (: []) <$> mkHook q "Root" "Hook 0" [fx]

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

mboundaryType :: RunEvent -> Maybe BoundaryType
mboundaryType = \case
  Boundary {boundaryType = bt} -> Just bt
  IterationMessage {} -> Nothing
  Message {} -> Nothing

chkHooks :: [NodeStats] -> [RunEvent] -> IO ()
chkHooks stats evntLst = 
  let 
    hks = filter isHookStats stats
    expectedHkIds = (id :: NodeStats -> Text) <$> hks
    bndrys = filter isBoundary evntLst

    chkHkExists ::  Text -> IO ()
    chkHkExists id' = 
      let 
        hkbds = filter (
                        \n -> boundaryId SuiteRuntimeTest.Hook Start n == Just id' || 
                              boundaryId SuiteRuntimeTest.Hook End n == Just id'
                        ) bndrys
      in do
        case hkbds of 
          [] -> chkFail $ "Expected start and end for hook id: " <> id' <> " but no hook boundary was found"
          [bnd] -> chkFail $ "Expected start and end for hook id: " <> id' <> " but only single boundary found: " <> txt bnd
          [start, end] -> do 
            -- hook should log a start before it ends
            Just Start ... mboundaryType start
            Just End ... mboundaryType end
          a -> chkFail $ "Too many boundary events for hook id: " <> id' <> txt a

    chkUnexpectedHooks :: IO ()
    chkUnexpectedHooks = let 
      actualHkIds = nub . catMaybes $ boundaryId' <$> bndrys
     in 
       chkEq' "Unexpected or empty hooks in actual" [] (actualHkIds \\ expectedHkIds) 
  in 
    do 
      traverse_ chkHkExists expectedHkIds
      chkUnexpectedHooks


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
  ~ chkHks
  ~ thread level hooks - should be easy just execute once on fork fixture thread
  ~ test with empty:
    ~ fixtures
    ~ hooks 
      ~ singleton
      ~ thread-level
  ~ simple multiple hooks / iterations / threads
  ~ simple exceptions
    ~ iteration
    ~ singleton hook
    ~ thread-level hook
    ~ update stats expectations
  ~ simple exceptions
  ~ property based - inc differening times / hook types / hook fixture counts 
   ~ write genrators
   ~ update stats expectations
  ~ killing run
-}
