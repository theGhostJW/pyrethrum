module SuiteRuntimeTest where

import Check (Checks)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import DSL.Interpreter
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
    (>>=),
    (?),
  )
import Pyrelude.Test (chk', chkFail)
import Pyrelude.Test as T hiding (maybe)
import TempUtils (debugLines)
import Text.Show.Pretty (pPrint, pPrintList, ppShow, ppShowList)
import UnliftIO.Concurrent as C
  ( ThreadId,
    forkFinally,
    forkIO,
    myThreadId,
    threadDelay,
  )
import UnliftIO.STM
import Prelude (Ord)

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

getStats :: PreNodeRoot a -> [NodeStats]
getStats PreNodeRoot {children} =
  let isFixture :: PreNode a b -> Bool
      isFixture = \case
        PN.Hook {} -> False
        PN.Fixture {} -> True

      isHook :: PreNode a b -> Bool
      isHook = \case
        PN.Hook {} -> True
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
                      fixtureCount = count isFixture hookChildren,
                      hookCount = count isHook hookChildren
                    }
             in thisNode : (zip [0 ..] hookChildren >>= uncurry (getStats' thisId))
          PN.Fixture {iterations} ->
            [ FixtureStats
                { id = parentId <> ".Fixture " <> txt subIndex,
                  parent = parentId,
                  iterationCount = length iterations
                }
            ]
   in zip [0 ..] children >>= uncurry (getStats' "Root")

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
logIteration q fxTxt iidx itMsg = logEvent q (IterationMessage fxTxt iidx itMsg)

logMessage :: TQueue RunEvent -> Text -> IO ()
logMessage q txt' = logEvent q (Message txt')

-- remove when pyrelude updated
chkEq' t = assertEqual (toS t)

mkFixture :: TQueue RunEvent -> Text -> Text -> Int -> PreNode () ()
mkFixture q parentId fxId itCount =
  PN.Fixture
    { fixtureAddress = fid,
      logStart = fixtureStart q parentId fid,
      iterations = mkIterations q fid itCount,
      logEnd = fixtureEnd q parentId fid
    }
  where
    fid = fullId parentId fxId

iterationMessage :: Int -> Text
iterationMessage i = "iteration " <> txt i

mkIterations :: TQueue RunEvent -> Text -> Int -> [() -> IO ()]
mkIterations q fixFullId count' =
  let mkIt :: Int -> (() -> IO ())
      mkIt idx = const (logIteration q fixFullId idx $ iterationMessage idx)
   in mkIt <$> [0 .. count' - 1]

superSimplSuite :: TQueue RunEvent -> PreNodeRoot ()
superSimplSuite q =
  PreNodeRoot
    [ mkFixture q "Root" "Fixture 0" 1
    ]

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

chkFixtures :: [NodeStats] -> [RunEvent] -> IO ()
chkFixtures stats lstRE =
  let fixIds bt = catMaybes $ boundaryId SuiteRuntimeTest.Fixture bt <$> lstRE
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
              evntsToChk = P.filter matchesFix lstRE

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

              boundaryType :: RunEvent -> Maybe BoundaryType
              boundaryType = \case
                Boundary {boundaryType = bt} -> Just bt
                IterationMessage {} -> Nothing
                Message {} -> Nothing

              isIteration :: RunEvent -> Bool
              isIteration = \case
                Boundary {} -> False
                IterationMessage {} -> True
                Message {} -> False

              firstEv = head evntsToChk >>= boundaryType
              lastEv = last evntsToChk >>= boundaryType
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

exeSuiteTests :: (TQueue RunEvent -> PreNodeRoot ()) -> Int -> IO ()
exeSuiteTests preSuite threadCount = do
  q <- atomically newTQueue
  let preSuite' = preSuite q
      stats = getStats preSuite'
  S.execute threadCount $ S.linkParents preSuite'
  l <- tQToList q
  pPrint $ getStats preSuite'
  pPrint l
  chkFixtures stats l

-- $> unit_simple_single
unit_simple_single :: IO ()
unit_simple_single = do
  exeSuiteTests superSimplSuite 1
