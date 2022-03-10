module SuiteRuntimeTest where

import Check (Checks)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import DSL.Interpreter
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import GHC.Records
import Internal.PreNode (PreNode (iterations))
import Internal.PreNode as PN
import qualified Internal.SuiteRuntime as S
import Language.Haskell.TH (pprint)
import Polysemy
import Pyrelude (Bool (..), Eq, IO, Int, ListLike (unsafeHead), Maybe (Nothing), Show, Text, const, count, debug_, debugf, error, length, maybe, maybef, pure, reverse, txt, txtPretty, uncurry, uu, zip, ($), (-), (.), (<$>), (<>), (>>=))
import qualified Pyrelude.Test
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

data BoundaryType
  = Start
  | End
  deriving (Show)

data BranchType
  = Hook
  | Fixture
  deriving (Show, Eq)

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

getStats :: PreNode a b -> [NodeStats]
getStats =
  let isFixture :: PreNode a b -> Bool
      isFixture = \case
        Root _ -> False
        PN.Hook {} -> False
        PN.Fixture {} -> True

      isHook :: PreNode a b -> Bool
      isHook = \case
        Root _ -> False
        PN.Hook {} -> True
        PN.Fixture {} -> False

      getStats' :: Text -> Int -> PreNode a b -> [NodeStats]
      getStats' parentId subIndex =
        \case
          Root _ -> error "should not be called"
          PN.Hook {hookChildren} ->
            let thisId = parentId <> ".Hook" <> txt subIndex
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
                { id = parentId <> ".Fixture" <> txt subIndex,
                  parent = parentId,
                  iterationCount = length iterations
                }
            ]
   in \case
        Root children -> (zip [0 ..] children >>= uncurry (getStats' "Root"))
        PN.Hook {} -> error "should not be called"
        PN.Fixture {} -> error "should not be called"

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
  deriving (Show)

logEvent :: TQueue RunEvent -> (ThreadId -> RunEvent) -> IO ()
logEvent q ev = do
  i <- myThreadId
  atomically . writeTQueue q $ ev i

logBoundary :: TQueue RunEvent -> BranchType -> BoundaryType -> Text -> Text -> IO ()
logBoundary q brt bdt parentId id' =
  logEvent q $ \thrd ->
    Boundary
      { branchType = brt,
        boundaryType = bdt,
        boundaryParentFix = parentId,
        threadId = thrd,
        id = id'
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

mkFixture :: TQueue RunEvent -> Text -> Text -> Int -> PreNode () ()
mkFixture q parentId fxId itCount =
  PN.Fixture
    { fixtureAdddress = fxId,
      logStart = fixtureStart q parentId fxId,
      iterations = mkIterations q fxId itCount,
      logEnd = fixtureEnd q parentId fxId
    }

iterationMessage :: Int -> Text
iterationMessage i = "iteration " <> txt i

mkIterations :: TQueue RunEvent -> Text -> Int -> [() -> IO ()]
mkIterations q fixId count' =
  let mkIt :: Int -> (() -> IO ())
      mkIt idx = const (logIteration q fixId idx $ iterationMessage idx)
   in mkIt <$> [0 .. count' - 1]

superSimplSuite :: TQueue RunEvent -> PreNode () ()
superSimplSuite q =
  Root
    [ mkFixture q "Root" "fixture 0" 1
    ]

tQToList :: TQueue a -> IO [a]
tQToList q =
  reverse <$> recurse []
  where
    recurse l =
      atomically (tryReadTQueue q)
        >>= maybe (pure l) (\a -> recurse (a : l))

exeSuiteTests :: (TQueue RunEvent -> PreNode () ()) -> Int -> IO ()
exeSuiteTests preSuite threadCount = do
  q <- atomically newTQueue
  let preSuite' = preSuite q
  S.execute threadCount $ S.linkParents preSuite'
  l <- tQToList q
  pPrint $ getStats preSuite'
  pPrint l

-- $> unit_simple_single
unit_simple_single :: IO ()
unit_simple_single = do
  exeSuiteTests superSimplSuite 1
