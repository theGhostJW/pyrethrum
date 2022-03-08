module SuiteRuntimeTest where

import Check (Checks)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import DSL.Interpreter
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import GHC.Records
import Internal.PreNode as PN
import qualified Internal.SuiteRuntime as S
import Language.Haskell.TH (pprint)
import Polysemy
import Pyrelude (IO, Int, ListLike (unsafeHead), Maybe (Nothing), Show, Text, const, debug_, debugf, maybe, maybef, pure, reverse, txt, txtPretty, uu, ($), (-), (.), (<$>), (<>), (>>=))
import qualified Pyrelude as System.IO
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
  deriving (Show)

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
fixtureEnd q =  SuiteRuntimeTest.fixture q End

logIteration :: TQueue RunEvent -> Text -> Int -> Text -> IO ()
logIteration q fxTxt iidx itMsg = logEvent q (IterationMessage fxTxt iidx itMsg)

logMessage :: TQueue RunEvent -> Text -> IO ()
logMessage q txt' = logEvent q (Message txt')

mkFixture :: TQueue RunEvent -> Text -> Text -> Int -> PreNode () ()
mkFixture q parentId fxId itCount =
  PN.Fixture
    { logStart = fixtureStart q parentId fxId,
      iterations = mkIterations q fxId itCount,
      logEnd = fixtureEnd q parentId fxId
    }

iterationMessage :: Int -> Text
iterationMessage i = "iteration " <> txt i

mkIterations :: TQueue RunEvent -> Text -> Int -> [() -> IO ()]
mkIterations q fixId count =
  let mkIt :: Int -> (() -> IO ())
      mkIt idx = const (logIteration q fixId idx $ iterationMessage idx)
   in mkIt <$> [0 .. count - 1]

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
  S.execute threadCount . S.linkParents $ preSuite q
  l <- tQToList q
  pPrint l

-- $> unit_simple_single
unit_simple_single :: IO ()
unit_simple_single = do
  exeSuiteTests superSimplSuite 1
