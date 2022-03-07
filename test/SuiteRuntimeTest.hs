module SuiteRuntimeTest where

import Check (Checks)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import DSL.Interpreter
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import GHC.Records
import Internal.PreNode
import qualified Internal.SuiteRuntime as S
import Language.Haskell.TH (pprint)
import Polysemy
import Pyrelude (IO, Int, ListLike (unsafeHead), Show, Text, const, debug_, debugf, maybe, maybef, pure, reverse, txt, txtPretty, uu, ($), (-), (.), (<$>), (<>), (>>=))
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
  = Boundary BranchType BoundaryType Text ThreadId
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

logBoundary :: TQueue RunEvent -> BranchType -> Text -> IO ()
logBoundary q et msg =
  logEvent q (Boundary et msg)

hookStart :: TQueue RunEvent -> Text -> IO ()
hookStart q = logBoundary q HookStart

hookEnd :: TQueue RunEvent -> Text -> IO ()
hookEnd q = logBoundary q HookEnd

fixtureStart :: TQueue RunEvent -> Text -> IO ()
fixtureStart q = logBoundary q FixtureStart

fixtureEnd :: TQueue RunEvent -> Text -> IO ()
fixtureEnd q = logBoundary q FixtureEnd

logIteration :: TQueue RunEvent -> Text -> Int -> Text -> IO ()
logIteration q fxTxt iidx itMsg = logEvent q (IterationMessage fxTxt iidx itMsg)

logMessage :: TQueue RunEvent -> Text -> IO ()
logMessage q txt' = logEvent q (Message txt')

mkFixture :: TQueue RunEvent -> Text -> Int -> PreNode () ()
mkFixture q fxprefix itCount =
  Fixture
    { logStart = fixtureStart q fxprefix,
      iterations = mkIterations q fxprefix itCount,
      logEnd = fixtureEnd q fxprefix
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
    [ mkFixture q "fixture 0" 1
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
