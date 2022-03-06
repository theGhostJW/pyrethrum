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
import Pyrelude (IO, Int, ListLike (unsafeHead), Show, Text, const, debug_, debugf, maybe, maybef, pure, reverse, txtPretty, uu, ($), (.), (<$>), (>>=))
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

data RunEventType
  = HookStart
  | HookEnd
  | FixtureStart
  | FixtureEnd
  deriving (Show)

data RunEvent
  = Boundary RunEventType Text ThreadId
  | Message Text ThreadId
  deriving (Show)

logEvent :: TQueue RunEvent -> (ThreadId -> RunEvent) -> IO ()
logEvent q ev = do
  i <- myThreadId
  atomically . writeTQueue q $ ev i

logBoundary :: TQueue RunEvent -> RunEventType -> Text -> IO ()
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

logMessage :: TQueue RunEvent -> Text -> IO ()
logMessage q txt = logEvent q (Message txt)

mkFixture :: Text -> Int -> PreNode () ()
mkFixture = 

superSimplSuite :: TQueue RunEvent -> PreNode () ()
superSimplSuite q =
  Root
    [ Fixture
        { logStart = fixtureStart q "started",
          iterations = [const (logMessage q "iteration")],
          logEnd = fixtureEnd q "ended"
        }
    ]

tQToList :: TQueue a -> IO [a]
tQToList q =
  reverse <$> recurse [] q
  where
    recurse :: [a] -> TQueue a -> IO [a]
    recurse l q =
      atomically (tryReadTQueue q)
        >>= maybe
          (pure l)
          (\a -> recurse (a : l) q)

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
