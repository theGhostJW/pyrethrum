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
import Polysemy
import Pyrelude (IO, Show, Text, const, debug_, debugf, pure, txtPretty, ($), (.))
import Pyrelude.Test
import Text.Show.Pretty
import UnliftIO.Concurrent as C
  ( ThreadId,
    forkFinally,
    forkIO,
    myThreadId,
    threadDelay,
  )
import UnliftIO.STM
import Language.Haskell.TH (pprint)
import qualified Pyrelude as System.IO

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

logEvent :: STM (TQueue RunEvent) -> (ThreadId -> RunEvent) -> IO ()
logEvent q ev = do
  q' <- atomically q
  i <- myThreadId
  atomically . writeTQueue q' $ debugf txtPretty (ev i)

logBoundary :: STM (TQueue RunEvent) -> RunEventType -> Text -> IO ()
logBoundary q et msg =
  logEvent q (Boundary et msg)

hookStart :: STM (TQueue RunEvent) -> Text -> IO ()
hookStart q = logBoundary q HookStart

hookEnd :: STM (TQueue RunEvent) -> Text -> IO ()
hookEnd q = logBoundary q HookEnd

fixtureStart :: STM (TQueue RunEvent) -> Text -> IO ()
fixtureStart q = logBoundary q FixtureStart

fixtureEnd :: STM (TQueue RunEvent) -> Text -> IO ()
fixtureEnd q = logBoundary q FixtureEnd

logMessage :: STM (TQueue RunEvent) -> Text -> IO ()
logMessage q txt = logEvent q (Message txt)

superSimplSuite :: STM (TQueue RunEvent) -> PreNode () ()
superSimplSuite q =
  Root
    [ Fixture
        { logStart = fixtureStart q "started",
          iterations = [const (logMessage q "iteration")],
          logEnd = fixtureEnd q "ended"
        }
    ]

-- $> unit_simple_single
unit_simple_single :: IO ()
unit_simple_single = do
  System.IO.print "AND WE'RE OFF!"
  S.execute 1 . S.linkParents $ superSimplSuite newTQueue
