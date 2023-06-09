module Internal.RunTimeLogging where

import qualified BasePrelude as P
import Control.Monad.State (Monad ((>>), (>>=)))
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Data.Set
import GHC.Show (show)
import PyrethrumExtras (toS, txt)
import Text.Show.Pretty (pPrint)
import UnliftIO (TChan, TQueue, atomically, finally, newChan, newTChan, newTChanIO, newTQueue, newTQueueIO, readTChan, writeChan, writeTChan, writeTQueue)
import UnliftIO.Concurrent (ThreadId, myThreadId)
import Prelude hiding (atomically, lines)

data Loc
  = Root
  | Node
      { parent :: Loc
      , tag :: Text
      }
  deriving (Show, Eq, Ord)

data ExeEventType
  = OnceHook
  | OnceHookRelease
  | ThreadHook
  | ThreadHookRelease
  | FixtureOnceHook
  | FixtureOnceHookRelease
  | FixtureThreadHook
  | FixtureThreadHookRelease
  | TestHook
  | TestHookRelease
  | Group
  | Fixture
  | Test
  deriving (Show, Eq, Ord, Enum)

isThreadedEvent :: ExeEventType -> Bool
isThreadedEvent = not . isOnceEvent

isOnceEvent :: ExeEventType -> Bool
isOnceEvent = \case
  OnceHook -> True
  OnceHookRelease -> True
  ThreadHook -> False
  ThreadHookRelease -> False
  FixtureOnceHook -> True
  FixtureOnceHookRelease -> True
  FixtureThreadHook -> False
  FixtureThreadHookRelease -> False
  TestHook -> False
  TestHookRelease -> False
  Group -> False
  Fixture -> False
  Test -> False

isGrouping :: ExeEventType -> Bool
isGrouping = \case
  OnceHook -> False
  ThreadHook -> False
  TestHook -> False
  FixtureThreadHook -> False
  FixtureOnceHook -> False
  FixtureThreadHookRelease -> False
  FixtureOnceHookRelease -> False
  OnceHookRelease -> False
  ThreadHookRelease -> False
  TestHookRelease -> False
  Group -> True
  Fixture -> True
  Test -> False

isFixtureChild :: ExeEventType -> Bool
isFixtureChild = \case
  OnceHook -> False
  ThreadHook -> False
  TestHook -> True
  FixtureThreadHook -> True
  FixtureOnceHook -> True
  FixtureThreadHookRelease -> True
  FixtureOnceHookRelease -> True
  OnceHookRelease -> False
  ThreadHookRelease -> False
  TestHookRelease -> True
  Group -> False
  Fixture -> False
  Test -> True

endIsTerminal :: ExeEventType -> Bool
endIsTerminal = \case
  FixtureThreadHookRelease -> True
  FixtureOnceHookRelease -> True
  OnceHookRelease -> True
  ThreadHookRelease -> True
  TestHookRelease -> True
  Group -> True
  Fixture -> True
  Test -> True
  OnceHook -> False
  ThreadHook -> False
  TestHook -> False
  FixtureThreadHook -> False
  FixtureOnceHook -> False

exceptionTxt :: SomeException -> PException
exceptionTxt e = PException $ txt <$> P.lines (displayException e)

mkFailure :: l -> ExeEventType -> Text -> SomeException -> ExeEvent l a
mkFailure l et t e = Failure t (exceptionTxt e) et l

mkParentFailure :: l -> ExeEventType -> l -> ExeEventType -> SomeException -> ExeEvent l a
mkParentFailure fl fet pl pet ex =
  ParentFailure
    { exception = exceptionTxt ex
    , loc = fl
    , fEventType = fet
    , parentLoc = pl
    , parentEventType = pet
    }

newtype PException = PException {displayText :: [Text]} deriving (Show, Eq, Ord)
newtype SThreadId = SThreadId {display :: Text} deriving (Show, Eq, Ord)

data Log a = Log
  { threadId :: SThreadId
  , idx :: Int
  , event :: a
  }
  deriving (Show)

data ExeEvent l a
  = StartExecution
  | Start
      { eventType :: ExeEventType
      , loc :: l
      }
  | End
      { eventType :: ExeEventType
      , loc :: l
      }
  | Failure
      { msg :: Text
      , exception :: PException
      , parentEventType :: ExeEventType
      , loc :: l
      }
  | ParentFailure
      { exception :: PException
      , loc :: l
      , fEventType :: ExeEventType
      , parentLoc :: l
      , parentEventType :: ExeEventType
      }
  | ApEvent
      { event :: a
      }
  | EndExecution
  deriving (Show)

  

-------  IO Logging --------
type ExeLog l a = Log (ExeEvent l a)

-- not used in concurrent code ie. one IORef per thread
-- this approach means I can't write a pure logger but I can live with that for now
mkLogger :: (ExeLog l a -> IO ()) -> IORef Int -> ThreadId -> ExeEvent l a -> IO ()
mkLogger sink threadCounter thrdId mkExeEvnt = do
  tc <- readIORef threadCounter
  let nxt = succ tc
  finally (sink $ Log (SThreadId $ txt thrdId) nxt mkExeEvnt) $ writeIORef threadCounter nxt

data LogControls m l a = LogControls
  { sink :: ExeLog l a -> IO ()
  , logWorker :: IO ()
  , stopWorker :: IO ()
  , log :: m (TQueue (ExeLog l a))
  }

testLogControls :: forall l a. (Show l, Show a) => TChan (Maybe (ExeLog l a)) -> TQueue (ExeLog l a) -> IO (LogControls Maybe l a)
testLogControls chn log = do
  -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln
  let logWorker :: IO ()
      logWorker =
        atomically (readTChan chn)
          >>= maybe
            (pure ())
            (\evt -> pPrint evt >> logWorker)

      stopWorker :: IO ()
      stopWorker = atomically $ writeTChan chn Nothing

      sink :: ExeLog l a -> IO ()
      sink eventLog =
        atomically $ do
          writeTChan chn $ Just eventLog
          writeTQueue log eventLog

  pure . LogControls sink logWorker stopWorker $ Just log

$(deriveToJSON defaultOptions ''SThreadId)
$(deriveJSON defaultOptions ''ExeEventType)
$(deriveToJSON defaultOptions ''Loc)
$(deriveToJSON defaultOptions ''PException)
$(deriveToJSON defaultOptions ''ExeEvent)