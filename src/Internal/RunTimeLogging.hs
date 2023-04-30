module Internal.RunTimeLogging where

import Control.Monad.State ( Monad((>>=), (>>)) )
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import GHC.Show (show)
import Text.Show.Pretty (pPrint)
import UnliftIO (TChan, TQueue, atomically, newChan, newTChan, newTChanIO, newTQueue, newTQueueIO, readTChan, writeChan, writeTChan, writeTQueue)
import UnliftIO.Concurrent (myThreadId, ThreadId) 
import Data.Set
import Prelude hiding (atomically, lines)
import qualified BasePrelude as P
import PyrethrumExtras

data Loc
  = Root
  | Node
      { parent :: Loc,
        tag :: Text
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

mkFailure :: l -> ExeEventType -> Text -> SomeException -> Int -> SThreadId -> ExeEvent l
mkFailure l et t e = Failure t (exceptionTxt e) et l

mkParentFailure :: l -> ExeEventType -> l -> ExeEventType -> SomeException -> Int -> SThreadId -> ExeEvent l
mkParentFailure fl fet pl pet ex idx trd = ParentFailure  { 
        exception = exceptionTxt ex,
        loc = fl,
        fEventType = fet,
        parentLoc = pl,
        parentEventType = pet,
        idx = idx,
        threadId = trd
      } 


newtype PException = PException {displayText :: [Text]} deriving (Show, Eq, Ord)
newtype SThreadId = SThreadId { display :: Text} deriving (Show, Eq, Ord)

data ExeEvent l
  = StartExecution
      { idx :: Int,
        threadId :: SThreadId
      }
  | Start
      { 
        eventType :: ExeEventType,
        loc :: l,
        idx :: Int,
        threadId :: SThreadId
      }
  | End
      { 
        eventType :: ExeEventType,
        loc :: l,
        idx :: Int,
        threadId :: SThreadId
      }
  | Failure
      { 
        msg :: Text,
        exception :: PException,
        parentEventType :: ExeEventType,
        loc :: l,
        idx :: Int,
        threadId :: SThreadId
      }
  | ParentFailure
      { 
        exception :: PException,
        loc :: l,
        fEventType :: ExeEventType,
        parentLoc :: l,
        parentEventType :: ExeEventType,
        idx :: Int,
        threadId :: SThreadId
      }
  | ApLog
      { msg :: Text,
        idx :: Int,
        threadId :: SThreadId
      }
  | EndExecution
      { idx :: Int,
        threadId :: SThreadId
      }
  deriving (Show)

-------  IO Logging --------
type EventSink l = ExeEvent l -> IO ()
type MessageLogger = Text -> IO ()

-- not used in concurrent code ie. one IORef per thread
-- this approach means I can't write a pure logger but I can live with that for now
mkLogger :: EventSink l -> IORef Int -> ThreadId -> (Int -> SThreadId -> ExeEvent l) -> IO ()
mkLogger sink threadCounter thrdId fEvnt = do
  tc <- readIORef threadCounter
  let nxt = succ tc
  sink . fEvnt nxt . SThreadId $ txt thrdId
  writeIORef threadCounter nxt

data LogControls m l = LogControls
  { sink :: EventSink l,
    logWorker :: IO (),
    stopWorker :: IO (),
    log :: m (TQueue (ExeEvent l))
  }

testLogControls :: forall l. Show l => TChan (Maybe (ExeEvent l)) -> TQueue (ExeEvent l) -> IO (LogControls  Maybe l)
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

      sink :: ExeEvent l -> IO ()
      sink evnt =
        atomically $ do
          writeTChan chn $ Just evnt
          writeTQueue log evnt

  pure . LogControls sink logWorker stopWorker $ Just log

$(deriveToJSON defaultOptions ''SThreadId)
$(deriveJSON defaultOptions ''ExeEventType)
$(deriveToJSON defaultOptions ''Loc)
$(deriveToJSON defaultOptions ''PException)
$(deriveToJSON defaultOptions ''ExeEvent)