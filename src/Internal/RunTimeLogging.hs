module Internal.RunTimeLogging where

import Control.Monad.State
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import GHC.Show (show)
import Pyrelude
  ( Applicative (pure),
    Bool(..),
    Enum (succ),
    Eq,
    Exception (displayException),
    IO,
    IORef,
    Int,
    Maybe (..),
    Num ((+)),
    Ord,
    Semigroup ((<>)),
    Show,
    SomeException,
    Text,
    ThreadId,
    coerce,
    const,
    maybe,
    modifyIORef,
    print,
    readIORef,
    txt,
    unpack,
    uu,
    writeIORef,
    ($),
    (.),
    (<$>),
  )
import Text.Show.Pretty (pPrint)
import UnliftIO (TChan, TQueue, atomically, newChan, newTChan, newTChanIO, newTQueue, newTQueueIO, readTChan, writeChan, writeTChan, writeTQueue)
import UnliftIO.Concurrent (myThreadId)
import Prelude (String, lines)

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
  deriving (Show, Eq)

endIsTerminal :: ExeEventType -> Bool
endIsTerminal = \case
  OnceHook -> False
  ThreadHook -> False
  TestHook -> False
  FixtureThreadHook -> False
  FixtureOnceHook -> False
  FixtureThreadHookRelease -> True
  FixtureOnceHookRelease -> True
  OnceHookRelease -> True
  ThreadHookRelease -> True
  TestHookRelease -> True
  Group -> True
  Fixture -> True
  Test -> True

exceptionTxt :: SomeException -> PException
exceptionTxt e = PException $ txt <$> lines (displayException e)

mkFailure :: Loc -> Text -> SomeException -> Int -> SThreadId -> ExeEvent
mkFailure l t = Failure l t . exceptionTxt

mkParentFailure :: Loc -> Loc -> ExeEventType -> SomeException -> Int -> SThreadId -> ExeEvent
mkParentFailure p l et = ParentFailure p l et . exceptionTxt

newtype PException = PException {displayText :: [Text]} deriving (Show, Eq)
newtype SThreadId = SThreadId { display :: Text} deriving (Show, Eq, Ord)

data ExeEvent
  = StartExecution
      { idx :: Int,
        threadId :: SThreadId
      }
  | Start
      { loc :: Loc,
        eventType :: ExeEventType,
        idx :: Int,
        threadId :: SThreadId
      }
  | End
      { loc :: Loc,
        eventType :: ExeEventType,
        idx :: Int,
        threadId :: SThreadId
      }
  | Failure
      { loc :: Loc,
        msg :: Text,
        exception :: PException,
        idx :: Int,
        threadId :: SThreadId
      }
  | ParentFailure
      { loc :: Loc,
        parentLoc :: Loc,
        parentEventType :: ExeEventType,
        exception :: PException,
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
type EventSink = ExeEvent -> IO ()
type ApLogger = Text -> IO ()

-- not used in concurrent code ie. one IORef per thread
-- this approach means I can't write a pure logger but I can live with that for now
mkLogger :: EventSink -> IORef Int -> ThreadId -> (Int -> SThreadId -> ExeEvent) -> IO ()
mkLogger sink threadCounter thrdId fEvnt = do
  tc <- readIORef threadCounter
  let nxt = succ tc
  sink . fEvnt nxt . SThreadId $ txt thrdId
  writeIORef threadCounter nxt

data LogControls m = LogControls
  { sink :: EventSink,
    logWorker :: IO (),
    stopWorker :: IO (),
    log :: m (TQueue ExeEvent)
  }

testLogControls :: TChan (Maybe ExeEvent) -> TQueue ExeEvent -> IO (LogControls Maybe)
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

      sink :: ExeEvent -> IO ()
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