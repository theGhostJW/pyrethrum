module Internal.RunTimeLogging where

import Control.Monad.State
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import GHC.Show (show)
import Pyrelude (Applicative (pure), Bool, Enum (succ), Eq, Exception (displayException), IO, IORef, Int, Maybe (..), Num ((+)), Ord, Semigroup ((<>)), Show, SomeException, Text, ThreadId, coerce, const, maybe, modifyIORef, print, readIORef, txt, unpack, uu, writeIORef, ($), (.), (<$>))
import Text.Show.Pretty (pPrint)
import UnliftIO (TChan, TQueue, atomically, newChan, newTChan, newTChanIO, newTQueue, newTQueueIO, readTChan, writeChan, writeTChan, writeTQueue)
import UnliftIO.Concurrent (myThreadId)
import Prelude (String)

data Loc
  = Root
  | Node Loc Text
  deriving (Show)

data ExeEventType
  = OnceHook
  | OnceHookRelease
  | ThreadHook
  | ThreadHookRelease
  | TestHook
  | TestHookRelease
  | Group
  | Fixture
  | Test
  deriving (Show, Eq)

exceptionTxt :: SomeException -> PException
exceptionTxt = PException . txt . displayException

mkFailure :: Loc -> Text -> SomeException -> Int -> Text -> ExeEvent
mkFailure l t = Failure l t . exceptionTxt

mkParentFailure :: Loc -> Loc -> SomeException -> Int -> Text -> ExeEvent
mkParentFailure p l = ParentFailure p l . exceptionTxt

newtype PException = PException {displayText :: Text} deriving (Show, Eq)

data ExeEvent
  = StartExecution
      { idx :: Int,
        threadId :: Text
      }
  | Start
      { loc :: Loc,
        event :: ExeEventType,
        idx :: Int,
        threadId :: Text
      }
  | End
      { loc :: Loc,
        event :: ExeEventType,
        idx :: Int,
        threadId :: Text
      }
  | Failure
      { loc :: Loc,
        msg :: Text,
        exception :: PException,
        idx :: Int,
        threadId :: Text
      }
  | ParentFailure
      { loc :: Loc,
        parentLoc :: Loc,
        exception :: PException,
        idx :: Int,
        threadId :: Text
      }
  | Debug
      { msg :: Text,
        idx :: Int,
        threadId :: Text
      }
  | EndExecution
      { idx :: Int,
        threadId :: Text
      }
  deriving (Show)

-------  IO Logging --------
type Sink = ExeEvent -> IO ()

-- not used in concurrent code ie. one IORef per thread
-- this approach means I can't write a pure logger but I can live with that for now
mkLogger :: Sink -> IORef Int -> ThreadId -> (Int -> Text -> ExeEvent) -> IO ()
mkLogger sink threadCounter thrdId fEvnt = do
  tc <- readIORef threadCounter
  let nxt = succ tc
  sink . fEvnt nxt $ txt thrdId
  writeIORef threadCounter nxt

data LogControls m = LogControls
  { sink :: Sink,
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

$(deriveJSON defaultOptions ''ExeEventType)
$(deriveToJSON defaultOptions ''Loc)
$(deriveToJSON defaultOptions ''PException)
$(deriveToJSON defaultOptions ''ExeEvent)