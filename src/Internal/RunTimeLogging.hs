module Internal.RunTimeLogging where

import Control.Monad.State
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import GHC.Show (show)
import Pyrelude (Applicative (pure), Bool, Enum (succ), Eq, Exception (displayException), IO, IORef, Int, Maybe (..), Num ((+)), Ord, Semigroup ((<>)), Show, SomeException, Text, coerce, const, maybe, modifyIORef, print, readIORef, txt, unpack, uu, writeIORef, ($), (.))
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

mkFailure :: Loc -> Text -> SomeException -> Int -> PThreadId -> ExeEvent
mkFailure l t = Failure l t . exceptionTxt

mkParentFailure :: Loc -> Loc -> SomeException -> Int -> PThreadId -> ExeEvent
mkParentFailure p l = ParentFailure p l . exceptionTxt

newtype PThreadId = PThreadId {threadId :: Text} deriving (Show, Eq)

newtype PException = PException {displayText :: Text} deriving (Show, Eq)

data ExeEvent
  = StartExecution
      { idx :: Int,
        trdIdx :: PThreadId
      }
  | Start
      { loc :: Loc,
        event :: ExeEventType,
        idx :: Int,
        trdIdx :: PThreadId
      }
  | End
      { loc :: Loc,
        event :: ExeEventType,
        idx :: Int,
        trdIdx :: PThreadId
      }
  | Failure
      { loc :: Loc,
        msg :: Text,
        exception :: PException,
        idx :: Int,
        trdIdx :: PThreadId
      }
  | ParentFailure
      { loc :: Loc,
        parentLoc :: Loc,
        exception :: PException,
        idx :: Int,
        trdIdx :: PThreadId
      }
  | Debug
      { msg :: Text,
        idx :: Int,
        trdIdx :: PThreadId
      }
  | EndExecution
      { idx :: Int,
        trdIdx :: PThreadId
      }
  deriving (Show)

-------  IO Logging --------
type Sink = ExeEvent -> IO ()

-- not used in concurrent code ie. one IORef per thread
-- this approach means I can't write a pure logger but I can live with that for now
mkLogger :: Sink -> IORef Int -> (Int -> PThreadId -> ExeEvent) -> IO ()
mkLogger sink threadCounter fEvnt = do
  iOld <- readIORef threadCounter
  let nxt = succ iOld
  tid <- myThreadId
  sink $ fEvnt nxt (PThreadId $ txt tid)
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
      sink evnt = do
        atomically $ do
          writeTChan chn $ Just evnt
          writeTQueue log evnt

  pure . LogControls sink logWorker stopWorker $ Just log

$(deriveJSON defaultOptions ''ExeEventType)
$(deriveToJSON defaultOptions ''PThreadId)
$(deriveToJSON defaultOptions ''Loc)
$(deriveToJSON defaultOptions ''PException)
$(deriveToJSON defaultOptions ''ExeEvent)