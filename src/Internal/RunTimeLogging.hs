module Internal.RunTimeLogging where

import Control.Monad.State
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Pyrelude (Applicative (pure), Enum (succ), Eq, Exception (displayException), IO, IORef, Int, Maybe (..), Num ((+)), Ord, Show, SomeException, Text, coerce, maybe, modifyIORef, readIORef, txt, uu, writeIORef, ($), (.))
import Text.Show.Pretty (pPrint)
import UnliftIO (TQueue, atomically, newChan, newTChan, newTChanIO, newTQueue, newTQueueIO, readTChan, writeChan, writeTChan, writeTQueue)
import UnliftIO.Concurrent (myThreadId)

mkStart :: Loc -> ExeEventType -> Index -> PThreadId -> ExeEvent
mkStart l e i t = Start i t l e

mkDebug :: Text -> Index -> PThreadId -> ExeEvent
mkDebug t i p = Debug i p t

mkFailure :: Loc -> Text -> SomeException -> Index -> PThreadId -> ExeEvent
mkFailure loc desc e idx trdId = Failure idx trdId loc desc (PException . txt $ displayException e)

newtype Loc = Loc {unLoc :: Text} deriving (Show, Eq, Ord)

data ExeEventType
  = OnceHook
  | OnceHookRelease
  | ThreadHook
  | ThreadHookRelease
  | TestHook
  | TestHookRelease
  | Group
  | Fixture
  deriving (Show, Eq)

newtype PThreadId = PThreadId {threadId :: Text} deriving (Show, Eq)

newtype PException = PException {displayText :: Text} deriving (Show, Eq)

newtype Index = Index {idx :: Int} deriving (Show, Eq, Ord)

data ExeEvent
  = StartExecution Index PThreadId
  | Start Index PThreadId Loc ExeEventType
  | Failure Index PThreadId Loc Text PException
  | Debug Index PThreadId Text
  | EndExecution Index PThreadId
  deriving (Show)

-------  IO Logging --------
type Sink = ExeEvent -> IO ()

-- not used in concurrent code ie. one IORef per thread
-- this approach means I can't write a pure logger but I can live with that for now
mkLogger :: Sink -> IORef Index -> (Index -> PThreadId -> ExeEvent) -> IO ()
mkLogger sink threadCounter fEvnt = do
  iOld <- readIORef threadCounter
  let nxt = Index . succ $ idx iOld
  tid <- myThreadId
  sink $ fEvnt nxt (PThreadId $ txt tid)
  writeIORef threadCounter nxt

data LogControls = LogControls
  { sink :: Sink,
    logWorker :: IO (),
    stopWorker :: IO (),
    log :: Maybe (TQueue ExeEvent)
  }

testLogControls :: IO LogControls
testLogControls = do
  -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln
  chn <- newTChanIO
  log <- newTQueueIO

  let logWorker :: IO ()
      logWorker =
        atomically (readTChan chn)
          >>= maybe
            (pure ())
            (\evnt -> pPrint evnt >> logWorker)

      stopWorker :: IO ()
      stopWorker = atomically $ writeTChan chn Nothing

      sink :: ExeEvent -> IO ()
      sink evnt = do
        atomically $ do
          writeTChan chn $ Just evnt
          writeTQueue log evnt

  pure $ LogControls sink logWorker stopWorker $ Just log


$(deriveJSON defaultOptions ''ExeEventType)
$(deriveToJSON defaultOptions ''PThreadId)
$(deriveToJSON defaultOptions ''Loc)
$(deriveToJSON defaultOptions ''PException)
$(deriveToJSON defaultOptions ''Index)
$(deriveToJSON defaultOptions ''ExeEvent)