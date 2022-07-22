module Internal.RunTimeLogging where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad.State
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Pyrelude (Applicative (pure), Enum (succ), Eq, Exception (displayException), IO, IORef, Int, Num ((+)), Ord, Show, SomeException, Text, coerce, modifyIORef, readIORef, txt, uu, writeIORef, ($), (.))

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
logger :: Sink -> IORef Index -> (Index -> PThreadId -> ExeEvent) -> IO ()
logger sink threadCounter fEvnt = do
  iOld <- readIORef threadCounter
  let nxt = Index . succ $ idx iOld
  tid <- myThreadId
  sink $ fEvnt nxt (PThreadId $ txt tid)
  writeIORef threadCounter nxt

$(deriveJSON defaultOptions ''ExeEventType)
$(deriveToJSON defaultOptions ''PThreadId)
$(deriveToJSON defaultOptions ''Loc)
$(deriveToJSON defaultOptions ''PException)
$(deriveToJSON defaultOptions ''Index)
$(deriveToJSON defaultOptions ''ExeEvent)