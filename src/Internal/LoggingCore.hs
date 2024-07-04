{-# LANGUAGE DeriveAnyClass #-}

module Internal.LoggingCore where

-- TODO: Explicit exports remove old code
import BasePrelude qualified as P
import DSL.Internal.NodeEvent qualified as AE
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Text as T (intercalate)
import Effectful.Concurrent.STM (TQueue)
import CoreUtils qualified as C
import PyrethrumExtras as PE (txt, (?), head, tail) 
import Text.Show.Pretty (pPrint)
import UnliftIO (finally, concurrently_, newIORef)
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (atomically, newTChanIO, newTQueueIO, readTChan, writeTChan, writeTQueue)
import Prelude hiding (atomically, lines)


-- adds log index and thread id to loggable event and sends it to the sink
mkLogger :: forall l lxp. (TE.ThreadId -> Int -> l -> lxp) -> (lxp -> IO ()) -> IORef Int -> ThreadId -> l -> IO ()
mkLogger expander sink threadCounter thrdId logEvnt  = do
  tc <- readIORef threadCounter
  let nxt = succ tc
  finally (sink $ expander (TE.mkThreadId thrdId) nxt logEvnt) $ writeIORef threadCounter nxt

-- TODO:: Logger should be wrapped in an except that sets non-zero exit code on failure

data LogControls l lx = LogControls
  { 
    expander :: TE.ThreadId -> Int -> l -> lx
  , sink :: lx -> IO ()
  , logWorker :: IO ()
  , stopWorker :: IO ()
  , log :: TQueue lx
  }

testLogControls :: forall l lx. (Show lx) => Bool -> IO (LogControls l lx, TQueue l)
testLogControls wantConsole = do
  chn <- newTChanIO
  logQ <- newTQueueIO

  -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln
  let logWorker :: IO ()
      logWorker =
        atomically (readTChan chn)
          >>= maybe
            (pure ())
            (\evt -> when wantConsole (pPrint evt) >> logWorker)

      stopWorker :: IO ()
      stopWorker = atomically $ writeTChan chn Nothing

      sink :: l -> IO ()
      sink eventLog =
        atomically $ do
          writeTChan chn $ Just eventLog
          writeTQueue logQ eventLog

  pure (LogControls sink logWorker stopWorker logQ, logQ)

data LoggerSource l = MkLoggerSource
  { rootLogger :: l -> IO (),
    newLogger :: IO (l -> IO ())
  }

runWithLogger :: forall l lx. LogControls l lx -> (LoggerSource l -> IO ()) -> IO ()
runWithLogger
  LogControls
    { sink,
      expander,
      logWorker,
      stopWorker
    }
  action =
    do
      rootLogger <- mkNewLogger
      let loggerSource = MkLoggerSource rootLogger mkNewLogger
      -- logWorker and execution run concurrently
      -- logworker serialises the log events emitted by the execution
      concurrently_
        logWorker
        ( finally
            (action loggerSource)
            stopWorker
        )
    where
      mkNewLogger :: IO (l -> IO ())
      mkNewLogger = mkLogger expander sink <$> UnliftIO.newIORef (-1) <*> P.myThreadId

testLogControls :: forall l lx. (Show lx) => Bool -> IO (LogControls l lx, TQueue l)
testLogControls wantConsole = do
  chn <- newTChanIO
  logQ <- newTQueueIO

  -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln
  let logWorker :: IO ()
      logWorker =
        atomically (readTChan chn)
          >>= maybe
            (pure ())
            (\evt -> when wantConsole (pPrint evt) >> logWorker)

      stopWorker :: IO ()
      stopWorker = atomically $ writeTChan chn Nothing

      sink :: l -> IO ()
      sink eventLog =
        atomically $ do
          writeTChan chn $ Just eventLog
          writeTQueue logQ eventLog

  pure (LogControls sink logWorker stopWorker logQ, logQ)

data LoggerSource l = MkLoggerSource
  { rootLogger :: l -> IO (),
    newLogger :: IO (l -> IO ())
  }

runWithLogger :: forall l lx. LogControls l lx -> (LoggerSource l -> IO ()) -> IO ()
runWithLogger
  LogControls
    { sink,
      expander,
      logWorker,
      stopWorker
    }
  action =
    do
      rootLogger <- mkNewLogger
      let loggerSource = MkLoggerSource rootLogger mkNewLogger
      -- logWorker and execution run concurrently
      -- logworker serialises the log events emitted by the execution
      concurrently_
        logWorker
        ( finally
            (action loggerSource)
            stopWorker
        )
    where
      mkNewLogger :: IO (l -> IO ())
      mkNewLogger = mkLogger expander sink <$> UnliftIO.newIORef (-1) <*> P.myThreadId