module Internal.LoggingCore where

-- TODO: Explicit exports remove old code
import BasePrelude qualified as P
import CoreUtils qualified as C
import Effectful.Concurrent.STM (TQueue)
import Text.Show.Pretty (pPrint)
import UnliftIO ( concurrently_, finally, newIORef, tryReadTQueue )
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (atomically, newTChanIO, newTQueueIO, readTChan, writeTChan, writeTQueue)
import Prelude hiding (atomically, lines)


data BaseLog lc evt = MkLog
  { logContext :: lc,
    event :: evt
  }
  deriving (Show)

data LoggerSource l = MkLoggerSource
  { rootLogger :: l -> IO (),
    newLogger :: IO (l -> IO ())
  }

runWithLogger :: forall l lx. LogControls l lx -> (LoggerSource l -> IO ()) -> IO ()
runWithLogger
  LogControls
    { sink,
      aggregator,
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
      mkNewLogger = mkLogger aggregator sink <$> UnliftIO.newIORef (-1) <*> P.myThreadId

-- adds log index and thread id to loggable event and sends it to the sink
mkLogger :: forall l lxp. (C.ThreadId -> Int -> l -> lxp) -> (lxp -> IO ()) -> IORef Int -> ThreadId -> l -> IO ()
mkLogger aggregator sink idxRef thrdId logEvnt = do
  tc <- readIORef idxRef
  let nxt = succ tc
  finally (sink $ aggregator (C.mkThreadId thrdId) nxt logEvnt) $ writeIORef idxRef nxt

-- TODO:: Logger should be wrapped in an except that sets non-zero exit code on failure

data LogControls l lx = LogControls
  { aggregator :: C.ThreadId -> Int -> l -> lx,
    sink :: lx -> IO (),
    logWorker :: IO (),
    stopWorker :: IO ()
  }


q2List :: TQueue a -> STM [a]
q2List qu = reverse <$> recurse [] qu
  where
    recurse :: [a] -> TQueue a -> STM [a]
    recurse l q =
      tryReadTQueue q
        >>= maybe (pure l) (\e -> recurse (e : l) q)

testLogControls' :: forall l lx. (Show lx) => (C.ThreadId -> Int -> l -> lx) -> Bool -> IO (LogControls l lx, STM [lx])
testLogControls' aggregator wantConsole = do
  chn <- newTChanIO
  log <- newTQueueIO

  -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln
  let logWorker :: IO ()
      logWorker =
        atomically (readTChan chn)
          >>= maybe
            (pure ())
            (\evt -> when wantConsole (pPrint evt) >> logWorker)

      stopWorker :: IO ()
      stopWorker = atomically $ writeTChan chn Nothing

      sink :: lx -> IO ()
      sink eventLog =
        atomically $ do
          writeTChan chn $ Just eventLog
          writeTQueue log eventLog

  pure (LogControls {..}, q2List log)
