module DSL.Logger where

import Common as C
import  DSL.LogProtocol as LP
import  DSL.CurrentTime as CT
import DSL.LogProtocol.PrettyPrint
import           Data.DList as D
import           Pyrelude as P
import           PyrethrumExtras.IO as PIO hiding (now)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import System.IO (stdout)
import Polysemy
import Polysemy.Output as O
import Polysemy.Reader
import Polysemy.State


data Logger e m a where
  LogItem :: (Show e, A.ToJSON e) => LogProtocolBase e -> Logger e m ()

  LogMessage :: Text -> Logger e m ()
  LogMessage' :: Text -> Text -> Logger e m ()

  LogWarning :: Text -> Logger e m ()
  LogWarning' :: Text -> Text -> Logger e m ()

  LogError :: Text -> Logger e m ()
  LogError' :: Text -> Text -> Logger e m ()

makeSem ''Logger

detailLog :: forall effs e. (Show e, A.ToJSON e, Member (Logger e) effs) => (DetailedInfo -> LogProtocolBase e) -> Text -> Text -> Sem effs ()
detailLog lpCons msg additionalInfo = logItem . lpCons $ DetailedInfo msg additionalInfo

log :: forall effs e. Member (Logger e) effs => Text -> Sem effs ()
log = logMessage

logAction ::(Show e, A.ToJSON e, Member (Logger e) effs) => Text -> Sem effs ()
logAction = logItem . IOAction

log' :: forall effs e. (Show e, A.ToJSON e, Member (Logger e) effs) => Text -> Text -> Sem effs ()
log' = detailLog Message'

logRunConsoleInterpreter :: forall effs a e. (Show e, Members '[CurrentTime, Embed IO] effs) => Sem (Logger e ': effs) a -> Sem effs a
logRunConsoleInterpreter = 
    interpret $ \lg -> embed $ case lg of
                          LogItem lp -> P.print lp
                          LogError msg -> P.print . LP.Error $ C.Error msg 
                          LogError' msg info -> P.print . LP.Error . Error' $ DetailedInfo msg info
                          
                          LogMessage s ->  P.print $ Message s 
                          LogMessage' msg info -> P.print . Message' $ DetailedInfo msg info
        
                          LogWarning s -> P.print $ Warning s 
                          LogWarning' msg info -> P.print . Warning' $ DetailedInfo msg info

-- ToDo move to lib
putLines :: Handle -> Text -> IO ()
putLines hOut tx = sequence_ $ PIO.hPutStrLn hOut <$> lines tx

utfEncode :: A.ToJSON a => a -> Text
utfEncode a = either 
                (\e -> "Encode error: " <> txt e)
                id
                (decodeUtf8' . B.toStrict $ A.encode a)

-- TODO - update to use info
logStrJSONWith :: A.ToJSON e => ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text
logStrJSONWith thrdInfo lgIdx time lp = 
  utfEncode LogProtocolOut {
                        logIndex = LogEventInfo {
                          rnId = thrdInfo.runId, 
                          threadIdx =  thrdInfo.threadIndex,
                          time = time,
                          idx = lgIdx
                        },
                        logInfo = utfEncode <$> lp
                      }
                
runThreadInfoReader :: Member CurrentTime r => Sem (Reader ThreadInfo ': r) a -> Sem r a 
runThreadInfoReader sem = do 
                            tz <- CT.getTimeZone
                            runReader (ThreadInfo "local" 1 tz) sem

logConsolePrettyInterpreter :: (Show e, Members '[Embed IO, Reader ThreadInfo, State LogIndex, CurrentTime] effs) => Sem (Logger e ': effs) a -> Sem effs a
logConsolePrettyInterpreter = logToHandles [(prettyPrintLogProtocolWith Run, stdout)]

incIdx :: LogIndex -> LogIndex
incIdx (LogIndex i) = LogIndex $ i + 1

-- TODO: looks like output
logWithSink :: forall effs a e. (Show e, A.ToJSON e) => ((Show e, A.ToJSON e) => LogProtocolBase e -> Sem effs ()) -> Sem (Logger e ': effs) a -> Sem effs a
logWithSink push = 
    interpret $ \case 
                  LogItem lp -> push lp

                  LogError msg -> push . LP.Error $ C.Error msg
                  LogError' msg inf -> push . LP.Error . C.Error' $ DetailedInfo msg inf

                  LogMessage s -> push $ Message s 
                  LogMessage' msg info -> push . Message' $ DetailedInfo msg info

                  LogWarning s -> push $ Warning s 
                  LogWarning' msg info ->  push . Warning' $ DetailedInfo msg info

-- can produce a list of LogProtocols - used for testing
logRunRawInterpreter :: forall effs a e. (Show e, A.ToJSON e, Member (Output (LogProtocolBase e)) effs) => Sem (Logger e ': effs) a -> Sem effs a
logRunRawInterpreter = logWithSink output

logToHandles :: forall effs e a. Members '[Embed IO, Reader ThreadInfo, State LogIndex, CurrentTime] effs => [(ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text, Handle)] -> Sem (Logger e ': effs) a -> Sem effs a
logToHandles convertersHandles = 
    interpret $ \lg -> 
                    do 
                      threadInfo :: ThreadInfo <- ask
                      modify incIdx
                      idx <- get
                      now' <- CT.now
                      let 
                        simpleConvertersHandles :: [(LogProtocolBase e -> Text, Handle)]
                        simpleConvertersHandles = (\(f , h) -> (f threadInfo idx now', h)) <$> convertersHandles

                        logToHandle :: (LogProtocolBase e -> Text) -> Handle -> LogProtocolBase e -> IO ()
                        logToHandle cvtr h = putLines h . cvtr
                    
                        logToh :: LogProtocolBase e -> (LogProtocolBase e -> Text, Handle) -> IO ()
                        logToh lp (f, h) = logToHandle f h lp
                    
                        logLogProtocol :: LogProtocolBase e -> IO ()
                        logLogProtocol lp =  P.sequence_ $ logToh lp <$> simpleConvertersHandles
                                         
                        logToIO :: Logger e m x -> IO x
                        logToIO = \case 
                                    LogItem lp -> logLogProtocol lp

                                    LogError msg -> logLogProtocol . LP.Error $ C.Error msg
                                    LogError' msg info -> logLogProtocol . LP.Error . C.Error' $ DetailedInfo msg info

                                    LogMessage s ->  logLogProtocol $ Message s 
                                    LogMessage' msg info -> logLogProtocol . Message' $ DetailedInfo msg info

                                    LogWarning s -> logLogProtocol $ Warning s 
                                    LogWarning' msg info ->  logLogProtocol . Warning' $ DetailedInfo msg info

                      embed $ logToIO lg


-- used for testing ~ cons to a list (so log is in reverse order)
consListLog :: forall effs a e. (Show e, Member (Output (LogProtocolBase e)) effs, A.ToJSON e) => Sem (Logger e ': effs) a -> Sem effs a
consListLog = logWithSink output

-- used for testing ~ cons to a list (so log is in reverse order)
consListLogPretty :: forall effs a e. (Show e, Member (Output Text) effs, A.ToJSON e) => Sem (Logger e ': effs) a -> Sem effs a
consListLogPretty = logWithSink (output . prettyPrintLogProtocol Run)

logDocInterpreter :: forall effs a e. (Show e, Member OutputDListText effs, A.ToJSON e) => Sem (Logger e ': effs) a -> Sem effs a
logDocInterpreter = logWithSink (output . dList)
                                                     
logDocPrettyInterpreter :: forall effs a e. (Show e, Member OutputDListText effs, A.ToJSON e) => Sem (Logger e ': effs) a -> Sem effs a
logDocPrettyInterpreter = logWithSink (output . D.fromList . lines . prettyPrintLogProtocol Doc)
