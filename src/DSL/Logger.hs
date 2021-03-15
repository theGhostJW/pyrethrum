
module DSL.Logger where

import Common as C
import  DSL.LogProtocol as LP
import  DSL.CurrentTime as CT
import DSL.LogProtocol.PrettyPrint
import           Data.DList as D
import           Pyrelude as P
import           Pyrelude.IO as PIO hiding (now)
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

logRP :: forall e effs. (Show e, A.ToJSON e, Member (Logger e) effs) => RunProtocol e -> Sem effs ()
logRP = logItem . logRun 

logDocAction :: (Show e, A.ToJSON e) => Member (Logger e) effs => Text -> Sem effs ()
logDocAction = logItem . IterationLog . Doc . DocAction . ActionInfo

detailLog :: forall effs e. (Show e, A.ToJSON e, Member (Logger e) effs) => (DetailedInfo -> LogProtocolBase e) -> Text -> Text -> Sem effs ()
detailLog lpCons msg additionalInfo = logItem . lpCons $ DetailedInfo msg additionalInfo

log :: forall effs e. Member (Logger e) effs => Text -> Sem effs ()
log = logMessage

log' :: forall effs e. (Show e, A.ToJSON e, Member (Logger e) effs) => Text -> Text -> Sem effs ()
log' = detailLog (logRun . Message')

logRunConsoleInterpreter :: forall effs a e. (Show e, Members '[CurrentTime, Embed IO] effs) => Sem (Logger e ': effs) a -> Sem effs a
logRunConsoleInterpreter = 
    interpret $ \lg -> embed $ case lg of
                          LogItem lp -> P.print lp
                          LogError msg -> P.print . logRun . LP.Error $ C.Error msg 
                          LogError' msg info -> P.print . logRun . LP.Error . Error' $ DetailedInfo msg info
                          
                          LogMessage s ->  P.print . logRun $ Message s 
                          LogMessage' msg info -> P.print . logRun . Message' $ DetailedInfo msg info
        
                          LogWarning s -> P.print. logRun $ Warning s 
                          LogWarning' msg info -> P.print . logRun . Warning' $ DetailedInfo msg info

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
                          rnId = runId thrdInfo, 
                          threadIdx = threadIndex thrdInfo,
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
logConsolePrettyInterpreter = logToHandles [(prettyPrintLogProtocolWith False, stdout)]

incIdx :: LogIndex -> LogIndex
incIdx (LogIndex i) = LogIndex $ i + 1

logRunWithSink :: forall effs a e. (Show e, A.ToJSON e) => ((Show e, A.ToJSON e) => LogProtocolBase e -> Sem effs ()) -> Sem (Logger e ': effs) a -> Sem effs a
logRunWithSink push = 
  let
    pushRun :: RunProtocol e -> Sem effs () 
    pushRun = push . logRun
  in
    interpret $ \case 
                  LogItem lp -> push lp

                  LogError msg -> pushRun . LP.Error $ C.Error msg
                  LogError' msg inf -> pushRun . LP.Error . C.Error' $ DetailedInfo msg inf

                  LogMessage s -> pushRun $ Message s 
                  LogMessage' msg info -> pushRun . Message' $ DetailedInfo msg info

                  LogWarning s -> pushRun $ Warning s 
                  LogWarning' msg info ->  pushRun . Warning' $ DetailedInfo msg info

-- can produce a list of LogProtocols - used for testing
logRunRawInterpreter :: forall effs a e. (Show e, A.ToJSON e, Member (Output (LogProtocolBase e)) effs) => Sem (Logger e ': effs) a -> Sem effs a
logRunRawInterpreter = logRunWithSink output

logToHandles :: forall effs e a. Members '[Embed IO, Reader ThreadInfo, State LogIndex, CurrentTime] effs => [(ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text, Handle)] -> Sem (Logger e ': effs) a -> Sem effs a
logToHandles convertersHandles = 
    interpret $ \lg -> 
                    do 
                      threadInfo :: ThreadInfo <- ask
                      modify incIdx
                      idx <- get
                      now' <- now
                      let 
                        simpleConvertersHandles :: [(LogProtocolBase e -> Text, Handle)]
                        simpleConvertersHandles = (\(f , h) -> (f threadInfo idx now', h)) <$> convertersHandles

                        logToHandle :: (LogProtocolBase e -> Text) -> Handle -> LogProtocolBase e -> IO ()
                        logToHandle cvtr h = putLines h . cvtr
                    
                        logToh :: LogProtocolBase e -> (LogProtocolBase e -> Text, Handle) -> IO ()
                        logToh lp (f, h) = logToHandle f h lp
                    
                        logLogProtocol :: LogProtocolBase e -> IO ()
                        logLogProtocol lp =  P.sequence_ $ logToh lp <$> simpleConvertersHandles
                    
                        logRunToHandles :: RunProtocol e -> IO ()
                        logRunToHandles = logLogProtocol . logRun 
                      
                        logToIO :: Logger e m x -> IO x
                        logToIO = \case 
                                    LogItem lp -> logLogProtocol lp

                                    LogError msg -> logRunToHandles . LP.Error $ C.Error msg
                                    LogError' msg info -> logRunToHandles . LP.Error . C.Error' $ DetailedInfo msg info

                                    LogMessage s ->  logRunToHandles $ Message s 
                                    LogMessage' msg info -> logRunToHandles . Message' $ DetailedInfo msg info

                                    LogWarning s -> logRunToHandles $ Warning s 
                                    LogWarning' msg info ->  logRunToHandles . Warning' $ DetailedInfo msg info

                      embed $ logToIO lg


logDocWithSink :: forall effs a e. (LogProtocolBase e -> Sem effs ()) -> Sem (Logger e ': effs) a -> Sem effs a
logDocWithSink push = 
  let
    pushDoc :: DocProtocol e -> Sem effs () 
    pushDoc = push . logDoc
  in
    interpret $ \case 
                  LogItem lp -> push lp

                  LogError msg -> pushDoc. DocError $ C.Error msg
                  LogError' msg inf -> pushDoc . DocError . C.Error' $ DetailedInfo msg inf

                  LogMessage s ->  pushDoc $ DocMessage s 
                  LogMessage' msg info -> pushDoc . DocMessage' $ DetailedInfo msg info

                  LogWarning s -> pushDoc $ DocWarning s 
                  LogWarning' msg info ->  pushDoc . DocWarning' $ DetailedInfo msg info


logDocInterpreter :: forall effs a e. (Show e, Member OutputDListText effs) => Sem (Logger e ': effs) a -> Sem effs a
logDocInterpreter = logDocWithSink (output . dList)
                                                     
logDocPrettyInterpreter :: forall effs a e. (Show e, Member OutputDListText effs) => Sem (Logger e ': effs) a -> Sem effs a
logDocPrettyInterpreter = logDocWithSink (output . D.fromList . lines . prettyPrintLogProtocol True)