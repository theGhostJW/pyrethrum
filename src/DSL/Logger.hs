
module DSL.Logger where

import Common as C
import  DSL.LogProtocol as LP
import  DSL.CurrentTime as CT
import DSL.LogProtocol.PrettyPrint
import           Data.DList
import           Pyrelude as P
import           Pyrelude.IO
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

logDocAction :: (Show e, A.ToJSON e) => Member (Logger e) effs => Text -> Sem effs ()
logDocAction = logItem . IterationLog . Doc . DocAction . ActionInfo

data LogAuxInfo = LogAuxInfo {
  runId :: Text,
  threadID :: Int,
  logTime :: UTCTime
}

detailLog :: forall effs e. (Show e, A.ToJSON e, Member (Logger e) effs) => (DetailedInfo -> LogProtocolBase e) -> Text -> Text -> Sem effs ()
detailLog lpCons msg additionalInfo = logItem . lpCons $ DetailedInfo msg additionalInfo

log :: forall effs e. Member (Logger e) effs => Text -> Sem effs ()
log = logMessage

log' :: forall effs e. (Show e, A.ToJSON e, Member (Logger e) effs) => Text -> Text -> Sem effs ()
log' = detailLog (logRun . Message')

-- TODO - phantom types ? 
logRunConsoleInterpreter :: forall effs a e. (Show e, Members '[CurrentTime, Embed IO] effs) => Sem (Logger e ': effs) a -> Sem effs a
logRunConsoleInterpreter = 
    interpret $ \lg -> do 
                        now <- CT.getCurrentTime
                        embed $ case lg of
                            LogItem lp -> P.print lp
                            LogError msg -> P.print . logRun . LP.Error $ C.Error msg 
                            LogError' msg info -> P.print . logRun . LP.Error . Error' $ DetailedInfo msg info
                            
                            LogMessage s ->  P.print . logRun $ Message s 
                            LogMessage' msg info -> P.print . logRun . Message' $ DetailedInfo msg info
          
                            LogWarning s -> P.print. logRun $ Warning s 
                            LogWarning' msg info -> P.print . logRun . Warning' $ DetailedInfo msg info

-- ToDo move to lib
putLines :: Handle -> Text -> IO ()
putLines hOut tx = sequence_ $ hPutStrLn hOut <$> lines tx

-- TODO - update to use info
logStrJSONWith :: A.ToJSON e => ThreadInfo -> LogIdxTime -> LogProtocolBase e -> Text
logStrJSONWith _ _ lp = eitherf (decodeUtf8' . B.toStrict . A.encode $ lp)
                          (\e -> "Encode error: " <> txt e)
                          id

runThreadInfoReader :: Member CurrentTime r => Sem (Reader ThreadInfo ': r) a -> Sem r a 
runThreadInfoReader sem = 
  do 
    zone <- CT.getCurrentTimeZone
    runReader (ThreadInfo "local" 1 zone) sem

logConsolePrettyInterpreter :: (Show e, Members '[Embed IO, Reader ThreadInfo, State LogIndex, CurrentTime] effs) => Sem (Logger e ': effs) a -> Sem effs a
logConsolePrettyInterpreter = logToHandles [(prettyPrintLogProtocolWith False, stdout)]

incIdx :: LogIndex -> LogIndex
incIdx (LogIndex i) = LogIndex $ i + 1

logRunWithSink :: forall effs a e. (Show e, A.ToJSON e) => ((Show e, A.ToJSON e) => LogProtocolBase e -> Sem effs ()) -> Sem (Logger e ': effs) a -> Sem effs a
logRunWithSink pushItem = 
  let
    pushRun :: RunProtocol e -> Sem effs () 
    pushRun = pushItem . logRun
  in
    interpret $ \case 
                  LogItem lp -> pushItem lp

                  LogError msg -> pushRun . LP.Error $ C.Error msg
                  LogError' msg inf -> pushRun . LP.Error . C.Error' $ DetailedInfo msg inf

                  LogMessage s -> pushRun $ Message s 
                  LogMessage' msg info -> pushRun . Message' $ DetailedInfo msg info

                  LogWarning s -> pushRun $ Warning s 
                  LogWarning' msg info ->  pushRun . Warning' $ DetailedInfo msg info

-- can produce a list of LogProtocols - used for testing
logRunRawInterpreter :: forall effs a e. (Show e, A.ToJSON e, Member (Output (LogProtocolBase e)) effs) => Sem (Logger e ': effs) a -> Sem effs a
logRunRawInterpreter = logRunWithSink output
logToHandles :: forall effs e a. Members '[Embed IO, Reader ThreadInfo, State LogIndex, CurrentTime] effs => [(ThreadInfo -> LogIdxTime -> LogProtocolBase e -> Text, Handle)] -> Sem (Logger e ': effs) a -> Sem effs a

logToHandles convertersHandles = 
    interpret $ \lg -> 
                    do 
                      threadInfo :: ThreadInfo <- ask
                      modify incIdx
                      idx :: LogIndex <- get
                      now <- CT.getCurrentTime
                      let 
                        lgInfo :: LogIdxTime
                        lgInfo = LogIdxTime (unLogIndex idx) now

                        simpleConvertersHandles :: [(LogProtocolBase e -> Text, Handle)]
                        simpleConvertersHandles = (\(f , h) -> (f threadInfo lgInfo, h)) <$> convertersHandles

                        logToHandle :: (LogProtocolBase e -> Text) -> Handle -> LogProtocolBase e -> IO ()
                        logToHandle cvtr h = putLines h . cvtr
                    
                        logToh :: LogProtocolBase e -> (LogProtocolBase e -> Text, Handle) -> IO ()
                        logToh lp (f, h) = logToHandle f h lp
                    
                        logLogProtocol :: LogProtocolBase e -> IO ()
                        logLogProtocol lp =  P.sequence_ $ logToh lp <$> simpleConvertersHandles
                    
                        logRunTohandles :: RunProtocol e -> IO ()
                        logRunTohandles = logLogProtocol . logRun 
                      
                        logToIO :: Logger e m x -> IO x
                        logToIO = \case 
                                    LogItem lp -> logLogProtocol lp

                                    LogError msg -> logRunTohandles . LP.Error $ C.Error msg
                                    LogError' msg info -> logRunTohandles . LP.Error . C.Error' $ DetailedInfo msg info

                                    LogMessage s ->  logRunTohandles $ Message s 
                                    LogMessage' msg info -> logRunTohandles . Message' $ DetailedInfo msg info

                                    LogWarning s -> logRunTohandles $ Warning s 
                                    LogWarning' msg info ->  logRunTohandles . Warning' $ DetailedInfo msg info

                      embed $ logToIO lg


logDocWithSink :: forall effs a e. (LogProtocolBase e -> Sem effs ()) -> Sem (Logger e ': effs) a -> Sem effs a
logDocWithSink pushItem = 
  let
    pushDoc :: DocProtocol e -> Sem effs () 
    pushDoc = pushItem . logDoc
  in
    interpret $ \case 
                  LogItem lp -> pushItem lp

                  LogError msg -> pushDoc. DocError $ C.Error msg
                  LogError' msg inf -> pushDoc . DocError . C.Error' $ DetailedInfo msg inf

                  LogMessage s ->  pushDoc $ DocMessage s 
                  LogMessage' msg info -> pushDoc . DocMessage' $ DetailedInfo msg info

                  LogWarning s -> pushDoc $ DocWarning s 
                  LogWarning' msg info ->  pushDoc . DocWarning' $ DetailedInfo msg info


logDocInterpreter :: forall effs a e. (Show e, Member OutputDListText effs) => Sem (Logger e ': effs) a -> Sem effs a
logDocInterpreter = logDocWithSink (output . dList)
                                                     
logDocPrettyInterpreter :: forall effs a e. (Show e, Member OutputDListText effs) => Sem (Logger e ': effs) a -> Sem effs a
logDocPrettyInterpreter = logDocWithSink (output . fromList . lines . prettyPrintLogProtocol True)