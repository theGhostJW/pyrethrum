
module DSL.Logger where

import Common
import  DSL.LogProtocol
import  DSL.CurrentTime as CT
import DSL.LogProtocol.PrettyPrint
import           Data.DList
import           Pyrelude as P
import           Pyrelude.IO
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import System.IO (stdout)
import Polysemy
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State


data Logger m a where
  LogItem :: LogProtocol -> Logger m ()

  LogMessage :: Text -> Logger m ()
  LogMessage' :: Text -> Text -> Logger m ()

  LogWarning :: Text -> Logger m ()
  LogWarning' :: Text -> Text -> Logger m ()

  LogError :: Text -> Logger m ()
  LogError' :: Text -> Text -> Logger m ()

makeSem ''Logger

logDocAction :: Member Logger effs => Text -> Sem effs ()
logDocAction = logItem . IterationLog . Doc . DocAction . ActionInfo

data LogAuxInfo = LogAuxInfo {
  runId :: Text,
  threadID :: Int,
  logTime :: UTCTime
}

detailLog :: forall effs. Member Logger effs => (DetailedInfo -> LogProtocol) -> Text -> Text -> Sem effs ()
detailLog lpCons msg additionalInfo = logItem . lpCons $ DetailedInfo msg additionalInfo

log :: forall effs. Member Logger effs => Text -> Sem effs ()
log = logMessage

log' :: forall effs. Member Logger effs => Text -> Text -> Sem effs ()
log' = detailLog (logRun . Message')

logConsoleInterpreter :: forall effs a. Members '[CurrentTime, Embed IO] effs => Sem (Logger ': effs) a -> Sem effs a
logConsoleInterpreter = 
    interpret $ \lg -> do 
                        now <- CT.getCurrentTime
                        embed $ case lg of
                            LogItem lp -> P.print lp
                            LogError msg -> P.print . logRun . Error $ AppUserError msg 
                            LogError' msg info -> P.print . logRun . Error . AppUserError' $ DetailedInfo msg info
                            
                            LogMessage s ->  P.print . logRun $ Message s 
                            LogMessage' msg info -> P.print . logRun . Message' $ DetailedInfo msg info
          
                            LogWarning s -> P.print. logRun $ Warning s 
                            LogWarning' msg info -> P.print . logRun . Warning' $ DetailedInfo msg info

logDocInterpreter :: forall effs a. Member WriterDList effs => Sem (Logger ': effs) a -> Sem effs a
logDocInterpreter = 
  let 
    tellDoc :: DocProtocol -> Sem effs ()
    tellDoc = output . dList . logDoc
  in
    interpret $ \case 
                    LogItem lp -> output $ dList lp

                    LogError msg -> tellDoc . DocError $ AppUserError msg 
                    LogError' msg info -> tellDoc . DocError . AppUserError' $ DetailedInfo msg info

                    LogMessage s ->  tellDoc $ DocMessage s 
                    LogMessage' msg info -> tellDoc . DocMessage' $ DetailedInfo msg info

                    LogWarning s -> tellDoc $ DocWarning s 
                    LogWarning' msg info ->  tellDoc . DocWarning' $ DetailedInfo msg info


-- ToDo move to lib
putLines :: Handle -> Text -> IO ()
putLines hOut tx = sequence_ $ hPutStrLn hOut <$> lines tx

-- TODO - update to use info
logStrJSONWith :: ThreadInfo -> LogInfo -> LogProtocol -> Text
logStrJSONWith _ _ lp = eitherf (decodeUtf8' . B.toStrict . A.encode $ lp)
                  (\e -> "Encode error: " <> txt e)
                  id

logStrJSON :: LogProtocol -> Text
logStrJSON lp = eitherf (decodeUtf8' . B.toStrict . A.encode $ lp)
                  (\e -> "Encode error: " <> txt e)
                  id

runIOThreadInfoReader :: Member CurrentTime r => Sem (Reader ThreadInfo ': r) a -> Sem r a 
runIOThreadInfoReader sem = 
  do 
    zone <- CT.getCurrentTimeZone
    runReader (ThreadInfo "local" 1 zone) sem

logConsolePrettyInterpreter :: Members '[Embed IO, Reader ThreadInfo, State LogIndex, CurrentTime] effs => Sem (Logger ': effs) a -> Sem effs a
logConsolePrettyInterpreter = logToHandles [(prettyPrintLogProtocolWith False, stdout)]

incIdx :: LogIndex -> LogIndex
incIdx (LogIndex i) = LogIndex $ i + 1

logToHandles :: Members '[Embed IO, Reader ThreadInfo, State LogIndex, CurrentTime] effs => [(ThreadInfo -> LogInfo -> LogProtocol -> Text, Handle)] -> Sem (Logger ': effs) a -> Sem effs a
logToHandles convertersHandles = 
    interpret $ \lg -> 
                    do 
                      threadInfo :: ThreadInfo <- ask
                      modify incIdx
                      idx :: LogIndex <- get
                      now <- CT.getCurrentTime
                      let 
                        lgInfo :: LogInfo
                        lgInfo = LogInfo (unLogIndex idx) now

                        simpleConvertersHandles :: [(LogProtocol -> Text, Handle)]
                        simpleConvertersHandles = (\(f , h) -> (f threadInfo lgInfo, h)) <$> convertersHandles

                        logToHandle :: (LogProtocol -> Text) -> Handle -> LogProtocol -> IO ()
                        logToHandle cvtr h = putLines h . cvtr
                    
                        logToh :: LogProtocol -> (LogProtocol -> Text, Handle) -> IO ()
                        logToh lp (f, h) = logToHandle f h lp
                    
                        logTohandles :: LogProtocol -> IO ()
                        logTohandles lp =  P.sequence_ $ logToh lp <$> simpleConvertersHandles
                    
                        logRunTohandles :: RunProtocol -> IO ()
                        logRunTohandles = logTohandles . IterationLog . Run 
                      
                        logToIO :: Logger m a -> IO a
                        logToIO = \case 
                                    LogItem lp -> logTohandles lp

                                    LogError msg -> logRunTohandles . Error $ AppUserError msg
                                    LogError' msg info -> logRunTohandles . Error . AppUserError' $ DetailedInfo msg info

                                    LogMessage s ->  logRunTohandles $ Message s 
                                    LogMessage' msg info -> logRunTohandles . Message' $ DetailedInfo msg info

                                    LogWarning s -> logRunTohandles $ Warning s 
                                    LogWarning' msg info ->  logRunTohandles . Warning' $ DetailedInfo msg info

                      embed $ logToIO lg
                         
                                          
                              
logDocPrettyInterpreter :: forall effs a. Member WriterDList effs => Sem (Logger ': effs) a -> Sem effs a
logDocPrettyInterpreter = 
  let
    toDList :: [Text] -> DList Text
    toDList = fromList

    pushItem :: LogProtocol -> Sem effs () 
    pushItem = output . toDList . lines . prettyPrintLogProtocol True

    pushDoc :: DocProtocol -> Sem effs () 
    pushDoc = pushItem . IterationLog . Doc
  in
    interpret $ \case 
                  LogItem lp -> pushItem lp

                  LogError msg -> pushDoc. DocError $ AppUserError msg
                  LogError' msg inf -> pushDoc . DocError . AppUserError' $ DetailedInfo msg inf

                  LogMessage s ->  pushDoc $ DocMessage s 
                  LogMessage' msg info -> pushDoc . DocMessage' $ DetailedInfo msg info

                  LogWarning s -> pushDoc $ DocWarning s 
                  LogWarning' msg info ->  pushDoc . DocWarning' $ DetailedInfo msg info


itrFuncDeleteMe :: Logger m x -> Sem effs x
itrFuncDeleteMe = uu

                                 
  