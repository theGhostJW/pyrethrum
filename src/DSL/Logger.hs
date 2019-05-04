
module DSL.Logger where

import Common
import  DSL.LogProtocol
import DSL.LogProtocol.PrettyPrint
import           Data.DList
import           Pyrelude as P
import           Pyrelude.IO
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import Text.Show.Pretty as PP
import RunElementClasses as C
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Check (ResultExpectation(..) , ExpectationActive(..), CheckReport(..), CheckInfo(..), GateStatus(..), classifyResult)
import Data.Yaml as Y
import System.IO (stdout)

data Logger r where
 LogItem :: LogProtocol -> Logger ()

 LogMessage :: Text -> Logger ()
 LogMessage' :: Text -> Text -> Logger ()

 LogWarning :: Text -> Logger ()
 LogWarning' :: Text -> Text -> Logger ()

 LogError :: Text -> Logger ()
 LogError' :: Text -> Text -> Logger ()

logItem :: Member Logger effs => LogProtocol -> Eff effs ()
logItem = send . LogItem

detailLog :: forall effs. Member Logger effs => (DetailedInfo -> LogProtocol) -> Text -> Text -> Eff effs ()
detailLog lpCons msg additionalInfo = logItem . lpCons $ DetailedInfo msg additionalInfo

log :: forall effs. Member Logger effs => Text -> Eff effs ()
log = send . LogMessage

log' :: forall effs. Member Logger effs => Text -> Text -> Eff effs ()
log' = detailLog (logRun . Message')

logWarning :: forall effs. Member Logger effs => Text -> Eff effs ()
logWarning = send . LogWarning

logWarning' :: forall effs. (Member Logger effs) => Text -> Text -> Eff effs ()
logWarning' = detailLog (logRun .  Warning')

logError :: forall effs. Member Logger effs => Text -> Eff effs ()
logError = send . LogError

logError' :: forall effs. Member Logger effs => Text -> Text -> Eff effs ()
logError' msg = send . LogError' msg --- detailLog  (Error . AppUserError')

logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsoleInterpreter = interpretM $ \case 
                                        LogItem lp -> P.print lp
                                        LogError msg -> P.print . logRun . Error $ AppUserError msg 
                                        LogError' msg info -> P.print . logRun . Error . AppUserError' $ DetailedInfo msg info
                                        
                                        LogMessage s ->  P.print . logRun $ Message s 
                                        LogMessage' msg info -> P.print . logRun . Message' $ DetailedInfo msg info
                      
                                        LogWarning s -> P.print. logRun $ Warning s 
                                        LogWarning' msg info ->  P.print . logRun . Warning' $ DetailedInfo msg info

logDocInterpreter :: forall effs. Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocInterpreter = 
                    let 
                      tellDoc :: DocProtocol -> Eff effs ()
                      tellDoc = tell . dList . logDoc
                    in
                      interpret $ \case 
                                      LogItem lp -> tell $ dList lp
                                      LogError msg -> tellDoc . DocError $ AppUserError msg 
                                      LogError' msg info -> tellDoc . DocError . AppUserError' $ DetailedInfo msg info

                                      LogMessage s ->  tellDoc $ DocMessage s 
                                      LogMessage' msg info -> tellDoc . DocMessage' $ DetailedInfo msg info

                                      LogWarning s -> tellDoc $ DocWarning s 
                                      LogWarning' msg info ->  tellDoc . DocWarning' $ DetailedInfo msg info


-- ToDo move to lib
putLines :: Handle -> Text -> IO ()
putLines hOut tx = sequence_ $ hPutStrLn hOut <$> lines tx

logStrJSON :: LogProtocol -> Text
logStrJSON lp = eitherf (decodeUtf8' . B.toStrict . A.encode $ lp)
                  (\e -> "Encode error: " <> txt e)
                  id

logConsolePrettyInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsolePrettyInterpreter = logToHandles [(prettyPrintLogProtocol False, stdout)]

logToHandles :: LastMember IO effs => [(LogProtocol -> Text, Handle)] -> Eff (Logger ': effs) ~> Eff effs
logToHandles convertersHandlers = 
                        let 
                          logToHandle :: (LogProtocol -> Text) -> Handle -> LogProtocol -> IO ()
                          logToHandle lp2Str h = putLines h . lp2Str 

                          logToh :: LogProtocol -> (LogProtocol -> Text, Handle) -> IO ()
                          logToh lp (f, h) = logToHandle f h lp

                          logTohandles :: LogProtocol -> IO ()
                          logTohandles lp =  P.sequence_ $ logToh lp <$> convertersHandlers

                          logRunTohandles :: RunProtocol -> IO ()
                          logRunTohandles = logTohandles . IterationLog . Run 
                        in
                          interpretM $ \case 
                                          LogItem lp -> logTohandles lp

                                          LogError msg -> logRunTohandles . Error $ AppUserError msg
                                          LogError' msg info -> logRunTohandles . Error . AppUserError' $ DetailedInfo msg info

                                          LogMessage s ->  logRunTohandles $ Message s 
                                          LogMessage' msg info -> logRunTohandles . Message' $ DetailedInfo msg info

                                          LogWarning s -> logRunTohandles $ Warning s 
                                          LogWarning' msg info ->  logRunTohandles . Warning' $ DetailedInfo msg info
                                          
                             
logDocPrettyInterpreter :: forall effs. Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocPrettyInterpreter = let
                            toDList :: [Text] -> DList Text
                            toDList = fromList

                            pushItem :: LogProtocol -> Eff effs () 
                            pushItem = tell . toDList . lines . prettyPrintLogProtocol True

                            pushDoc :: DocProtocol -> Eff effs () 
                            pushDoc = pushItem . IterationLog . Doc
                          in
                            interpret $  \case 
                                            LogItem lp -> pushItem lp

                                            LogError msg -> pushDoc. DocError $ AppUserError msg
                                            LogError' msg inf -> pushDoc . DocError . AppUserError' $ DetailedInfo msg inf

                                            LogMessage s ->  pushDoc $ DocMessage s 
                                            LogMessage' msg info -> pushDoc . DocMessage' $ DetailedInfo msg info

                                            LogWarning s -> pushDoc $ DocWarning s 
                                            LogWarning' msg info ->  pushDoc . DocWarning' $ DetailedInfo msg info

                               
