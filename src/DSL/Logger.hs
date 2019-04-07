
module DSL.Logger where

import Common
import  DSL.LogProtocol
import           Data.DList hiding (replicate)
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

-- simpleLog :: forall effs. Member Logger effs => (Text -> LogProtocol) -> Text -> Eff effs ()
-- simpleLog lpCons = logItem . lpCons

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


putLines :: Handle -> Text -> IO ()
putLines hOut tx = sequence_ $ hPutStrLn hOut <$> lines tx

prettyPrintFilterItem :: FilterResult -> Text
prettyPrintFilterItem FilterResult{..} =
    let
      description :: Text
      description = toString (testModAddress testInfo) <> " - " <> testTitle testInfo
    in
      maybef reasonForRejection
        ("accepted: " <> description)
        (\reason -> "rejected: " <> description <> " - Reason: " <> reason)

logStrJSON :: LogProtocol -> Text
logStrJSON lp = eitherf (decodeUtf8' . B.toStrict . A.encode $ lp)
                  (\e -> "Encode error: " <> txt e)
                  id

logStrPP :: Bool -> LogProtocol -> Text
logStrPP docMode =
            let
              hdr l h = l <> " " <> h <> " " <> l
              subHeader = hdr "----"
              header = hdr "===="
              tstHeader = hdr "==="
              itrHeader = hdr "=="

              iterId :: ItemId -> Text
              iterId (ItemId tst iid) = toString tst <> " / item " <> txt iid

              newLn :: Text
              newLn = "\n" :: Text

              indent2 :: Text -> Text
              indent2 = indentText 2 

              prettyBlock :: Char -> Text -> ItemId -> Text -> Text
              prettyBlock pfxChr headr iid body = indent2 $ toS (replicate 3 ' ') <> " " <> headr <> " - " <> iterId iid <> newLn <> indent2 body

              ppAeson:: Y.Value -> Text
              ppAeson val = toS ((getLenient . toS . Y.encode $ val) :: Text)

              ppAesonBlock:: Y.Value -> Text
              ppAesonBlock = indent2 . ppAeson

              ppMsgInfo :: Show a => Maybe a -> Text
              ppMsgInfo mbInfo = maybef mbInfo
                                        ""
                                        (\m -> newLn <> indent2 (showPretty m))

              docMarkUp :: Text -> Text
              docMarkUp s = (docMode ? id $ indent2) $ (docMode ? "  >> " $ "") <> s

              logIO :: Show a => a -> Text
              logIO m =  docMarkUp $ "IO Action: " <> showPretty m

              detailDoc :: Text -> DetailedInfo -> Text
              detailDoc hedr (DetailedInfo msg det) = newLn <> (docMode ? id $ indent2) (subHeader hedr <> newLn <> msg <> newLn <> det)
                                                                                                               
            in
              \case
                  BoundaryLog bl -> case bl of 
                                      FilterLog fltrInfos -> newLn <> header "Filter Log" <> newLn <>
                                                                    foldl (\acc fi -> acc <> fi <> newLn) "" (prettyPrintFilterItem <$> fltrInfos)

                                      StartRun ttle rc -> header ("Test Run: " <> unRunTitle ttle) <> 
                                                          newLn <> "Run Config:" <>
                                                          newLn <> ppAesonBlock rc

                                      StartGroup gt -> header $ "Group: " <> unGroupTitle gt
                                      EndGroup gt -> header $ "End Group: " <> unGroupTitle gt

                                      StartTest TestDisplayInfo{..} -> newLn <> tstHeader ("Start Test: " <> toString testModAddress <> " - " <> testTitle) <> 
                                                                        newLn <> "Test Config:" <>
                                                                        newLn <> ppAesonBlock testConfig

                                      EndTest (TestModule address) -> newLn <> tstHeader ("End Test: " <> address)
                                      StartIteration iid  _ _ val -> newLn <> subHeader ("Start Iteration: " <> iterId iid) <> 
                                                                      newLn <> "Item:" <> 
                                                                      newLn <> ppAesonBlock val <>
                                                                      (docMode ? "" $ newLn)

                                      EndIteration iid -> newLn <> subHeader ("End Iteration: " <> iterId iid)
                                      EndRun -> newLn <> header "End Run"

                  IterationLog (Doc dp) -> case dp of 
                                        DocAction ai -> case ai of
                                          ActionInfo msg -> "  >> " <> msg
                                          ActionInfoM msg extended -> "  >> " <> 
                                                                          msg <> 
                                                                          newLn <> 
                                                                          indent2 extended
                                        DocInteraction -> newLn <> "Interaction:"
                                        DocChecks -> newLn <> "Checks:"
                                        DocCheck iid chkhdr resultExpectation gateStatus -> 
                                                    indent2 $ "% " <> chkhdr  <> 
                                                        (
                                                          gateStatus == GateCheck 
                                                            ? "(Gate: subsequent checks will not be executed if this check fails)" 
                                                            $ ""
                                                        ) <> 
                                                        (
                                                        case resultExpectation of 
                                                          ExpectPass -> ""
                                                          ExpectFailure Inactive  _  -> ""
                                                          ExpectFailure Active message -> newLn <> indent2 ("!! This check is expected to fail: " <> message)
                                                        )
  
                                        DocIOAction m -> logIO m

                                        DocMessage s -> docMarkUp $ "message: " <> s
                                        DocMessage' detailedInfo -> detailDoc "Message" detailedInfo
                      
                                        DocWarning s -> docMarkUp $ "warning: " <> s
                                        DocWarning' detailedInfo -> detailDoc "Warning" detailedInfo

                                        e@(DocError _) -> showPretty e

                  IterationLog (Run rp) -> case rp of
                                        StartInteraction -> newLn <> "Interaction:"
                                        StartChecks -> newLn <> "Checks:"
                                        StartPrepState -> newLn <> "PrepState:"
                      
                                        IOAction m -> indent2 $ logIO m 
                                        
                                        InteractorSuccess iid (ApStateDisplay as) -> newLn <> prettyBlock '>' "Interactor Complete"  iid as
                                          
                                        InteractorFailure iid err -> prettyBlock '>' "Interactor Failure" iid $ showPretty err

                                        PrepStateSuccess iid (DStateDisplay ds) -> prettyBlock '>' "PrepState Complete" iid ds
                                        PrepStateFailure iid err -> prettyBlock '>' "PrepState Failure" iid $ showPretty err

                                        CheckOutcome iid (CheckReport reslt (CheckInfo chkhdr mbInfo)) -> prettyBlock 'x' ("Check: " <> showPretty (classifyResult reslt)) iid $ 
                                                                                                                                    chkhdr  <> " -> " <> showPretty reslt <> 
                                                                                                                                    ppMsgInfo mbInfo
                                        Message s -> docMarkUp $ "message: " <> s
                                        Message' detailedInfo -> detailDoc "Message" detailedInfo
                      
                                        Warning s -> docMarkUp $ "warning: " <> s
                                        Warning' detailedInfo -> detailDoc "Warning" detailedInfo

                                        e@(Error _) -> showPretty e


logConsolePrettyInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsolePrettyInterpreter = logToHandles [(logStrPP False, stdout)]

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
                            pushItem = tell . toDList . lines . logStrPP True

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

                               
