
module DSL.LogProtocol.PrettyPrint (
  prettyPrintLogProtocol
) where

import Common
import PrettyPrintCommon as PC
import  DSL.LogProtocol as LP
import LogTransformation.Test as T
import           Pyrelude as P
import Text.Show.Pretty as PP
import RunElementClasses as C
import Check (ResultExpectation(..) , ExpectationActive(..), CheckReport(..), CheckInfo(..), GateStatus(..), classifyResult)
import Data.Yaml as Y

prettyPrintLogProtocol :: Bool -> LogProtocol -> Text
prettyPrintLogProtocol docMode =
  let
    iterId :: ItemId -> Text
    iterId (ItemId tst iid) = toString tst <> " / item " <> txt iid

    prettyBlock :: Char -> Text -> ItemId -> Text -> Text
    prettyBlock pfxChr headr iid body = indent2 $ toS (replicate 3 ' ') <> " " <> headr <> " - " <> iterId iid <> newLn <> indent2 body

    ppMsgInfo :: Show a => Maybe a -> Text
    ppMsgInfo mbInfo = maybef mbInfo
                              ""
                              (\m -> newLn <> indent2 (txtPretty m))

    docMarkUp :: Text -> Text
    docMarkUp s = (docMode ? id $ indent2) $ (docMode ? "  >> " $ "") <> s

    logIO :: Show a => a -> Text
    logIO m =  docMarkUp $ "IO Action: " <> txtPretty m

    detailDoc :: Text -> DetailedInfo -> Text
    detailDoc hedr (DetailedInfo msg det) = newLn <> (docMode ? id $ indent2) (subHeader hedr <> newLn <> msg <> newLn <> det)
                                                                                                      
  in
    \case
        BoundaryLog bl -> case bl of 
                            LP.FilterLog fltrInfos -> ppFilterLog fltrInfos
                            LP.StartRun ttle rc -> ppStartRun ttle rc

                            LP.StartGroup gt -> groupHeader gt
                            LP.EndGroup gt -> groupFooter gt

                            LP.StartTest TestDisplayInfo{..} -> newLn <> tstHeader ("Start Test: " <> toString testModAddress <> " - " <> testTitle) <> 
                                                              newLn <> "Test Config:" <>
                                                              newLn <> ppAesonBlock testConfig

                            EndTest (TestModule address) -> newLn <> tstHeader ("End Test: " <> address)
                            StartIteration iid  _ _ val -> newLn <> subHeader ("Start Iteration: " <> iterId iid) <> 
                                                            newLn <> "Item:" <> 
                                                            newLn <> ppAesonBlock val <>
                                                            (docMode ? "" $ newLn)

                            EndIteration iid -> newLn <> subHeader ("End Iteration: " <> iterId iid)
                            LP.EndRun -> newLn <> PC.header "End Run"

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

                              e@(DocError _) -> txtPretty e

        IterationLog (Run rp) -> case rp of
                              StartInteraction -> newLn <> "Interaction:"
                              StartChecks -> newLn <> "Checks:"
                              StartPrepState -> newLn <> "PrepState:"
            
                              IOAction m -> indent2 $ logIO m 
                              
                              InteractorSuccess iid (ApStateDisplay as) -> newLn <> prettyBlock '>' "Interactor Complete"  iid as
                                
                              InteractorFailure iid err -> prettyBlock '>' "Interactor Failure" iid $ txtPretty err

                              PrepStateSuccess iid (DStateDisplay ds) -> prettyBlock '>' "PrepState Complete" iid ds
                              PrepStateFailure iid err -> prettyBlock '>' "PrepState Failure" iid $ txtPretty err

                              CheckOutcome iid (CheckReport reslt (CheckInfo chkhdr mbInfo)) -> prettyBlock 'x' ("Check: " <> txtPretty (classifyResult reslt)) iid $ 
                                                                                                                          chkhdr  <> " -> " <> txtPretty reslt <> 
                                                                                                                          ppMsgInfo mbInfo
                              Message s -> docMarkUp $ "message: " <> s
                              Message' detailedInfo -> detailDoc "Message" detailedInfo
            
                              Warning s -> docMarkUp $ "warning: " <> s
                              Warning' detailedInfo -> detailDoc "Warning" detailedInfo

                              e@(Error _) -> txtPretty e
