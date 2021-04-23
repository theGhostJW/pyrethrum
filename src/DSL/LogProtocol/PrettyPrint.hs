
module DSL.LogProtocol.PrettyPrint (
  prettyPrintLogProtocol,
  prettyPrintLogProtocolWith,
  -- prettyPrintLogProtocolSimple,
  LogStyle(..)
) where

import Common as C ( DetailedInfo(DetailedInfo), HookLocation(..) )
import PrettyPrintCommon as PC
    ( Justification(LeftJustify)
     , newLn
     , indent2
     , majorHeader
     , fullHeader
     , ppAesonBlock
     , prettyYamlKeyValues 
     , prettyPrintFilterItem )
import DSL.LogProtocol as LP
    ( LogProtocolBase(..),
      ThreadInfo(ThreadInfo, runId, threadIndex),
      LogIndex(LogIndex),
      ItemId(ItemId),
      DStateJSON(DStateJSON),
      ApStateJSON(ApStateJSON), GroupTitle, unGroupTitle, RunTitle, unRunTitle )
import Pyrelude as P
import RunElementClasses as REC
    ( TestDisplayInfo(TestDisplayInfo, testConfig, testTitle,
                      testModAddress),
      TestAddress(TestAddress),
      toString, TestFilterResult )
import Check (ResultExpectation(..) , ExpectationActive(..), CheckReport(..), GateStatus(..), classifyResult)
import Data.Yaml as Y ( Value )

data LogStyle = Run | Doc | Outline deriving Eq

separator' :: LogStyle -> Text
separator' = \case
                Run -> newLn
                Doc -> newLn
                Outline -> " - "

describeLoc :: HookLocation -> Text
describeLoc = \case 
                 BeforeAll -> "Before All"
                 AfterAll -> "After All"
                 BeforeEach -> "Before Each"
                 AfterEach -> "After Each"

prettyPrintLogProtocolWith :: Show e => LogStyle -> ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text
prettyPrintLogProtocolWith style ThreadInfo{runId, threadIndex} (LogIndex idx) time lgProtocol = 
    runId <>  " - " <> txt threadIndex <> " - " <> txt idx <> " - " <>  txt time
    <> separator' style 
    <> prettyPrintLogProtocol style lgProtocol

prettyPrintLogProtocolSimple :: Show e => LogProtocolBase e -> Text
prettyPrintLogProtocolSimple = prettyPrintLogProtocolBase Nothing Outline

prettyPrintLogProtocol :: Show e => LogStyle -> LogProtocolBase e -> Text
prettyPrintLogProtocol = prettyPrintLogProtocolBase Nothing

iterId :: ItemId -> Text
iterId (ItemId tst iid) = toString tst <> " / item " <> txt iid

prettyPrintLogProtocolBase :: Show e => Maybe Text -> LogStyle -> LogProtocolBase e -> Text
prettyPrintLogProtocolBase _mTimeSuffix style =
  let 
    isOutline = style == Outline 
    hdr :: Text -> Text -> Text
    hdr l h = isOutline ? h $ l <> " " <> h <> " " <> l
    subHeader = hdr "----"
    header = hdr "===="
    tstHeader = hdr "==="
    itrHeader = hdr "=="

    ppStartRun :: RunTitle -> Y.Value -> Text
    ppStartRun ttle rc = majorHeader isOutline (unRunTitle ttle) <> 
                          newLn <> newLn <> "Run Config:" <>
                          newLn <> ppAesonBlock rc

    groupHeader :: GroupTitle -> Text
    groupHeader = groupTitle "Group"

    groupFooter :: GroupTitle -> Text
    groupFooter = groupTitle "End Group"

    groupTitle :: Text -> GroupTitle -> Text
    groupTitle hdr' gt = header $ hdr' <> " - " <> unGroupTitle gt

    ppFilterLog :: [TestFilterResult] -> Text
    ppFilterLog fltrInfos = newLn <> header "Filter Log" <> newLn <>
                        foldl' (\acc fi -> acc <> fi <> newLn) "" (prettyPrintFilterItem <$> fltrInfos)

    separator :: Text
    separator = separator' style

    prettyBlock :: Char -> Text -> ItemId -> Text -> Text
    prettyBlock pfxChr headr iid body = indent2 $ toS (replicate 4 ' ') <> singleton pfxChr <> " " <> headr <> " - " <> iterId iid <> separator <> indent2 body

    ppMsgInfo :: Text -> Text
    ppMsgInfo dtl = null dtl ?
                              "" $
                              separator <> indent2 (txtPretty dtl)

    docMarkUp :: Text -> Text
    docMarkUp s = (style == Doc ? id $ indent2) $ (style == Doc ? "  >> " $ "") <> s

    logIO :: Show a => a -> Text
    logIO m =  docMarkUp $ "IO Action: " <> txtPretty m

    logIO' :: DetailedInfo -> Text
    logIO' (DetailedInfo m i) = docMarkUp $ "IO Action: " <> txtPretty m <> "\n" <> txtPretty i

    detailDoc :: Text -> DetailedInfo -> Text
    detailDoc hedr (DetailedInfo msg det) = separator <> (style == Doc ? id $ indent2) (subHeader hedr <> separator <> msg <> separator <> det)                                                                                                  
  in
    \case
        LP.FilterLog fltrInfos -> ppFilterLog fltrInfos
        LP.StartRun ttle _offset rc -> ppStartRun ttle rc

        LP.StartGroup gt -> groupHeader gt
        LP.EndGroup gt -> groupFooter gt

        LP.StartHook loc title -> "Start " <> describeLoc loc <> " Hook: " <> title
        LP.EndHook loc title ->   "End " <> describeLoc loc <> " Hook: "  <> title

        LP.StartTest TestDisplayInfo{..} -> separator <> tstHeader ("Start Test: " <> toString testModAddress <> " - " <> testTitle) <> 
                                          separator <> "Test Config:" <>
                                          separator <> ppAesonBlock testConfig

        EndTest (TestAddress address) -> separator <> tstHeader ("End Test: " <> address)
        StartIteration iid  _ _ val -> separator <> subHeader ("Start Iteration: " <> iterId iid) <> 
                                        separator <> "Item:" <> 
                                        separator <> ppAesonBlock val <>
                                        (style == Doc ? "" $ separator)

        EndIteration iid -> separator <> subHeader ("End Iteration: " <> iterId iid)
        LP.EndRun -> separator <> header "End Run"

        StartInteraction -> separator <> "Interaction:"
        StartChecks -> separator <> "Checks:"
        StartParser -> separator <> "Domain:"

        IOAction m -> indent2 $ logIO m 
        IOAction' i -> logIO' i
        
        InteractorSuccess iid (ApStateJSON as) -> 
          separator <> 
            ( 
              isOutline ?
                "Interactor Complete " <> iterId iid $
                prettyBlock '>' "Interactor Complete" iid (prettyYamlKeyValues 2 LeftJustify as)
            )
          
        InteractorFailure iid err -> prettyBlock '>' "Interactor Failure" iid $ txtPretty err

        ParserSuccess iid (DStateJSON ds) -> prettyBlock '>' "ParseComplete" iid $ prettyYamlKeyValues 2 LeftJustify ds
        ParserSkipped iid -> docMarkUp $ "ParseSkipped: " <> txt iid
        ParserFailure iid err -> prettyBlock '>' "ParseFailure" iid $ txtPretty err

        CheckOutcome iid (CheckReport reslt (DetailedInfo chkhdr info)) -> prettyBlock 'x' ("Check: " <> txtPretty (classifyResult reslt)) iid $ 
                                                                                                    chkhdr  <> " -> " <> txtPretty reslt <> 
                                                                                                    ppMsgInfo info
        Message s -> docMarkUp $ "message: " <> s
        Message' detailedInfo -> detailDoc "Message" detailedInfo

        Warning s -> docMarkUp $ "warning: " <> s
        Warning' detailedInfo -> detailDoc "Warning" detailedInfo

        e@(Error _) -> txtPretty e
