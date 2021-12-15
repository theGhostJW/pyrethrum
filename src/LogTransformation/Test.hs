module LogTransformation.Test where

-- import Common as C (FrameworkError(..))
-- import qualified RunElementClasses as RC
-- import LogTransformation.Common
-- import LogTransformation.Iteration as I
-- import RunElementClasses (TestFilterResult, unTestAddress)
-- import Check as CK
-- import Pyrelude as P hiding (fail)
-- import Pyrelude.IO
-- import qualified Data.DList as D
-- import DSL.LogProtocol as LP
-- import qualified Data.Aeson as A
-- import Data.Aeson.TH
-- import Data.Aeson as A
-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy as L
-- import PrettyPrintCommon as PC
-- import Data.HashMap.Strict as H

-- testStep :: LineNo                                                            -- lineNo
--             -> TestAccum                                                      -- accum
--             -> IterationLogElement                                            -- parse error or FrameworkError
--             -> (TestAccum, Either LogTransformError (Maybe [TestLogElement])) -- (newAccum, err / result)
-- testStep lineNo (TestAccum runStats mThisRec mGroup) itr =
--   let 
--     thisRec :: TestRecord
--     thisRec = fromMaybe emptyRecord mThisRec

--     defaultProp :: a -> (TestRecord -> a) -> a
--     defaultProp = maybef mThisRec

--     nxtRec :: Maybe TestRecord                   
--     nxtRec = case itr of
--               i@(Iteration ir) -> 
--                 let 
--                   iSummary = summary ir
--                   iIssues = issues iSummary
--                   iStatus = I.status iSummary
--                   tStats = stats thisRec
--                 in 
--                   Just $ thisRec {
--                               title = defaultProp "Unavailable Error In Source Logs" title,
--                               address = defaultProp "Unavailable Error In Source Logs" address,
--                               status = max (LogTransformation.Test.status thisRec) iStatus,
--                               stats = tStats {
--                                 iterationStatusCounts = incStatusCount (iterationStatusCounts tStats) iStatus,
--                                 issueCounts = issueCounts tStats <> iIssues
--                               },
--                               iterationsDesc = ir : iterationsDesc thisRec
--                             }

--               be@(BoundaryItem iaux) -> case iaux of
--                                             I.FilterLog fl -> Nothing
--                                             I.StartRun title val -> Nothing
--                                             e@I.EndRun -> Nothing
--                                             I.StartGroup gt -> Nothing
--                                             I.EndGroup gt -> Nothing
--                                             I.StartTest (RC.TestLogInfo address title config) -> Just $ emptyRecord {
--                                                                                                               title = title,
--                                                                                                               address = RC.unTestAddress address,
--                                                                                                               config = config
--                                                                                                             } 
--                                             I.EndTest tm -> Nothing
--               I.LineError le -> mThisRec

--     nxtTestLog :: Maybe TestLogElement                   
--     nxtTestLog = case itr of
--               i@(Iteration ir) -> Nothing
--               be@(BoundaryItem iaux) -> case iaux of
--                                             I.FilterLog fl -> Just $ LogTransformation.Test.FilterLog fl

--                                             I.StartRun title val -> Just $ LogTransformation.Test.StartRun title val
--                                             e@I.EndRun -> Just $ LogTransformation.Test.EndRun runStats
                                          
--                                             I.StartGroup gt -> Just $ LogTransformation.Test.StartGroup gt
--                                             I.EndGroup gt -> Just $ LogTransformation.Test.EndGroup gt
                                          
--                                             I.StartTest{} -> Nothing
--                                             I.EndTest tm -> Test <$> mThisRec
--               I.LineError le -> Nothing -- handled in nxtError

--     nxtStats :: TestStats                   
--     nxtStats = 
--       let 
--         incFailed :: TestStats
--         incFailed = 
--           let 
--             fails' = I.fail (issueCounts runStats) + 1
--           in 
--             runStats { issueCounts = (issueCounts runStats) {fail = fails'} }

--         newStats :: TestStats
--         newStats = isJust nxtError || isJust nxtPassThroughError ? incFailed $ runStats
--       in
--         case itr of
--               Iteration _ -> runStats
--               be@(BoundaryItem iaux) -> case iaux of
--                                               I.FilterLog _ -> newStats
--                                               I.StartRun{} -> newStats
--                                               I.EndRun -> newStats
--                                               I.StartGroup _ -> newStats
--                                               I.EndGroup _ -> newStats
--                                               I.StartTest{} -> newStats
--                                               I.EndTest tm -> newStats <> stats thisRec
--               I.LineError _ -> newStats
    
--     nxtGroup :: Maybe Text               
--     nxtGroup = case itr of
--                   i@Iteration{} -> mGroup
--                   bi@(BoundaryItem iaux) -> 
--                       case iaux of
--                             I.FilterLog{} -> Nothing
--                             I.StartRun{} -> Nothing
--                             I.EndRun -> Nothing
--                             I.StartGroup gt -> Just $ unGroupTitle gt
--                             I.EndGroup{} ->  Nothing

--                             I.StartTest{} -> mGroup
--                             I.EndTest tm -> mGroup
--                   I.LineError e -> mGroup
    
--     nxtError :: Maybe TestTransformError                  
--     nxtError = 
--       let 
--         transErr :: IterationLogElement -> Text ->  Maybe TestTransformError
--         transErr i txt' = Just $ IterationTransError (txt' <> "\n" <> "This could be due to lost messages or messages being received out of sequence") i
    
--         chkGroupAndTestEmpty :: IterationLogElement -> Text -> Text -> Maybe TestTransformError  
--         chkGroupAndTestEmpty i tstErrorTxt grpErrorTxt = isJust mThisRec ? transErr i tstErrorTxt 
--                                                          $ isJust mGroup ? transErr i grpErrorTxt
--                                                          $ Nothing
--       in
--         case itr of
--             i@Iteration{} -> isJust mThisRec 
--                               ? Nothing 
--                               $ transErr i "Unexpected log message encountered, an iteration message has been received before a test has started."
                                                  
--             bi@(BoundaryItem iaux) -> 
--               let 
--                 chkBothEmpty = chkGroupAndTestEmpty bi
--                 err = transErr bi
--               in 
--                 case iaux of
--                   I.FilterLog{} -> Nothing
--                   I.StartRun{} -> Nothing
--                   I.EndRun -> chkBothEmpty "End of run encountered before end of test" "End of run encountered before end of group" 
--                   I.StartGroup _ -> chkBothEmpty "Start of group encountered before end of test" "Start of group encountered before end of previous group"
--                   I.EndGroup _ -> isJust mThisRec 
--                                     ? err "End of group encountered before end of test"
--                                     $ isNothing mGroup 
--                                         ? err "End of group encountered before start of group"
--                                         $ Nothing

--                   I.StartTest{} -> isJust mThisRec 
--                                     ? err "Start of test encountered before end of previous test"
--                                     $ isNothing mGroup 
--                                         ? err "Start of test encountered before start of group"
--                                         $ Nothing

--                   I.EndTest tm -> isNothing mThisRec 
--                                     ? err "End of test encountered before start of test"
--                                     $ Nothing

--             I.LineError e -> Nothing

--     nxtPassThroughError :: Maybe LogTransformError                   
--     nxtPassThroughError = case itr of
--                             Iteration{} -> Nothing
--                             BoundaryItem iaux -> Nothing
--                             I.LineError e -> Just e

--     nxtAccum :: TestAccum
--     nxtAccum = TestAccum {
--                  runInfo = nxtStats,
--                  currentRec = nxtRec,
--                  SuiteItem = nxtGroup
--                }

--     nextResultItem :: Either LogTransformError (Maybe [TestLogElement])
--     nextResultItem = 
--       let 
--         logs = catMaybes [nxtTestLog, TransError <$> nxtError]
--         mLogs = P.null logs ? Nothing $ Just logs
--       in
--         maybe (Right mLogs) Left nxtPassThroughError
--   in 
--     (nxtAccum, nextResultItem)

-- data TestTransformError = IterationTransError Text IterationLogElement deriving (Eq, Show)

-- incStatusCount :: StatusCount -> ExecutionStatus -> StatusCount
-- incStatusCount sc@StatusCount{..} =
--   \case 
--     Inconclusive -> sc{inconclusive = inconclusive + 1}
--     I.Pass -> sc{pass = pass + 1}
--     I.Warning _ -> sc{warning = warning + 1}
--     I.Fail _ -> sc{fail = fail + 1}

-- data StatusCount = StatusCount {
--                     pass :: Int,
--                     fail :: Int,
--                     warning :: Int,
--                     inconclusive :: Int
--                   }
--                   deriving (Eq, Show)

-- instance Semigroup StatusCount where 
--   s0 <> s1 = 
--     let 
--       plus :: (StatusCount -> Int) -> Int
--       plus f = f s0 + f s1 
--     in 
--       StatusCount {
--                     pass = plus pass,
--                     warning = plus warning,
--                     fail = plus LogTransformation.Test.fail,
--                     inconclusive = plus inconclusive
--                   } 

-- instance Monoid StatusCount where 
--   mempty = StatusCount 0 0 0 0 

-- data TestStats = TestStats {
--                     iterationStatusCounts :: StatusCount,
--                     issueCounts :: Issues
--                   }
--                   deriving (Eq, Show)

-- instance Semigroup TestStats where 
--   TestStats s0 c0 <> TestStats s1 c1 = TestStats (s0 <> s1) (c0 <> c1)

-- instance Monoid TestStats where 
--   mempty = TestStats mempty mempty

-- data TestRecord = TestRecord {
--                     title :: Text,
--                     address :: Text,
--                     config :: A.Value,
--                     status :: ExecutionStatus,
--                     stats :: TestStats,
--                     iterationsDesc :: [IterationRecord]
--                   } deriving (Eq, Show)

-- emptyRecord :: TestRecord
-- emptyRecord = TestRecord {
--   title = "",
--   address = "",
--   config = A.Null,
--   status = Inconclusive,
--   stats = mempty,
--   iterationsDesc = []
-- }

-- data TestLogElement = 
--             Test TestRecord |
--             FilterLog [TestFilterResult] |

--             StartRun RunTitle A.Value | 
--             EndRun TestStats |

--             StartGroup GroupTitle |
--             EndGroup GroupTitle |

--             TransError TestTransformError 

--           deriving (Eq, Show)

-- data TestAccum = TestAccum {
--   runInfo :: TestStats,
--   currentRec :: Maybe TestRecord,
--   SuiteItem :: Maybe Text
-- }

-- emptyTestAccum :: TestAccum
-- emptyTestAccum = TestAccum mempty Nothing Nothing

-- removeHeaderAndAddress :: A.Value -> A.Value
-- removeHeaderAndAddress val = case val of
--                                Object o -> Object $ H.delete "header" $ H.delete "address" o 
--                                _ -> val
                            
-- displayStats :: TestStats -> Text
-- displayStats ts = unlines $ P.foldr (\r lst -> isInfixOf "issueCounts:" r ? "" : r : lst $ r : lst ) [] $ lines $ ppAsYaml ts

-- ppIteration :: IterationRecord -> Text
-- ppIteration (IterationRecord summary validation otherErrorsDesc otherWarningsDesc item apState domainState rawLog) = 
--   let 
--     ItemId md i = iid summary
--     headr = PC.header $ unTestAddress md <> " / " <> txt i <> " - " <> txt (I.status summary)
--   in 
--     headr <> newLn <>
--     "when: " <> unWhenClause (pre summary) <> newLn <>
--     "then: " <> unThenClause (post summary) <> newLn <>
--     "issues:" <> newLn <> ppAsYaml (issues summary) <> newLn <>
--     "checks:" <> newLn <> ppAsYaml validation <> newLn <>
--     "other errors:" <> newLn <> ppAsYaml (reverse otherErrorsDesc) <> newLn <>
--     "other warnings:" <> newLn <> ppAsYaml (reverse otherWarningsDesc)


-- {-
-- data IterationRecord = IterationRecord {
--   summary :: IterationSummary,
--   validation :: [CheckReport],
--   otherErrorsDesc :: [IterationError],
--   otherWarningsDesc :: [IterationWarning],
--   item :: Maybe ItemInfo,
--   apState :: Maybe ApStateInfo,
--   domainState :: Maybe ParserStatus,
--   rawLog :: DList LogProtocolBase
-- } deriving (Eq, Show)
-- -}

-- prettyPrintTestLogElement :: TestLogElement -> Text
-- prettyPrintTestLogElement = \case 
--                                 LogTransformation.Test.StartRun ttle rc -> ppStartRun ttle rc
--                                 LogTransformation.Test.FilterLog arFltrRslt -> ppFilterLog arFltrRslt
--                                 LogTransformation.Test.StartGroup gt -> groupHeader gt <> newLn
--                                 Test tr@(TestRecord titl address config status stats iterationsDesc) -> 
--                                   PC.header (address <> " - " <> titl <> " - " <> txt status) <> newLn <>
--                                   "Config:" <> newLn <> ppAsYaml (removeHeaderAndAddress config) <> newLn <> newLn <>
--                                   "Stats:" <> newLn <> displayStats stats <> newLn <> newLn <>
--                                   P.intercalate newLn (ppIteration <$> P.reverse iterationsDesc) 

                                  
--                                 LogTransformation.Test.EndGroup gt -> groupFooter gt
--                                 TransError err -> newLn <> PC.header "ERROR" <> newLn <> ppAesonBlock (toJSON err)
--                                 LogTransformation.Test.EndRun stats -> newLn <> PC.header "End Run" <> newLn <> ppAsYaml stats
                                                                        

-- $(deriveJSON defaultOptions ''TestLogElement)
-- $(deriveJSON defaultOptions ''TestRecord)
-- $(deriveJSON defaultOptions ''TestStats)
-- $(deriveJSON defaultOptions ''TestTransformError)
-- $(deriveJSON defaultOptions ''StatusCount)