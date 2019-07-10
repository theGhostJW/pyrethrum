module LogTransformation.PrintLogDisplayElement (
  emptyIterationAccum,
  printLogDisplayStep,
  prettyPrintDisplayElement,
  IterationAccum(..),
  PrintLogDisplayElement(..),
  IterationRecord(..),
  LogTransformError(..),
  IterationError(..),
  IterationWarning(..),
  ApStateInfo(..),
  PrepStateInfo(..)
) where

import Common as C (AppError(..), DetailedInfo(..))
import LogTransformation.Common as LC
import Check as CK
import Pyrelude as P
import Data.DList as D
import qualified DSL.LogProtocol as LP
import DSL.LogProtocol hiding (StartRun)
import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import RunElementClasses
import LogTransformation.Stats
import PrettyPrintCommon
import Data.Yaml.Pretty as YP
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

-- TODO: creation relational records
-- relational records from Iteration records and use reporting service
-- to provide full report - use sql lite locally 
-- see https://www.oreilly.com/library/view/microservices-antipatterns-and/9781492042716/ch04.html


--------------------------------------------------------
----------------- Iteration Aggregation ----------------
--------------------------------------------------------

data PrintLogDisplayElement =
  FilterLog [FilterResult] |

  StartRun {  
        title :: RunTitle, 
        config :: A.Value, 
        runStatus :: ExecutionStatus,
        testStats :: StatusCount, 
        iterationStats :: StatusCount,
        outOfTest :: StatusCount
    } | 
  EndRun (Maybe [FilterResult]) |

  -- StartGroup GroupTitle |
  -- EndGroup GroupTitle |

  StartTest {  
    tstTitle :: Text,
    modAddress :: TestModule,
    notes :: Maybe Text,
    config :: A.Value, -- test Config as Json
    status :: ExecutionStatus, -- test Config as Json
    stats :: StatusCount
  }  |
  -- EndTest TestModule |
  Iteration IterationRecord |
  LineError LogTransformError
  deriving (Show, Eq)

data IterationSummary = IterationSummary {
                          modulePath :: Text,
                          itmId :: Int,
                          notes :: Maybe Text,
                          pre :: Text,
                          post:: Text,
                          status :: ExecutionStatus
                        } deriving (Eq, Show)

data IterationError = IterationError {
    phase :: IterationPhase,
    error :: LogProtocol
  } deriving (Eq, Show)
                          
data IterationWarning = IterationWarning {
    phase :: IterationPhase,
    warning :: LogProtocol
  } deriving (Eq, Show)

data ApStateInfo = SucceededInteractor ApStateJSON | 
                   FailedInteractor AppError
                   deriving (Eq, Show)

data PrepStateInfo = SucceededPrepState DStateJSON |
                     FailedPrepState AppError
                     deriving (Eq, Show)

data IterationRecord = IterationRecord {
  modulePath :: Text,
  itmId :: Int,
  notes :: Maybe Text,
  pre :: Text,
  post:: Text,
  outcome :: IterationOutcome,
  validation :: [CheckReport],
  otherErrors :: [IterationError],
  otherWarnings :: [IterationWarning],
  item :: Maybe A.Value,
  apState :: Maybe ApStateInfo,
  domainState :: Maybe PrepStateInfo
} deriving (Eq, Show)

data IterationAccum = IterationAccum {
  rec :: Maybe IterationRecord,
  stepInfo :: LPStep,
  filterLog :: Maybe [FilterResult]
} deriving Show

emptyIterationAccum :: IterationAccum
emptyIterationAccum = IterationAccum {
  rec = Nothing,
  stepInfo = emptyLPStep,
  filterLog = Nothing
}

printLogDisplayStep ::
              RunResults                                                            -- RunResults
              -> LineNo                                                             -- lineNo
              -> IterationAccum                                                     -- accum
              -> Either DeserialisationError LogProtocol                                                        -- parse error or apperror
              -> (IterationAccum, Maybe [PrintLogDisplayElement]) -- (newAccum, err / result)
printLogDisplayStep runResults lineNo oldAccum@(IterationAccum mRec stepInfo mFltrLg) eithLp = 
  
  eitherf eithLp
   (\err -> 
      let 
        nxtFailStage = calcNextIterationFailStage (faileStage stepInfo) LC.Fail (LC.phase stepInfo) Nothing
        nxtStepInfo = stepInfo {faileStage = nxtFailStage}
      in
        (
          oldAccum { stepInfo = nxtStepInfo} :: IterationAccum, 
          Just [LineError $ LogDeserialisationError err]
        )
   )

   (\lp ->
     let 
        skipLog = (oldAccum, Nothing)

        RunResults outOfTest iterationResults = runResults

        -- itrOutcome ItemId -> Maybe IterationOutcome
        testStatuses :: M.Map TestModule ExecutionStatus
        testStatuses = testExStatus iterationResults

        testStatus :: TestModule -> ExecutionStatus
        testStatus tm = fromMaybe LC.Fail $ M.lookup tm testStatuses

        tstIterationStatusCounts :: M.Map TestModule StatusCount
        tstIterationStatusCounts = testIterationStatusCounts runResults

        testItrStats :: TestModule -> StatusCount
        testItrStats tm = M.findWithDefault M.empty tm tstIterationStatusCounts

        elOut :: a -> Maybe [a]
        elOut a = Just [a]

        nxtStepInfo@(LPStep nxtPhaseValid nxtFailStage nxtPhase
                       logItemStatus nxtActiveItr nxtCheckEncountered) = logProtocolStep stepInfo lp

        accum :: IterationAccum
        accum = oldAccum {stepInfo = nxtStepInfo}

        lineError :: Text -> Maybe [PrintLogDisplayElement]
        lineError txt' = Just [LineError $ LogTransformError lineNo lp txt']

        getNotes :: Y.Value -> Maybe Text
        getNotes = 
          let 
            txtOf :: Y.Value -> Maybe Text
            txtOf = 
              \case
                Y.Object obj -> Nothing
                Y.Array _ -> Nothing
                Y.String txt' -> Just txt'
                Y.Number _ -> Nothing 
                Y.Bool _ -> Nothing 
                Y.Null -> Nothing 
          in
            \case
              Y.Object obj -> firstJust [HM.lookup "notes" obj, HM.lookup "note" obj] >>= txtOf
              Y.Array _ -> Nothing
              Y.String _ -> Nothing
              Y.Number _ -> Nothing 
              Y.Bool _ -> Nothing 
              Y.Null -> Nothing 

        updateItrRec :: (IterationRecord -> IterationRecord) -> (IterationAccum, Maybe [PrintLogDisplayElement]) 
        updateItrRec func = 
          let 
            mIrec :: Maybe IterationRecord
            mIrec = rec accum
          in 
            maybef mIrec
              (accum, lineError "An iteration event has been encounterred before the start iteration event - possible loss of log event - check raw logs")
              (\irec -> (accum { rec = Just $ func irec }, Nothing))
              
        in
          case lp of
            BoundaryLog bl-> 
              case bl of
                LP.FilterLog flgs -> (accum {filterLog = Just flgs}, Nothing) 

                LP.StartRun runTitle jsonCfg -> (accum, elOut $ StartRun {  
                  title = runTitle, 
                  config = jsonCfg, 
                  runStatus = worstStatus runResults,
                  testStats = testStatusCounts runResults, 
                  iterationStats = iterationStatusCounts runResults,
                  outOfTest = outOfTest
                } )
                LP.EndRun -> (accum, elOut . LogTransformation.PrintLogDisplayElement.EndRun $ filterLog accum)
                
                LP.StartGroup _ -> skipLog
                LP.EndGroup _ -> skipLog
            
                LP.StartTest (TestDisplayInfo testModAddress testTitle testConfig) -> (
                      accum, elOut $ 
                              LogTransformation.PrintLogDisplayElement.StartTest 
                                testTitle
                                testModAddress
                                (getNotes testConfig)
                                testConfig
                                (testStatus testModAddress)
                                (testItrStats testModAddress)
                  )
                LP.EndTest _ -> skipLog
            
                StartIteration iid@(ItemId tstModule itmId) (WhenClause whn) (ThenClause thn) jsonItmVal ->  
                  (accum 
                    {rec = Just $ IterationRecord {
                      modulePath = unTestModule tstModule,
                      itmId = itmId,
                      notes = getNotes jsonItmVal,
                      pre = whn,
                      post = thn,
                      outcome = M.findWithDefault (IterationOutcome LC.Fail OutOfIteration) iid iterationResults,
                      validation = P.empty,
                      otherErrors = P.empty,
                      otherWarnings = P.empty,
                      item = Just jsonItmVal,
                      apState = Nothing,
                      domainState = Nothing
                    } 
                  }, Nothing)

                EndIteration (ItemId tstModule itmId) -> 
                  let 
                    severityDesc :: CheckReport -> CheckReport -> Ordering
                    severityDesc (CheckReport rslt1 _) (CheckReport rslt2 _) = compare (Down rslt1) (Down rslt2)

                    sortChecks :: IterationRecord -> IterationRecord
                    sortChecks ir = ir { validation = sortBy severityDesc $ validation ir}
                  in
                    (
                      accum {rec = Nothing}, 
                      elOut $ 
                        maybef (rec accum)
                          (LineError $ LogTransformError {
                            linNo = lineNo,
                            logItem = lp,
                            info = "Error end iteration message encountered when the before start iteration - check raw logs"
                          })
                          (Iteration . sortChecks)
                    ) 

            IterationLog subProtocol -> 
              case subProtocol of
                Doc dp -> (accum, lineError "Documentation log item encounterred in live test log - this should not happen - probably a bug in the test runner")
                Run action -> 
                  case action of
                    IOAction txt' -> skipLog
                    StartPrepState -> skipLog
                    StartInteraction -> skipLog
                    InteractorSuccess iid apStateJSON -> updateItrRec (\ir -> ir {apState = Just $ SucceededInteractor apStateJSON})
                    InteractorFailure iid err -> updateItrRec (\ir -> ir {apState = Just $ FailedInteractor err})
                  
                    PrepStateSuccess iid dStateJSON -> updateItrRec (\ir -> ir {domainState = Just $ SucceededPrepState dStateJSON})
                  
                    PrepStateFailure iid err -> updateItrRec (\ir -> ir {domainState = Just $ FailedPrepState err})
                    StartChecks -> skipLog 
                    CheckOutcome itmId chkReport -> updateItrRec (\ir -> ir {validation = chkReport : validation ir})
    
                    Message txt' -> skipLog
                    Message' (DetailedInfo msg txt') -> skipLog
                  
                    LP.Warning _ -> updateItrRec (\ir -> ir {otherWarnings = IterationWarning nxtPhase lp : otherWarnings ir})
                    Warning' (DetailedInfo msg txt') -> updateItrRec (\ir -> ir {otherWarnings = IterationWarning nxtPhase lp : otherWarnings ir})
    
                    LP.Error err -> updateItrRec (\ir -> ir {otherErrors = IterationError nxtPhase lp : otherErrors ir})
      )

prettyPrintDisplayElement :: PrintLogDisplayElement -> Text
prettyPrintDisplayElement pde = 
  let 
    noImp = ""
    newLn2 = newLn <> newLn
    header' ttlTxt status = ttlTxt <> " - " <> toTitle (statusLabel False status) <> " - 00:00:88"
  in 
    case pde of
      LogTransformation.PrintLogDisplayElement.FilterLog arrFr -> noImp

      LogTransformation.PrintLogDisplayElement.StartRun (RunTitle titl) config runStatus testStats iterationStats outOfTest -> 
            majorHeader (header' titl runStatus) 
                <> newLn
                <> "toDo - start end duration raw file" 
                <> newLn2 
                <> "runConfig: " 
                <> newLn 
                <> prettyYamlKeyValues 2 LeftJustify config
                <> newLn
                <> "test stats:" 
                <> newLn
                <> alignKeyValues True 2 RightJustify (statusCountTupleText False testStats)
                <> newLn
                <> "iteration stats:" 
                <> newLn
                <> alignKeyValues True 2 RightJustify (statusCountTupleText False iterationStats)
                <> newLn
                <> "out of test issues:" 
                <> newLn
                <> alignKeyValues True 2 RightJustify (statusCountTupleText True outOfTest)

      LogTransformation.PrintLogDisplayElement.EndRun mFltrLog -> 
        majorHeader "End Run"
        <> newLn
        <> fullHeader '-' False "Filter Log"
        <> newLn
        <> maybef mFltrLog
             "  No Filter Log Available"
             (
               \fltrs -> 
                let 
                  fltrItems :: (Maybe Text -> Bool) -> [FilterResult]
                  fltrItems f = P.filter (f . reasonForRejection) fltrs

                  acceptedItems:: [FilterResult]
                  acceptedItems = fltrItems isNothing

                  rejectedItems :: [FilterResult]
                  rejectedItems = fltrItems isJust

                  address :: FilterResult -> Text
                  address = unTestModule . testModAddress . testInfo 

                  rejectText :: FilterResult -> Text
                  rejectText fr = maybef (reasonForRejection fr) 
                                    "ACCEPTED" -- should never happen
                                    (\rtxt -> address fr <> " - " <> rtxt)

                  accepted :: Text
                  accepted = P.null acceptedItems 
                              ? "  No Tests Accepted" 
                              $ indent2 . P.unlines . P.sort $ ("- " <>) . address <$> acceptedItems

                  rejected :: Text
                  rejected = P.null rejectedItems 
                              ? "  No Tests Rejected" 
                              $ indent2 . P.unlines . P.sort $ ("- " <>) . rejectText <$> rejectedItems
                in
                 "accepted: " 
                    <> newLn 
                    <> accepted 
                    <> newLn2
                    <> "rejected:"
                    <> newLn 
                    <> rejected
             )
        <> newLn


      LogTransformation.PrintLogDisplayElement.StartTest titl tstMod notes cfg status itrStats -> 
          majorHeader (header' titl status) 
           <> newLn
           <> "module:" 
           <> newLn
           <> indent2 (unTestModule tstMod)
           <> newLn2 
           <> "stats:" 
           <> newLn
           <> alignKeyValues True 2 RightJustify (statusCountTupleText False itrStats)

      LogTransformation.PrintLogDisplayElement.Iteration (
          IterationRecord
            modulePath
            itmId
            notes
            when'
            then'
            outcome@(IterationOutcome status phse)
            validation
            otherErrors
            otherWarnings
            item
            apStateInfo
            domainState) -> 
              let 
                
                hdrLines :: [(Text, Text)]
                hdrLines = 
                  let baseLines = [
                       ("when", when')
                       , ("then", then')
                       , ("status", statusLabel False status <> (status == LC.Pass ? "" $ " - " <> txt phse))
                       ]
                  in 
                    maybef notes 
                      baseLines
                      (\n -> P.snoc baseLines ("notes:", n))

                keyOrdering :: Text -> Text -> Ordering
                keyOrdering k1 k2 = 
                  let 

                    pos :: Text -> Int
                    pos k 
                      | "iid" == k = 0
                      | "pre" == k = 1
                      | "post" == k = 2
                      | "notes" == k = 3
                      | "checks" == k = 100
                      | otherwise = 10
                  in
                    compare (pos k1) (pos k2)

                valLine :: CheckReport -> (Text, Text)
                valLine (CheckReport result (MessageInfo hder extrInfo)) = (hder, toLower $ txt result)

                detailValLine :: CheckReport -> (Text, Text)
                detailValLine (CheckReport result (MessageInfo hder extrInfo)) = (hder, toLower $ txt result <> 
                                            maybef extrInfo 
                                            " - no additional info " 
                                            (\exInfo -> newLn <> "additional info:" <> newLn <> indent2 exInfo)
                                          )

                dsText :: Text
                dsText = maybef domainState
                          "- Domain State is Empty"
                          (
                            \case 
                              SucceededPrepState dsDisplay -> prettyYamlKeyValues 2 LeftJustify $ unDStateJSON dsDisplay
                              FailedPrepState err -> "PrepState Failure - Domain State is Empty:\n" <> indent2 ("- " <> txtPretty err)
                          )
              in
                iterationHeader (header' (modulePath <> " - " <> txt itmId) status)
                <> newLn
                <> alignKeyValues True 0 LeftJustify hdrLines
                <> newLn
                <> "validation:"
                <> newLn
                <> (P.null validation ? "  - Test Design Error - This test has no checks\n" $ alignKeyValues True 2 LeftJustify (valLine <$> validation))
                <> newLn
                <> "domain state:"
                <> newLn
                <> indent2 dsText
                <> newLn2
               
                <> "application state:"
                <> newLn
                <> maybef apStateInfo "  No ApState Recorded" (indent2 . getLenient . convertString . encodePretty defConfig)
                
                <> (
                P.null validation 
                   ? newLn 
                   $ newLn2 
                   <> "validation details:"
                   <> newLn
                   <> alignKeyValues True 2 LeftJustify (detailValLine <$> validation)
                )
                <> newLn

                <> "full item:"
                <> newLn
                <> maybef item "  No Item Recorded" (indent2 . getLenient . convertString . encodePretty (setConfCompare keyOrdering defConfig))
                <> newLn

      LineError err -> fullHeader '!' True "ERROR"
                       <> newLn
                       <> indent2 (txt err)

statusLabel :: Bool -> ExecutionStatus -> Text
statusLabel outOfTest = \case
                          LC.Pass -> "pass"
                          LC.Fail -> outOfTest ? "error" $ "fail"
                          LC.Regression -> "regression"
                          Type2Error -> "type 2 error"
                          LC.Warning -> "warning"
                          KnownError -> "known error"

statusCountTupleText :: Bool -> StatusCount -> [(Text, Text)]
statusCountTupleText outOfTest sc = 
  let
    chkOnlyStatuses :: [ExecutionStatus]
    chkOnlyStatuses = [KnownError, Type2Error, LC.Regression, LC.Pass]

    defList :: [ExecutionStatus]
    defList = outOfTest ? enumList \\ chkOnlyStatuses $ enumList
    
    defaults :: StatusCount
    defaults = M.fromList $ (, 0) <$> defList

    displayOrder :: (ExecutionStatus, a) -> (ExecutionStatus, a) -> Ordering
    displayOrder (es1, _) (es2, _) =
      let 
        exOrd :: ExecutionStatus -> Int
        exOrd  = \case
                    LC.Pass -> 0
                    LC.Fail -> 1
                    LC.Regression -> 2
                    Type2Error -> 3
                    KnownError -> 4
                    LC.Warning -> 5
      in 
        compare (exOrd es1) (exOrd es2)

    statusTuples :: [(Text, Text)]
    statusTuples = bimap (statusLabel outOfTest) txt <$> sortBy displayOrder (M.toList (M.union sc defaults))
  in
    outOfTest 
        ? statusTuples 
        $ ("total", txt . sum $ M.elems sc) : statusTuples

$(deriveJSON defaultOptions ''PrintLogDisplayElement)
$(deriveJSON defaultOptions ''IterationRecord)
$(deriveJSON defaultOptions ''ApStateInfo)
$(deriveJSON defaultOptions ''IterationWarning)
$(deriveJSON defaultOptions ''IterationError)
$(deriveJSON defaultOptions ''PrepStateInfo)