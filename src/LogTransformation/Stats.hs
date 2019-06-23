module LogTransformation.Stats where

import Pyrelude as P hiding (phase)
import qualified Data.Map.Strict as M
import RunElementClasses
import DSL.LogProtocol
import LogTransformation.Common
import Data.Aeson.TH


data StepAccum = StepAccum {
    runResults :: RunResults,
    stepInfo :: LPStep
  } deriving Show


emptyStepAccum = StepAccum {
  runResults = RunResults M.empty M.empty,
  stepInfo = emptyLPStep
}

statsStepForReducer :: LineNo                                             -- lineNo
                    -> StepAccum                                          -- accum
                    -> Either DeserialisationError LogProtocol            -- Logprotocol
                    -> (StepAccum, Maybe [StepAccum])                     -- (newAccum, err / result)
statsStepForReducer _ accum lp = (statsStep accum lp, Nothing)

statsStepFromLogProtocol :: StepAccum -> LogProtocol -> StepAccum
statsStepFromLogProtocol (StepAccum runResults@(RunResults outOfTest itrRslts) stepInfo) lp = 
  let 
    nxtStepInfo@(LPStep nxtPhaseValid nxtFailStage nxtPhase
                    logItemStatus nxtActiveItr) = logProtocolStep stepInfo lp

    inIteration :: Bool
    inIteration = isJust nxtActiveItr

    nxtOutOfTest :: StatusCount
    nxtOutOfTest = inIteration || logItemStatus == Pass --only count out of iteration messages that have not passed 
                                  ? outOfTest 
                                  $ M.insertWith (+) logItemStatus 1 outOfTest

    nxtItrRslts :: IterationResults
    nxtItrRslts = maybef nxtActiveItr
                    itrRslts
                    (\(iid, outcome) -> M.insertWith max iid (IterationOutcome logItemStatus nxtPhase) itrRslts)

  in 
    StepAccum {
      runResults = RunResults {
        outOfTest = nxtOutOfTest,
        iterationResults = nxtItrRslts
      },
      stepInfo = nxtStepInfo
    }

statsStepFromDeserialisationError :: StepAccum -> DeserialisationError -> StepAccum
statsStepFromDeserialisationError stepAccum@(StepAccum (RunResults outOfTest itrRslts) stepInfo) lp = 
  let 
    activeItr = activeIteration stepInfo

    nxtOutOfTest :: StatusCount
    nxtOutOfTest = isJust activeItr
                         ?  outOfTest 
                         $  M.insertWith (+) Fail 1 outOfTest

    nxtItrRslts :: IterationResults
    nxtItrRslts = maybef activeItr
                    itrRslts
                    (\(iid, outcome) -> M.insertWith max iid (IterationOutcome Fail (phase stepInfo)) itrRslts)
  in 
    stepAccum {
      runResults = RunResults {
        outOfTest = nxtOutOfTest,
        iterationResults = nxtItrRslts
      }
    }

statsStep :: StepAccum -> Either DeserialisationError LogProtocol -> StepAccum
statsStep stepAccum eithLP = 
    eitherf eithLP
      (statsStepFromDeserialisationError stepAccum)
      (statsStepFromLogProtocol stepAccum)

testExStatus :: IterationResults -> M.Map TestModule ExecutionStatus
testExStatus ir = executionStatus <$> M.mapKeysWith max tstModule ir

listTestStatus :: RunResults -> M.Map TestModule ExecutionStatus 
listTestStatus = testExStatus . iterationResults 

testStatusCounts :: RunResults -> StatusCount
testStatusCounts = countValues . listTestStatus

listIterationStatus :: RunResults -> M.Map ItemId ExecutionStatus 
listIterationStatus runResults = executionStatus <$> iterationResults runResults

itrStatusesGroupedByTest :: RunResults -> M.Map TestModule (M.Map ItemId ExecutionStatus)
itrStatusesGroupedByTest rr = 
  let 
    step :: M.Map TestModule (M.Map ItemId ExecutionStatus) -> ItemId -> ExecutionStatus -> M.Map TestModule (M.Map ItemId ExecutionStatus) 
    step accum iid status = 
       let 
         tstMod :: TestModule
         tstMod = tstModule iid

         tstMap :: M.Map ItemId ExecutionStatus
         tstMap = M.findWithDefault M.empty tstMod accum 
       in 
        M.insert tstMod (M.insert iid status tstMap) accum
  in 
    M.foldlWithKey' step M.empty $ listIterationStatus rr

testIterationStatusCounts :: RunResults -> M.Map TestModule StatusCount
testIterationStatusCounts rr = countValues <$> itrStatusesGroupedByTest rr

iterationStatusCounts :: RunResults -> StatusCount
iterationStatusCounts = countValues . listIterationStatus

worstStatus :: RunResults -> ExecutionStatus
worstStatus rr@(RunResults outOfTest _) = 
  let 
    nonZero :: StatusCount -> [ExecutionStatus]
    nonZero = M.keys . M.filter (> 0) -- should not be in map anyway

    testStatuses :: [ExecutionStatus]
    testStatuses = nonZero $ testStatusCounts rr
  in 
    null testStatuses
      ? Fail -- empty test statuses is deemed fail
      $ fromMaybe Fail $ maximum $ nonZero outOfTest <> testStatuses

$(deriveJSON defaultOptions ''StepAccum)