module LogTransformation.Stats where

import Pyrelude as P hiding (phase)
import qualified Data.Map.Strict as M
import RunElementClasses
import DSL.LogProtocol
import LogTransformation.Common
import Data.Aeson.TH


data StepAccum = StepAccum {
    failStage :: Maybe IterationPhase,
    phase :: IterationPhase,
    runResults :: RunResults,
    activeIteration :: Maybe (ItemId, IterationOutcome)
  } deriving Show


emptyStepAccum = StepAccum {
  failStage = Nothing,
  phase = OutOfIteration,
  runResults = RunResults M.empty M.empty,
  activeIteration = Nothing
}

statsStepForReducer :: LineNo                                             -- lineNo
                    -> StepAccum                                          -- accum
                    -> Either DeserialisationError LogProtocol            -- Logprotocol
                    -> (StepAccum, Maybe [StepAccum])                     -- (newAccum, err / result)
statsStepForReducer _ accum lp = (statsStep accum lp, Nothing)

statsStepFromLogProtocol :: StepAccum -> LogProtocol -> StepAccum
statsStepFromLogProtocol (StepAccum failStage phase runResults@(RunResults outOfTest itrRslts) activeIteration) lp = 
  let 
    (
      phaseValid :: Bool, 
      nxtPhase :: IterationPhase
      ) = phaseChange phase failStage lp

    nxtActiveItr :: Maybe (ItemId, IterationOutcome)
    nxtActiveItr = nxtIteration activeIteration lp

    nxtStatus :: ExecutionStatus
    nxtStatus = max (logProtocolStatus lp) (phaseValid ? Pass $ Fail)

    inIteration :: Bool
    inIteration = isJust nxtActiveItr

    nxtOutOfTest :: StatusCount
    nxtOutOfTest = inIteration || nxtStatus == Pass --only count out of iteration messages that have not passed 
                                  ? outOfTest 
                                  $ M.insertWith (+) nxtStatus 1 outOfTest

    nxtItrRslts :: IterationResults
    nxtItrRslts = maybef nxtActiveItr
                    itrRslts
                    (\(iid, outcome) -> M.insertWith max iid (IterationOutcome nxtStatus nxtPhase) itrRslts)

    nxtFailStage :: Maybe IterationPhase
    nxtFailStage = calcNextIterationFailStage failStage nxtStatus nxtPhase

  in 
    StepAccum {
      failStage = nxtFailStage,
      phase = nxtPhase,
      runResults = RunResults {
        outOfTest = nxtOutOfTest,
        iterationResults = nxtItrRslts
      },
      activeIteration = nxtActiveItr
    }

statsStepFromDeserialisationError :: StepAccum -> DeserialisationError -> StepAccum
statsStepFromDeserialisationError stepAccum@(StepAccum failStage phase runResults@(RunResults outOfTest itrRslts) activeIteration) lp = 
  let 
    nxtOutOfTest :: StatusCount
    nxtOutOfTest = isJust activeIteration
                         ?  outOfTest 
                         $  M.insertWith (+) Fail 1 outOfTest

    nxtItrRslts :: IterationResults
    nxtItrRslts = maybef activeIteration
                    itrRslts
                    (\(iid, outcome) -> M.insertWith max iid (IterationOutcome Fail phase) itrRslts)
  in 
    stepAccum {
      runResults = RunResults {
        outOfTest = nxtOutOfTest,
        iterationResults = nxtItrRslts
      }
    }

statsStep :: StepAccum -> Either DeserialisationError LogProtocol -> StepAccum
statsStep stepAccum@(StepAccum failStage phase runResults@(RunResults outOfTest itrRslts) activeIteration) eithLP = 
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