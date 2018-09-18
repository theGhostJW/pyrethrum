
module Runner (
  module InternalFuncs,
  module TestItem,
  runTest,
  runFullTest,
  runTestNoValidation,
  TestRunElements(..),

  TestInfo(..)
) where

import Check
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..))
import           TestItem

class TestConfigCapabilities where
  testTitle :: a -> String
  active :: a -> Bool

class RunConfigCapabilities where
  runTitle :: a -> String


data Test t runConfig item apEffs apState valState = Test {
  testConfig :: t,
  runElements :: TestRunElements runConfig item apEffs apState valState
}

data TestRunElements runConfig item apEffs apState valState = TestRunElements {
  testInteractor :: runConfig -> item -> apEffs,
  testPrepState :: apState -> valState,
  testItems :: [item]
}

runTest :: (TestItem item valState) =>  (item -> a -> valState -> r)       -- prepStateToTransformer
                                        -> runConfig                       -- runConfig
                                        -> TestRunElements runConfig item apEffs a valState
                                        -> ((a -> r) -> apEffs -> result)  -- interpreter
                                        -> Filter item                     -- item filter
                                        -> Either FilterError [result]
runTest prepstateToTransformer runConfig TestRunElements {..} interpreter filtr =
    let
      a2v i a = prepstateToTransformer i a (testPrepState a)
      i2rslt i = interpreter (a2v i) $ testInteractor runConfig i
    in
      (i2rslt <$>) <$> filterredItems filtr testItems


data TestInfo i a v = TestInfo {
  item :: i,
  apState  :: Maybe a,
  valState :: Maybe v,
  checkResult :: Maybe CheckResultList
} deriving Show

testInfoFull :: TestItem item valState => item -> apState -> valState -> TestInfo item apState valState
testInfoFull item apState valState =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Just valState,
      checkResult = Just $ calcChecks valState $ checkList item
    }

runFullTest :: (TestItem item valState) => runConfig                                              -- runConfig
                                        -> TestRunElements runConfig item apEffs a valState
                                        -> ((a -> TestInfo item a valState) -> apEffs -> result)  -- interpreter
                                        -> Filter item                                            -- item filter
                                        -> Either FilterError [result]
runFullTest = runTest testInfoFull

testInfoNoValidation :: i -> a -> p -> TestInfo i a v
testInfoNoValidation item apState valState =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Nothing,
      checkResult = Nothing
    }


runTestNoValidation :: (TestItem item valState) =>  runConfig                                             -- runConfig
                                                -> TestRunElements runConfig item apEffs a valState
                                                -> ((a -> TestInfo item a valState) -> apEffs -> result)  -- interpreter
                                                -> Filter item                                            -- item filter
                                                -> Either FilterError [result]
runTestNoValidation = runTest testInfoNoValidation
