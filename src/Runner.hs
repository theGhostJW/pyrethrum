
module Runner (
  module InternalFuncs,
  module TestItem,
  runTest,
  runFullTest,
  runTestNoValidation,
  TestInfo(..)
) where

import Check
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..))
import           TestItem

runTest :: (TestItem item valState) =>  (item -> a -> valState -> r)       -- prepStateToTransformer
                                        -> runConfig                       -- runConfig
                                        -> (a -> valState)                 -- prepState
                                        -> (runConfig -> item -> apEffs)   -- interactor
                                        -> [item]                          -- test items
                                        -> ((a -> r) -> apEffs -> result)  -- interpreter
                                        -> Filter item                     -- item filter
                                        -> Either FilterError [result]
runTest prepstateToTransformer runConfig prepState interactor items interpreter filtr =
    let
      a2v i a = prepstateToTransformer i a (prepState a)
      i2rslt i = interpreter (a2v i) $ interactor runConfig i
    in
      (i2rslt <$>) <$> filterredItems filtr items


data TestInfo i a v = TestInfo {
  item :: i,
  apState  :: Maybe a,
  valState :: Maybe v,
  validationResult :: Maybe CheckResultList
} deriving Show

testInfoFull :: TestItem item valState => item -> apState -> valState -> TestInfo item apState valState
testInfoFull item apState valState =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Just valState,
      validationResult = Just $ calcChecks valState $ validation item
    }

runFullTest :: (TestItem item valState) => runConfig                                              -- runConfig
                                        -> (a -> valState)                                        -- prepState
                                        -> (runConfig -> item -> apEffs)                          -- interactor
                                        -> [item]                                                 -- test items
                                        -> ((a -> TestInfo item a valState) -> apEffs -> result)  -- interpreter
                                        -> Filter item                                            -- item filter
                                        -> Either FilterError [result]
runFullTest = runTest testInfoFull

testInfoNoValidation :: TestItem item valState => item -> apState -> valState -> TestInfo item apState valState
testInfoNoValidation item apState valState =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Just valState,
      validationResult = Just $ calcChecks valState $ validation item
    }


runTestNoValidation :: (TestItem item valState) =>  runConfig                                             -- runConfig
                                                -> (a -> valState)                                        -- prepState
                                                -> (runConfig -> item -> apEffs)                          -- interactor
                                                -> [item]                                                 -- test items
                                                -> ((a -> TestInfo item a valState) -> apEffs -> result)  -- interpreter
                                                -> Filter item                                            -- item filter
                                                -> Either FilterError [result]
runTestNoValidation = runTest testInfoNoValidation
