
module Runner (
    module Runner
  , module InternalFuncs
  , module ItemClass
) where

import Check
import DSL.Logger
import TestAndRunConfig
import           Control.Monad.Freer
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..))
import           ItemClass
import           DSL.Interpreter


data GenericTest testConfig runConfig item effs apState valState = Test {
  address :: String,
  configuration :: testConfig,
  components :: TestComponents runConfig item effs apState valState
}

data GenericResult testConfig rslt = TestResult {
  address :: String,
  configuration :: testConfig,
  results :: Either FilterError [rslt]
} deriving Show

data TestComponents runConfig item effs apState valState = TestComponents {
  testItems :: [item],
  testInteractor :: runConfig -> item -> effs,
  testPrepState :: apState -> valState
}

runTest :: (ItemClass i vs, EFFLogger effs) => rc                           -- runConfig
                            -> (i -> as -> vs -> ag)                        -- aggreagator - a constructor for the final result type
                            -> ((as -> ag) -> Eff effs as -> rslt)          -- interpreter
                            -> Filter i                                     -- item filter
                            -> GenericTest testConfig rc i (Eff effs as) as vs
                            -> GenericResult testConfig rslt
runTest runConfig aggregator interpreter filtr Test {..} = TestResult {
                                                              address = address,
                                                              configuration = configuration,
                                                              results = runSteps aggregator runConfig components interpreter filtr
                                                            }

runSteps :: (ItemClass i vs, EFFLogger effs) =>  (i -> as -> vs -> ag)      -- aggreagator - a constructor for the final result type
                            -> rc                                           -- runConfig
                            -> TestComponents rc i (Eff effs as) as vs      -- items / interactor / prepState
                            -> ((as -> ag) -> Eff effs as -> rslt)          -- interpreter
                            -> Filter i                                     -- item filter
                            -> Either FilterError [rslt]
runSteps aggregator runConfig TestComponents {..} interpreter filtr =
    let
      apStateToValState i a = aggregator i a (testPrepState a)
      interactorEffects = testInteractor runConfig
      itemToResult i = interpreter (apStateToValState i) (interactorEffects i)
    in
      (itemToResult <$>) <$> filterredItems filtr testItems

data TestInfo i as vs = TestInfo {
  item :: i,
  apState  :: Maybe as,
  valState :: Maybe vs,
  checkResult :: Maybe CheckResultList
} deriving Show

testInfoFull :: ItemClass i vs => i -> as -> vs -> TestInfo i as vs
testInfoFull item apState valState =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Just valState,
      checkResult = Just $ calcChecks valState $ checkList item
    }

testInfoFullShow :: ItemClass i vs => (TestInfo i as vs -> r) -> i -> as -> vs -> r
testInfoFullShow presenter item apState valState = presenter $ testInfoFull item apState valState

runFullTest :: (ItemClass i vs, EFFLogger effs) => rc                                  -- runConfig
                                -> TestComponents rc i (Eff effs as) as vs             -- items / interactor / prepState
                                -> ((as -> TestInfo i as vs) -> Eff effs as  -> rslt)  -- interpreter
                                -> Filter i                                            -- item filter
                                -> Either FilterError [rslt]
runFullTest = runSteps testInfoFull

testInfoNoValidation :: i -> a -> p -> TestInfo i a v
testInfoNoValidation item apState _ =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Nothing,
      checkResult = Nothing
    }

runStepsNoValidation :: (ItemClass i vs, EFFLogger effs) =>  rc                               -- runConfig
                                        -> TestComponents rc i (Eff effs as) as vs
                                        -> ((as -> TestInfo i as vs) -> Eff effs as -> rslt)  -- interpreter
                                        -> Filter i                                           -- item filter
                                        -> Either FilterError [rslt]
runStepsNoValidation = runSteps testInfoNoValidation
