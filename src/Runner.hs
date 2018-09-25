
module Runner (
    module Runner
  , module InternalFuncs
  , module ItemClass
) where

import Check
import TestAndRunConfig
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..))
import           ItemClass

data GenericTest tc rc i effs as vs = Test {
  address :: String,
  configuration :: tc,
  steps :: TestSteps rc i effs as vs
}

data GenericResult tc rslt = TestResult {
  address :: String,
  configuration :: tc,
  results :: Either FilterError [rslt]
} deriving Show

data TestSteps rc i effs as vs = TestSteps {
  testInteractor :: rc -> i -> effs,
  testPrepState :: as -> vs,
  testItems :: [i]
}

runTest :: (ItemClass i vs) => rc                               -- runConfig
                            -> (i -> as -> vs -> ag)           -- aggreagator
                            -> ((as -> ag) -> effs -> rslt)     -- interpreter
                            -> Filter i                         -- item filter
                            -> GenericTest tc rc i effs as vs
                            -> GenericResult tc rslt
runTest runConfig aggregator interpreter filtr Test {..} = TestResult {
                                                              address = address,
                                                              configuration = configuration,
                                                              results = runSteps aggregator runConfig steps interpreter filtr
                                                            }

runSteps :: (ItemClass i vs) =>  (i -> as -> vs -> ag)           -- aggreagator
                            -> rc                               -- runConfig
                            -> TestSteps rc i effs as vs
                            -> ((as -> ag) -> effs -> rslt)     -- interpreter
                            -> Filter i                         -- item filter
                            -> Either FilterError [rslt]
runSteps aggregator runConfig TestSteps {..} interpreter filtr =
    let
      a2v i a = aggregator i a (testPrepState a)
      i2rslt i = interpreter (a2v i) $ testInteractor runConfig i
    in
      (i2rslt <$>) <$> filterredItems filtr testItems

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

runFullTest :: (ItemClass i vs) => rc                                        -- runConfig
                                -> TestSteps rc i effs a vs
                                -> ((a -> TestInfo i a vs) -> effs -> rslt)  -- interpreter
                                -> Filter i                                  -- item filter
                                -> Either FilterError [rslt]
runFullTest = runSteps testInfoFull

testInfoNoValidation :: i -> a -> p -> TestInfo i a v
testInfoNoValidation item apState valState =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Nothing,
      checkResult = Nothing
    }

runStepsNoValidation :: (ItemClass i vs) =>  rc                                         -- runConfig
                                        -> TestSteps rc i effs as vs
                                        -> ((as -> TestInfo i as vs) -> effs -> rslt)  -- interpreter
                                        -> Filter i                                    -- item filter
                                        -> Either FilterError [rslt]
runStepsNoValidation = runSteps testInfoNoValidation
