
module Runner (
  module InternalFuncs,
  module TestItem,
  runTest,
--  runFullTest,
  TestInfo(..)
) where

import Check
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..))
import           TestItem

runTest :: (TestItem item valState) =>  (a -> v -> c)                     -- prepStateToTransformer
                                        -> runConfig                       -- runConfig
                                        -> (runConfig -> a -> v)           -- prepState
                                        -> (runConfig -> item -> apEffs)   -- interactor
                                        -> [item]                          -- test items
                                        -> ((a -> c) -> apEffs -> result)  -- interpreter
                                        -> Filter item                     -- item filter
                                        -> Either FilterError [result]
runTest prepstateToTransformer runConfig prepState interactor items interpreter filtr =
    let
      a2c a = prepstateToTransformer a (prepState runConfig a)
      i2rslt = interpreter a2c . interactor runConfig
    in
      (i2rslt <$>) <$> filterredItems filtr items


data TestInfo a v = TestInfo {
  apState  :: a,
  valState :: v
} deriving Show

runFullTest :: (TestItem item valState) =>  runConfig                                    -- runConfig
                                          -> (runConfig -> a -> b)                      -- prepState
                                          -> (runConfig -> item -> apEffs)              -- interactor
                                          -> [item]                                     -- test items
                                          -> ((a -> TestInfo a b) -> apEffs -> result)  -- interpreter
                                          -> Filter item                                -- item filter
                                          -> Either FilterError [result]
runFullTest = runTest TestInfo
