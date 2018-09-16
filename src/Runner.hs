
module Runner (
  module InternalFuncs,
  module TestItem,
  runTest,
--  runFullTest,
  TestInfo(..)
) where

import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..))
import           TestItem

runTest :: (TestItem item valState) =>  (item -> a -> v -> r)              -- prepStateToTransformer
                                        -> runConfig                       -- runConfig
                                        -> (a -> v)                         -- prepState
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

-- trivial
data TestInfo i a v = TestInfo {
  item :: i,
  apState  :: a,
  valState :: v
} deriving Show

runFullTest :: (TestItem item valState) =>  runConfig                                        -- runConfig
                                          -> (a -> v)                           -- prepState
                                          -> (runConfig -> item -> apEffs)                   -- interactor
                                          -> [item]                                          -- test items
                                          -> ((a -> TestInfo item a v) -> apEffs -> result)  -- interpreter
                                          -> Filter item                                      -- item filter
                                          -> Either FilterError [result]
runFullTest = runTest TestInfo
