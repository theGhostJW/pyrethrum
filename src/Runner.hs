
module Runner (
  module InternalFuncs,
  runTest,
  runFullTest
) where

import           AppError
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..),
                                                       TestItem (..))

runTest :: (TestItem item) =>  (a -> b -> c)                     -- prepStateToTransformer
                              -> runConfig                       -- runConfig
                              -> (runConfig -> a -> b)           -- prepState
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

runFullTest = undefined
