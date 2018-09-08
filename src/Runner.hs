
module Runner (
  module InternalFuncs,
  runTest
) where

import           AppError
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..),
                                                       TestItem (..))

runTest :: (TestItem item) => runConfig
                              -> (runConfig -> a -> b)           -- prepState
                              -> (a -> b -> c)                   -- prepStateToTransformer
                              -> (runConfig -> item -> apEffs)   -- interactor
                              -> [item]                          -- test items
                              -> ((a -> c) -> apEffs -> result)  -- interpreter
                              -> Filter item                     -- item filter
                              -> Either FilterError [result]
runTest runConfig prepState prepstateToTransformer interactor items interpreter filtr =
    let
      a2c a = prepstateToTransformer a (prepState runConfig a)
      i2rslt = interpreter a2c . interactor runConfig
    in
      (i2rslt <$>) <$> filterredItems filtr items


-- runTest :: (TestItem item) => (runConfig -> apState -> valState)                            -- prepState
--                               -> runConfig
--                               -> (runConfig -> item -> apEffs)                              -- interactor
--                               -> [item]                                                     -- test items
--                               -> ((runConfig -> apState -> valState) -> runConfig -> apEffs -> valState)  --  prepState -> runConfig -> interpreter
--                               -> Filter item                                                -- item filter
--                               -> Either FilterError [(apState, valState)]
-- runTest prepState runConfig interactor items interpreter filtr =
--   let
--     toValState = validate prepState runConfig
--     valTuple apState = (apState, toValState apState)
--     apStates = runTestNoValidation runConfig interactor items (interpreter prepState runConfig) filtr
--   in
--     (valTuple <$>) <$> apStates



validate :: (runConfig -> apState -> valState) ->  runConfig -> apState -> valState
validate prepState = prepState
