
module Runner (
  module InternalFuncs,
  runTest,
  runTestValidate
) where

import           AppError
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       TestItem (..))

runTest :: (TestItem item) => runConfig
                                      -> (runConfig -> item -> apEffs)  -- interactor
                                      -> [item]                         -- test items
                                      -> (apEffs -> result)            -- interpreter
                                      -> Filter item                    -- item filter
                                      -> Either AppError [result]
runTest runConfig interactor items interpreter filtr = (interpreter . interactor runConfig <$>) <$> filterredItems filtr items



runTestValidate :: (TestItem item) => (runConfig -> apState -> valState)   -- prepState
                                      -> runConfig
                                      -> (runConfig -> item -> apEffs)        -- interactor
                                      -> [item]                               -- test items
                                      -> (apEffs -> apState)                  -- interpreter
                                      -> Filter item                          -- item filter
                                      -> Either AppError [(apState, valState)]
runTestValidate prepState runConfig interactor items interpreter filtr = let
                                                                           toValState = validate prepState runConfig
                                                                           valTuple apState = (apState, toValState apState)
                                                                           apStates = runTest runConfig interactor items interpreter filtr
                                                                         in
                                                                            (valTuple <$>) <$> apStates



validate :: (runConfig -> apState -> valState) ->  runConfig -> apState -> valState
validate prepState = prepState
