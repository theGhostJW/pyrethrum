
module Runner (
  module InternalFuncs,
--  runTest,
  runTestNoValidation
) where

import           AppError
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..),
                                                       TestItem (..))

runTestNoValidation :: (TestItem item) => runConfig
                                      -> (runConfig -> item -> apEffs) -- interactor
                                      -> [item]                        -- test items
                                      -> (apEffs -> result)            -- interpreter
                                      -> Filter item                   -- item filter
                                      -> Either FilterError [result]
runTestNoValidation runConfig interactor items interpreter filtr = (interpreter . interactor runConfig <$>) <$> filterredItems filtr items


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
