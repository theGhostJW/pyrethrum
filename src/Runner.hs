
module Runner (
  module InternalFuncs,
  module TestItem,
  runTest,
) where

import Check
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..))
import           TestItem

interact :: runConfig                          -- runConfig
            -> (runConfig -> item -> apEffs)  -- interactor
            -> (apEffs -> apState)            -- interpreter
            -> item                           -- test item
            -> (item, apState)
interact runConfig interactor interpreter item = (item, interpreter $ interactor runConfig item)

runInteractor :: (TestItem item valState) =>  runConfig                     -- runConfig
                                          -> (runConfig -> item -> apEffs)  -- interactor
                                          -> (apEffs -> result)            -- interpreter
                                          -> [item]                         -- test items
                                          -> Filter item                    -- item filter
                                          -> Either FilterError [(item, apState)]
runInteractor runConfig interactor interpreter items filtr =
    (interact runConfig interactor interpreter <$>) <$> filterredItems filtr items


calcValState :: (r -> a -> v)           -- prep state
             -> r                       -- runConfig
             -> (i, a)                  -- (item, apState)
             -> (i, a, v)
calcValState prepState runConfig (item, apState) = (item, apState, prepState runConfig apState)

runInteractorVs :: (TestItem item valState) =>
                                          (runConfig -> apState -> valState) -- prepSatate
                                          -> runConfig                     -- runConfig
                                          -> (runConfig -> item -> apEffs)  -- interactor
                                          -> (apEffs -> apState)            -- interpreter
                                          -> [item]                         -- test items
                                          -> Filter item                    -- item filter
                                          -> Either FilterError [(item, apState, valState)]
runInteractorVs prepState runConfig interactor interpreter items filtr =
    (calcValState prepState runConfig <$>) <$> runInteractor runConfig interactor interpreter items filtr

--validate :: (TestItem item) i => (a, valState) -> i -> (a, valState, ResultList)
validate :: TestItem i v => (i, a, v) -> (i, a, v, ResultList)
validate (i, a, valState) = (i, a, valState, calcChecks valState (validation i))

runTest :: (TestItem item valState) => (runConfig -> apState -> valState) -- prepSatate
                                    -> runConfig                          -- runConfig
                                    -> (runConfig -> item -> apEffs)      -- interactor
                                    -> (apEffs -> apState)                -- interpreter
                                    -> [item]                             -- test items
                                    -> Filter item                        -- item filter
                                    -> Either FilterError [(item, apState, valState, ResultList)]
runTest prepState runConfig interactor interpreter items filtr =
    (validate <$>) <$> runInteractorVs prepState runConfig interactor interpreter items filtr


-- runTest :: (TestItem item valState) =>  (a -> v -> c)                     -- prepStateToTransformer
--                                         -> runConfig                       -- runConfig
--                                         -> (runConfig -> a -> v)           -- prepState
--                                         -> (runConfig -> item -> apEffs)   -- interactor
--                                         -> [item]                          -- test items
--                                         -> ((a -> c) -> apEffs -> result)  -- interpreter
--                                         -> Filter item                     -- item filter
--                                         -> Either FilterError [result]
-- runTest prepstateToTransformer runConfig prepState interactor items interpreter filtr =
--     let
--       a2c a = prepstateToTransformer a (prepState runConfig a)
--       i2rslt = interpreter a2c . interactor runConfig
--     in
--       (i2rslt <$>) <$> filterredItems filtr items
