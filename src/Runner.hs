
module Runner (
  module InternalFuncs,
  module TestItem,
--  runFullTest,
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
            -> apState
interact runConfig interactor interpreter item = interpreter $ interactor runConfig item

runInteractor :: (TestItem item valState) =>  runConfig                     -- runConfig
                                          -> (runConfig -> item -> apEffs)  -- interactor
                                          -> (apEffs -> apState)            -- interpreter
                                          -> [item]                         -- test items
                                          -> Filter item                    -- item filter
                                          -> Either FilterError [apState]
runInteractor runConfig interactor interpreter items filtr =
    (interact runConfig interactor interpreter <$>) <$> filterredItems filtr items


calcValState :: r                       -- runConfig
             -> (r -> a -> v)           -- prep state
             -> a                       -- app states
             -> (a, v)
calcValState runConfig prepState apState = (apState, prepState runConfig apState)

--validate :: (TestItem item) i => (a, valState) -> i -> (a, valState, ResultList)
--validate :: TestItem a v => (a1, b) -> a -> (a1, b) -> (a1, b, ResultList)
validate (a, valState) item = (a, valState, calcChecks valState (validation item) )

-- calcValState :: r                           -- runConfig
--              -> (r -> a -> v)            -- prep state
--              -> Either FilterError [a]   -- app states
--              -> Either FilterError [(a, v)]
-- calcValState runConfig prepState apStates =
--     let
--       toValState a = (a, prepState runConfig a)
--     in
--       (toValState <$>) <$> apStates
