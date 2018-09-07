
module Runner (
  module InternalFuncs,
  runTest,
) where

import           AppError
import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       TestItem (..))

runTest :: (TestItem item) => runConfig
                                      -> (runConfig -> item -> apEffs) -- interactor
                                      -> [item]                        -- test items
                                      -> (apEffs -> result)            -- interpreter
                                      -> Filter item                   -- item filter
                                      -> Either AppError [result]
runTest runConfig interactor items interpreter filtr = (interpreter . interactor runConfig <$>) <$> filterredItems filtr items
