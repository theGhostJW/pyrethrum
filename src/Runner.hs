
module Runner where

import           Foundation.Extended

runTest :: Functor f =>  runConfig -> (runConfig -> item -> apEffs) -> f item -> (apEffs -> result) -> f result
runTest runConfig interactor items interpreter = interpreter . interactor runConfig <$> items
