
module Runner where

import           Foundation.Extended

data Filter a = IID Int |
                All |
                Last |
                LastVal |
                Pred (a -> Bool)

runTest :: Functor f => runConfig
                     -> (runConfig -> item -> apEffs) -- interactor
                     -> f item                        -- test items
                     -> (apEffs -> result)            -- interpreter
                     -> f result
runTest runConfig interactor items interpreter = interpreter . interactor runConfig <$> items

class TestItem a where
  identifier :: a -> Int
  whenClause :: a -> String
  thenClause :: a -> String
  whenThen :: a -> String
  whenThen a = "When: " <> whenClause a  <> "\n" <>
               "Then: " <> thenClause a  <> "\n"
