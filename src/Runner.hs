
module Runner where

import           AppError
import qualified Data.List.Safe      as SafeList
import           Foundation.Extended

data Filter a = IID Int |
                All |
                Last |
                LastVal |
                Pred (a -> Bool)

runTest :: (TestItem item) => runConfig
                                      -> (runConfig -> item -> apEffs) -- interactor
                                      -> [item]                        -- test items
                                      -> Filter item                       -- item filter
                                      -> (apEffs -> result)            -- interpreter
                                      -> Either AppError [result]
runTest runConfig interactor items filtr interpreter = let
                                                         listOrFail lst msg = null lst
                                                                                    ? Left (InvalidItemFilter msg)
                                                                                    $ Right lst
                                                         filterredItems = case filtr of
                                                                            IID iid -> listOrFail (filter (\i -> identifier i == iid) items) $ "id: " <> show iid <> " not in item list"
                                                                            All -> Right items
                                                                            Last -> maybe (Left $ InvalidItemFilter "items list is empty") (Right . pure) (SafeList.last items)
                                                                            LastVal -> Left $ NotImplemented "LastVal handler not implemented in runTest"
                                                                            Pred func -> listOrFail (filter func items) "No test items match filter function"
                                                       in
                                                         (interpreter . interactor runConfig <$>) <$> filterredItems

class TestItem a where
  identifier :: a -> Int
  whenClause :: a -> String
  thenClause :: a -> String
  whenThen :: a -> String
  whenThen a = "When: " <> whenClause a  <> "\n" <>
               "Then: " <> thenClause a  <> "\n"
