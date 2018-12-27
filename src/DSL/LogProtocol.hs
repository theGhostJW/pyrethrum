
module DSL.LogProtocol where

import           DSL.Common
import           Foundation.Extended
import           TestAndRunConfig

data LogProtocol a where
  Message :: String -> LogProtocol String
  Message' :: DetailedInfo -> LogProtocol DetailedInfo

  Warning :: String -> LogProtocol String
  Warning' :: DetailedInfo -> LogProtocol DetailedInfo

  IOAction :: String -> LogProtocol String

  Error :: AppError -> LogProtocol AppError
  FilterLog :: (Show tc, Eq tc, TestConfigClass tc) => [Either (FilterRejection tc) tc] -> LogProtocol tc

  StartRun :: forall rc. (Show rc, Eq rc, Titled rc) => rc -> LogProtocol rc
  StartGroup :: String -> LogProtocol String
  StartTest :: forall tc. (Show tc, Eq tc, TestConfigClass tc) => tc -> LogProtocol tc
  StartIteration :: String -> Int -> LogProtocol (String, Int) -- iid / test module
  EndIteration :: String -> Int -> String -> LogProtocol (String, Int, String) -- test module / test Info
  EndRun :: forall rc. (Show rc, Eq rc, Titled rc) => rc -> LogProtocol rc

deriving instance Show (LogProtocol a)
deriving instance Eq (LogProtocol a)
