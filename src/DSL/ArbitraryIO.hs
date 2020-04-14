module DSL.ArbitraryIO where

import Common
import DSL.LogProtocol
import DSL.Logger
import           Pyrelude
-- import           Control.Monad.Freer
-- import           Control.Monad.Freer.Error
import           Control.Exception as E
import Polysemy
import Polysemy.Error as PE


data ArbitraryIO m a where
  ArbitraryIO :: Text -> b -> IO b -> ArbitraryIO m b

makeSem ''ArbitraryIO

arbitraryIODocInterpreter :: forall effs a. Member Logger effs => Sem (ArbitraryIO ': effs) a -> Sem effs a
arbitraryIODocInterpreter = interpret $ \(ArbitraryIO msg def _) -> logItem (IterationLog . Doc $ DocIOAction msg) $> def

arbitraryIOInterpreter :: forall effs a. Members '[Error AppError, Logger, Embed IO] effs => Sem (ArbitraryIO ': effs) a -> Sem effs a
arbitraryIOInterpreter =
  let
    handleException :: forall b. Text -> IO b -> Sem effs b
    handleException msg action = do
                                r <- embed (E.try action)
                                case r of
                                  Left (e :: IOException) -> PE.throw (IOError' ("Exception raised when executing arbituary IO action with message: " <> msg) e)
                                  Right f -> pure f
  in
    interpret $ \(ArbitraryIO msg _ actn) -> logItem (IterationLog . Run $ IOAction msg) *> handleException msg actn

