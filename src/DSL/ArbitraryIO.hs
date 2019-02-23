
module DSL.ArbitraryIO where

import Common
import DSL.LogProtocol
import DSL.Logger
import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad
import           Control.Exception as E
import           Data.Functor

data ArbitraryIO r where
  ArbitraryIO :: forall a. String -> a -> IO a -> ArbitraryIO a

arbitraryIO :: forall effs a. Member ArbitraryIO effs => String -> a -> IO a -> Eff effs a
arbitraryIO msg def action = send $ ArbitraryIO msg def action

arbitraryIODocInterpreter :: forall effs a. Member Logger effs => Eff (ArbitraryIO ': effs) a -> Eff effs a
arbitraryIODocInterpreter = interpret $ \(ArbitraryIO msg def _) -> logItem (IOAction msg) $> def

arbitraryIOIOInterpreter :: Members '[Error AppError, Logger, IO] effs => Eff (ArbitraryIO ': effs) a -> Eff effs a
arbitraryIOIOInterpreter =
                          let
                            handleException msg action = do
                                                       r <- send (E.try action)
                                                       case r of
                                                         Left (e :: IOException) -> throwError (AppIOError' ("Exception raised when executing arbituary IO action with message: " <> msg) e)
                                                         Right f -> pure f
                           in
                            interpret $ \(ArbitraryIO msg _ actn) -> logItem (IOAction msg) *> handleException msg actn
