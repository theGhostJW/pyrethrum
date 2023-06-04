module DSL.ArbitraryIO where

import qualified Data.Aeson as A
import Common
import DSL.LogProtocol
import DSL.LoggerPsy
import           Control.Exception as E
import Polysemy
import Polysemy.Error as PE


data ArbitraryIO m a where
  ArbitraryIO :: Text -> b -> IO b -> ArbitraryIO m b

makeSem ''ArbitraryIO

arbitraryIODocInterpreter :: forall effs a e. (Show e, A.ToJSON e, Member (Logger e) effs) => Sem (ArbitraryIO ': effs) a -> Sem effs a
arbitraryIODocInterpreter = interpret $ \(ArbitraryIO msg def _) -> logItem (IOAction msg) $> def

arbitraryIOInterpreter :: forall effs a e. (Show e, A.ToJSON e, Members '[Error (FrameworkError e), Logger e, Embed IO] effs) => Sem (ArbitraryIO ': effs) a -> Sem effs a
arbitraryIOInterpreter =
  let
    handleException :: forall b. Text -> IO b -> Sem effs b
    handleException msg action = do
                                r <- embed (E.try action)
                                case r of
                                  Left (e :: IOException) -> PE.throw (IOError' ("Exception raised when executing arbitrary IO action with message: " <> msg) e)
                                  Right f -> pure f
  in
    interpret $ \(ArbitraryIO msg _ actn) -> logItem (IOAction msg) *> handleException msg actn

