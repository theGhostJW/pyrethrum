
module DSL.Logger where

import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad
import qualified Prelude as P

data Logger r where
 Log :: Show s => s -> Logger ()

log :: (Show s, Member Logger effs) => s -> Eff effs ()
log = send . Log

logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) a -> Eff effs a
logConsoleInterpreter =  interpretM $ \case
                                         Log msg -> P.print msg

-- -- interactor :: Effects effs => (ItemClass Item ValState) => RunConfig -> Item -> Eff effs ApState
-- runConsole :: Eff '[Logger, IO] a -> IO a
-- runConsole = runM . interpretM (\case
--   PutStrLn msg -> putStrLn msg
--   GetLine -> getLine
--   ExitSuccess -> exitSuccess)

applyLogger :: (Show s) => (Eff '[Logger, IO] () -> IO ()) -> s -> IO ()
applyLogger interpreter = interpreter . log

consoleLogger :: (Show s) => s -> IO ()
consoleLogger = applyLogger (runM . logConsoleInterpreter)

{-
data MyData1 = MyData1 {name :: String, num :: Int}
data MyData2 = MyData2 {name :: String, num2 :: Int}

md2Name :: MyData2 -> String
md2Name = name

mixNames :: MyData1 -> MyData2 -> String
mixNames MyData1{..} md2 = let
                             name2 = md2Name md2
                           in
                             name <> " " <> name2
-}
