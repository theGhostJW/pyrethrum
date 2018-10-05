
module DSL.Logger where

import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad

data Logger r where
 Log :: String -> Logger ()

log :: Member Logger effs => String -> Eff effs ()
log = send . Log


logConsoleInterpreter :: forall effs a. LastMember IO effs => Eff (Logger ': effs) a -> Eff effs a
logConsoleInterpreter =  interpretM $ \case
                                         Log msg -> putStrLn msg

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
