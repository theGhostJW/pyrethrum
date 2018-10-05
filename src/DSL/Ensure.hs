
module DSL.Ensure where

import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad

data Ensure r where
 Ensure :: Truthy conditon => String -> conditon -> Ensure ()
 Throw :: String -> Ensure ()

newtype EnsureError = EnsureError String deriving (Show, Eq)

ensure :: Member Ensure effs => String -> Bool -> Eff effs ()
ensure err condition = send $ Ensure err condition

throw :: Member Ensure effs => String -> Eff effs ()
throw = send . Throw

ensureInterpreter :: forall effs a. Member (Error EnsureError) effs => Eff (Ensure ': effs) a -> Eff effs a
ensureInterpreter = interpret $ \case
                                    Ensure message condition -> Monad.unless (isTruthy condition) $ throwError $ EnsureError message
                                    Throw message -> throwError $ EnsureError message

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
