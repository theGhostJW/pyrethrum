module DSL.Logger (
  Log,
  Logger (..),
  log,
) where

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, getStaticRep, unsafeEff_)

{- 
a very simple  logging effect initially copied from 
https://hackage.haskell.org/package/effectful-core-2.2.2.2/docs/Effectful-Dispatch-Static.html
-}

data Log :: Effect
newtype Logger = Logger {sink :: forall a. a -> IO ()}
newtype instance StaticRep Log = Log Logger

type instance DispatchOf Log = Static WithSideEffects

log :: (Log :> es) => a -> Eff es ()
log msg = do
  Log (Logger sink) <- getStaticRep
  unsafeEff_ $ sink msg