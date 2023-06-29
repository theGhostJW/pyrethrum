{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- TODO - Why do I to need this?
{-# LANGUAGE NoPolyKinds #-}

module DSL.Out (
  Out,
  Sink (..),
  out,
) where

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects, NoSideEffects), StaticRep, getStaticRep, unsafeEff_, unsafeLiftMapIO)
import qualified Effectful.Error.Static as E
import PyrethrumExtras (finally)
import DSL.Internal.ApEvent

{-
a very simple  logging effect initially copied from
https://hackage.haskell.org/package/effectful-core-2.2.2.2/docs/Effectful-Dispatch-Static.html
-}

data Out a :: Effect
type instance DispatchOf (Out a) = Static WithSideEffects
newtype instance StaticRep (Out a) = Out (Sink a)

newtype Sink a = Sink {sink :: a -> IO ()}

out :: (HasCallStack, Out a :> es) => a -> Eff es ()
out payload = do
  Out (Sink sink) <- getStaticRep
  unsafeEff_ . sink $ payload
