-- TODO - Why do I to need this?
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.Out (
  Out,
  Sink (..),
  out,
) where

import DSL.Internal.ApEvent
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (NoSideEffects, WithSideEffects), StaticRep, evalStaticRep, getStaticRep, unsafeEff_, unsafeLiftMapIO)
import qualified Effectful.Error.Static as E
import PyrethrumExtras (finally)

{-
a very simple  logging effect initially copied from
https://hackage.haskell.org/package/effectful-core-2.2.2.2/docs/Effectful-Dispatch-Static.html
-}

data Out a :: Effect
type instance DispatchOf (Out a) = Static WithSideEffects
newtype instance StaticRep (Out a) = Out (Sink a)

newtype Sink a = Sink {sink :: a -> IO ()}

out :: (Out a :> es) => a -> Eff es ()
out payload = do
  Out (Sink sink) <- getStaticRep
  unsafeEff_ . sink $ payload

runOut :: (IOE :> es) => (a -> IO ()) -> Eff (Out a : es) a -> Eff es a
runOut = evalStaticRep . Out . Sink
