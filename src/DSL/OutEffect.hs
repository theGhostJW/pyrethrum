-- TODO - Why do I to need this?
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.OutEffect where

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (StaticRep, SideEffects(..), getStaticRep, unsafeEff_, evalStaticRep)

{-
a very simple  logging effect initially copied from
https://hackage.haskell.org/package/effectful-core-2.2.2.2/docs/Effectful-Dispatch-Static.html
inspired by polysemy's Out effect
-}

data Out a :: Effect
type instance DispatchOf (Out a) = Static WithSideEffects
newtype instance StaticRep (Out a) = Out (Sink a)

newtype Sink a = Sink {sink :: a -> IO ()}

out :: (Out a :> es) => a -> Eff es ()
out payload = do
  Out s <- getStaticRep
  unsafeEff_ . s.sink $ payload