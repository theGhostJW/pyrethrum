{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.Out (
  Out,
  Sink (..),
  out,
) where

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, getStaticRep, unsafeEff_, unsafeLiftMapIO)
import qualified Effectful.Error.Static as E
import PyrethrumExtras (finally)
import DSL.Internal.ApEvent

{-
a very simple  logging effect initially copied from
https://hackage.haskell.org/package/effectful-core-2.2.2.2/docs/Effectful-Dispatch-Static.html
-}

data Out a :: Effect
newtype Sink = Sink {sink :: forall a. a -> IO ()}
newtype instance StaticRep (Out a) = Out Sink

type instance DispatchOf (Out a) = Static WithSideEffects

out :: (HasCallStack, Out a :> es) => a -> Eff es ()
out payload = do
  Out (Sink sink) <- getStaticRep
  unsafeEff_ . sink $ payload

-- folder :: (Out a :> es) => Text -> Eff es () -> Eff es ()
-- folder fldrName action = do
--   Out (Sink sink) <- getStaticRep
--   finally
--     ( do
--         unsafeEff_ . sink $ StartFolder fldrName
--         action
--     )
--     (unsafeEff_ $ sink EndFolder) 