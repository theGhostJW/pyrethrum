{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.Logger (
  Log,
  Logger (..),
  log,
) where

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, getStaticRep, unsafeEff_, unsafeLiftMapIO)
import qualified Effectful.Error.Static as E
import PyrethrumExtras (finally)

{-
a very simple  logging effect initially copied from
https://hackage.haskell.org/package/effectful-core-2.2.2.2/docs/Effectful-Dispatch-Static.html
-}

data Log a :: Effect
newtype Logger = Logger {sink :: forall a. LogAction a -> IO ()}
newtype instance StaticRep (Log a) = Log Logger

type instance DispatchOf (Log a) = Static WithSideEffects

data LogAction a where
  LogMessage :: a -> LogAction ()
  LogMessage' :: a -> Text -> LogAction ()
  LogWarning :: a -> LogAction ()
  LogWarning' :: a -> Text -> LogAction ()
  LogError :: a -> LogAction ()
  LogError' :: a -> Text -> LogAction ()
  StartFolder :: Text -> LogAction ()
  EndFolder :: LogAction ()

log :: (HasCallStack, Log a :> es) => a -> Eff es ()
log msg = do
  Log (Logger sink) <- getStaticRep
  unsafeEff_ . sink $ LogMessage msg

logMessage' :: (HasCallStack, Log a :> es) => a -> Text -> Eff es ()
logMessage' msg txt = do
  Log (Logger sink) <- getStaticRep
  unsafeEff_ . sink $ LogMessage' msg txt

logWarning :: (HasCallStack, Log a :> es) => a -> Eff es ()
logWarning msg = do
  Log (Logger sink) <- getStaticRep
  unsafeEff_ . sink $ LogWarning msg

logWarning' :: (HasCallStack, Log a :> es) => a -> Text -> Eff es ()
logWarning' msg txt = do
  Log (Logger sink) <- getStaticRep
  unsafeEff_ . sink $ LogWarning' msg txt

logError :: (HasCallStack, Log a :> es) => a -> Eff es ()
logError msg = do
  Log (Logger sink) <- getStaticRep
  unsafeEff_ . sink $ LogError msg

logError' :: (HasCallStack, Log a :> es) => a -> Text -> Eff es ()
logError' msg txt = do
  Log (Logger sink) <- getStaticRep
  unsafeEff_ . sink $ LogError' msg txt

folder :: (Log a :> es) => Text -> Eff es () -> Eff es ()
folder fldrName action = do
  Log (Logger sink) <- getStaticRep
  finally
    ( do
        unsafeEff_ . sink $ StartFolder fldrName
        action
    )
    (unsafeEff_ $ sink EndFolder)