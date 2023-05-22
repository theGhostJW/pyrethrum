module DSL.FileSystem.EffectDynamic
  ( -- * Effect
  --   FileSystem

  --   -- ** Handlers
  -- , runFileSystem
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

{-
-- | An effect for interacting with the filesystem.
data FileSystem :: Effect where
  EnsureDir :: Path b Dir -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic


type instance DispatchOf (FileSystem r) = Dynamic

-- | Run the 'FileSystem' effect.
runFileSystem :: IOE :> es => Eff (FileSystem : es) a -> Eff es a
runFileSystem = evalStaticRep FileSystem






data Reader r :: Effect where
  Ask   :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

type instance DispatchOf (Reader r) = Dynamic

-- | Run the 'Reader' effect with the given initial environment (via
-- "Effectful.Reader.Static").
runReader
  :: r -- ^ The initial environment.
  -> Eff (Reader r : es) a
  -> Eff es a
runReader r = reinterpret (R.runReader r) $ \env -> \case
  Ask       -> R.ask
  Local f m -> localSeqUnlift env $ \unlift -> R.local f (unlift m)

-}
