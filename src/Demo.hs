module Demo where

class OnceParam a
class ThreadParam a

data OnceParent
instance OnceParam OnceParent
instance ThreadParam OnceParent

data ThreadParent
instance ThreadParam ThreadParent


data Fixture loc a where
  -- once hooks
  OnceBefore ::
    { onceAction :: (Monad m) => m a
    } ->
    Fixture OnceParent a
  OnceBefore' :: 
    { onceParent :: forall l. (OnceParam l) => Fixture l a
    , onceAction' :: (Monad m) => a -> m b
    } ->
    Fixture OnceParent b
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: (Monad m) => m a
    } ->
    Fixture ThreadParent a
  ThreadBefore' ::
    { threadParent ::  forall tl. (ThreadParam tl) => Fixture tl a
    , threadAction' :: (Monad m) => a -> m b
    } ->
    Fixture ThreadParent b


intOnceHook :: Fixture OnceParent Int
intOnceHook =
  OnceBefore
    { onceAction = pure 1
    }

intThreadHook :: Fixture ThreadParent Int
intThreadHook = ThreadBefore $ do
  pure 42

addOnceIntHook :: Fixture OnceParent Int
addOnceIntHook =
  OnceBefore'
    { 
      -- why does this compile? intThreadHook  does not have a OnceParam instance
      onceParent = intOnceHook
    , onceAction' =
        \i -> pure $ i + 1
    }
