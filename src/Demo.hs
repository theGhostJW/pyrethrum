module Demo where

class OnceParam a
class ThreadParam a
class EachParam a

class OnceAfterParam a
class ThreadAfterParam a
class EachAfterParam a

data OnceParent
instance OnceParam OnceParent
instance ThreadParam OnceParent
instance EachParam OnceParent

data ThreadParent
instance ThreadParam ThreadParent
instance EachParam ThreadParent

data EachParent
instance EachParam EachParent

data Fixture loc a where
  -- once hooks
  OnceBefore ::
    { onceAction :: (Monad m) => m a
    } ->
    Fixture OnceParent a
  OnceBefore' ::
    { onceParent :: (OnceParam loc) => Fixture loc a
    , onceAction' :: (Monad m) => a -> m b
    } ->
    Fixture OnceParent b
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: (Monad m) => m a
    } ->
    Fixture ThreadParent a
  ThreadBefore' ::
    { threadParent :: (ThreadParam loc) => Fixture loc a
    , threadAction' :: (Monad m) => a -> m b
    } ->
    Fixture ThreadParent b

intOnceHook :: Fixture OnceParent Int
intOnceHook =
  OnceBefore
    { onceAction = pure 1
    }

--  this should not compile
addOnceIntHook :: Fixture OnceParent Int
addOnceIntHook =
  OnceBefore'
    { onceParent = intThreadHook
    -- onceParent = onceThreadHook,
    , onceAction' =
        \i -> do
          pure $ i + 1
    }

intThreadHook :: Fixture ThreadParent Int
intThreadHook = ThreadBefore $ do
  pure 42
