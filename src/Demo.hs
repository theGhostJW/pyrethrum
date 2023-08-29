module Demo where

class OnceParam a
class ThreadParam a

data OnceParent
instance OnceParam OnceParent
instance ThreadParam OnceParent

data ThreadParent
instance ThreadParam ThreadParent

--  correct solution

data Fixture loc a where
  -- once hooks
  OnceBefore ::
    { onceAction :: IO a
    } ->
    Fixture OnceParent a
  OnceBefore' :: forall a b ol. (OnceParam ol) =>
    { onceParent :: Fixture ol a
    , onceAction' :: a -> IO b
    } ->
    Fixture OnceParent b
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: IO a
    } ->
    Fixture ThreadParent a
  ThreadBefore' :: forall a b tl. (ThreadParam tl) =>
    { threadParent :: Fixture tl a
    , threadAction' :: a -> IO b
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
      onceParent = intOnceHook
    , onceAction' =
        \i -> pure $ i + 1
    }