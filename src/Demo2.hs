module Demo where

class OnceParam a
class ThreadParam a

data OnceParent
instance OnceParam OnceParent
instance ThreadParam OnceParent

data ThreadParent
instance ThreadParam ThreadParent


-- -- -- -- -- -- -- -- -- -- -- -- -- -- --------------
-- -- --    Move Constraint - Impossible?  -- -- -- -- --
---------------------------------------------------------


data Fixture loc a where
  -- once hooks
  OnceBefore ::
    { onceAction :: IO a
    } ->
    Fixture OnceParent a
  OnceBefore' :: 
    { onceParent :: (OnceParam ol) => Fixture ol a
    , onceAction' :: a -> IO b
    } ->
    Fixture OnceParent b
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: IO a
    } ->
    Fixture ThreadParent a
  ThreadBefore' :: 
    { threadParent :: (ThreadParam tl) => Fixture tl a
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
  pure 4

-- no type error here but can't unpack the value, see below
addOnceIntHook :: Fixture OnceParent Int
addOnceIntHook =
  OnceBefore'
    { 
     onceParent = intThreadHook
    , onceAction' =
        \i -> pure $ i + 1
    }


getThreadChildValue :: Fixture tl a -> IO (Maybe b)
getThreadChildValue = \case 
  OnceBefore {} -> pure Nothing
  OnceBefore' {} -> pure Nothing
  ThreadBefore {} ->  pure Nothing
  ThreadBefore' threadParent  threadAction'-> do
    i <- getThreadChildValue threadParent
    traverse threadAction' i

