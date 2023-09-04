module Demo where
import Data.Foldable (traverse_)

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
    { onceParent = intOnceHook
    , onceAction' =
        \i -> pure $ i + 1
    }


-- getThreadChildValue :: Fixture tl a -> IO (Maybe b)
-- getThreadChildValue = \case 
--   OnceBefore {} -> pure Nothing
--   OnceBefore' {} -> pure Nothing
--   ThreadBefore {} ->  pure Nothing
--   ThreadBefore' threadParent  threadAction'-> do
--     i <- getThreadChildValue threadParent
--     traverse threadAction' i



data DemoData a where 
  DemoData ::
   { demoField :: Show b => b
   } -> DemoData a

-- showb :: DemoData Int -> String
-- showb DemoData {demoField} = show demoField

showb2 :: Int -> String
showb2 i = 
  showb demoData
  where 
    demoData = DemoData {demoField = i}
    showb DemoData {demoField} = show demoField

data DemoDataFixed a where 
  DemoDataFixed :: Show b =>
   { demoField ::  b
   } -> DemoDataFixed a

showFixedb :: DemoDataFixed Int -> String
showFixedb DemoDataFixed {demoField} = show demoField

