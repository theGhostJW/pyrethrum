module WebDriverIOInterpreter where

  
import Data.Text.IO qualified as T
import Effectful as EF (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  liftIO,
  runEff,
  type (:>),
 )
import Effectful.Reader.Dynamic
import Effectful.Dispatch.Dynamic (
  interpret
 )
import Effectful.TH (makeEffect)
import WebDriverEffect

runWebDriverIO :: forall es a. ( IOE :> es) => Eff (WebUI : es) a -> Eff es a
runWebDriverIO =
  interpret $ \_ ->
    EF.liftIO . \case
      Click name -> T.putStrLn $ "Click " <> name
      Go name -> T.putStrLn $ "Go " <> name
      Read name -> undefined -- T.putStrLn $ "Read " <> name



-- Interpreters

-- runWebUI :: forall es a. ( IOE :> es) => Eff (WebUI : es) a -> Eff es a
-- runWebUI =
--   interpret $ \_ ->
--     EF.liftIO . \case
--       Hello name -> T.putStrLn $ "Hello " <> name
--       Goodbye name -> T.putStrLn $ "Goodbye " <> name

-- runWebUICasual :: forall es a. ( IOE :> es) =>Eff (WebUI : es) a -> Eff es a
-- runWebUICasual =
--   interpret $ \_ ->
--     EF.liftIO . \case
--       Hello name -> T.putStrLn $ "hi " <> name
--       Goodbye name -> T.putStrLn $ "bye " <> name


-- https://hackage.haskell.org/package/effectful-core-2.2.2.2/docs/Effectful-Dispatch-Dynamic.html#g:9
-- $integration
--
-- #integration#
--
-- There exists a lot of libraries that provide their functionality as an @mtl@
-- style effect, which generally speaking is a type class that contains core
-- operations of the library in question.
--
-- Such effects are quite easy to use with the 'Eff' monad. As an example,
-- consider the @mtl@ style effect for generation of random numbers:
--
-- >>> :{
--   class Monad m => MonadRNG m where
--     randomInt :: m Int
-- :}
--
-- Let's say the library also defines a helper function for generation of random
-- strings:
--
-- >>> import Control.Monad
-- >>> import Data.Char
--
-- >>> :{
--  randomString :: MonadRNG m => Int -> m String
--  randomString n = map chr <$> replicateM n randomInt
-- :}
--
-- To make it possible to use it with the 'Eff' monad, the first step is to
-- create an effect with operations that mirror the ones of a type class:
--
-- >>> :{
--   data RNG :: Effect where
--     RandomInt :: RNG m Int
-- :}
--
-- >>> type instance DispatchOf RNG = Dynamic
--
-- If we continued as in the example above, we'd now create top level helper
-- functions that execute effect operations using 'send', in this case
-- @randomInt@ tied to @RandomInt@. But this function is already declared by the
-- @MonadRNG@ type class! Therefore, what we do instead is provide an
-- __orphan__, __canonical__ instance of @MonadRNG@ for 'Eff' that delegates to
-- the @RNG@ effect:
--
-- >>> :set -XUndecidableInstances
--
-- >>> :{
--   instance RNG :> es => MonadRNG (Eff es) where
--     randomInt = send RandomInt
-- :}
--
-- Now we only need an interpreter:
--
-- >>> :{
--   runDummyRNG :: Eff (RNG : es) a -> Eff es a
--   runDummyRNG = interpret $ \_ -> \case
--     RandomInt -> pure 55
-- :}
--
-- and we can use any function that requires a @MonadRNG@ constraint with the
-- 'Eff' monad as long as the @RNG@ effect is in place:
--
-- >>> runEff . runDummyRNG $ randomString 3
-- "777"
--