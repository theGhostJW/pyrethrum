{-# language UndecidableInstances #-}

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
  interpret, send, localSeqUnliftIO, localSeqUnlift
 )
import Web.Api.WebDriver
import Effectful.TH (makeEffect)
import WebDriverEffect
import PyrethrumExtras (uu)
import GHC.Clock (getMonotonicTime)


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


type MyWebDriver eff a = WebDriverT (Eff eff) a


-- instance (WebUI :> es) => WebDriverT (Eff es) a where
--     randomInt = send RandomInt

{-
runWebDriverIO' :: forall es a. ( IOE :> es) => MyWebDriver (WebUI : es) a  -> MyWebDriver es a
runWebDriverIO' wd =
  interpret (\_ ->
    EF.liftIO . \case
      Click css -> findElement CssSelector css >>= elementClick
      Go url -> navigateTo url
      Sleep i -> wait i
      Read css -> findElement CssSelector css >>= getElementText) <$> wd

runWebDriverIO ::  WebDriverT IO () -> IO ()
runWebDriverIO webInteraction = do
  execWebDriverT defaultWebDriverConfig
    (runIsolated_ defaultFirefoxCapabilities webInteraction)
  pure ()

-}

data Profiling :: Effect where
  Profile :: String -> m a -> Profiling m a

type instance DispatchOf Profiling = Dynamic

profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
profile label action = send (Profile label action)


{-
runProfilingWrong :: IOE :> es => Eff (Profiling : es) a -> Eff es a
runProfilingWrong = interpret $ \_ -> \case
  Profile label action -> do
    t1 <- liftIO getMonotonicTime
    r <- action
    t2 <- liftIO getMonotonicTime
    liftIO . putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
    pure r

 • Couldn't match type ‘localEs’ with ‘es’
  Expected: Eff es a1
    Actual: Eff localEs a1   
-}

runProfiling :: IOE :> es => Eff (Profiling : es) a -> Eff es a
runProfiling = interpret $ \env -> \case
   Profile label action -> localSeqUnliftIO env $ \unlift -> do
     t1 <- getMonotonicTime
     r <- unlift action
     t2 <- getMonotonicTime
     putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
     pure r

runNoProfiling :: Eff (Profiling : es) a -> Eff es a
runNoProfiling = interpret $ \env -> \case
   Profile _label action -> localSeqUnlift env $ \unlift -> unlift action
   
action :: forall es. (Profiling :> es, IOE :> es) => Eff es ()
action = profile "greet" . liftIO $ putStrLn "Hello!"

-- $> profileAction
profileAction :: IO ()
profileAction = runEff . runProfiling $ action

{-



_test_deleteCookie_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_deleteCookie_success page =
  let
    session = do
      navigateTo $ Text.pack page
      findElement CssSelector "button#add-cookie-button" >>= elementClick
      () <- deleteCookie "fakeCookie"
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError


-}
-- https://hackage.haskell.org/package/resourcet-effectful-1.0.1.0/docs/src/Effectful.Resource.html#runResource

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

