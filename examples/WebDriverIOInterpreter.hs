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


type MyWebDriver eff a = WebDriverT (Eff eff) a


-- instance (WebUI :> es) => WebDriverT (Eff es) a where
--     randomInt = send RandomInt


runWebDriverIO' :: forall es a. ( IOE :> es) => Eff (WebUI : es) a  -> Eff es a
runWebDriverIO' =
  interpret $ \_ ->
    EF.liftIO . \case
      Click css -> uu
      Go url -> uu -- navigateTo url
      Sleep i -> uu -- wait i
      Read css -> uu -- findElement CssSelector css >>= getElementText) 




data Profiling :: Effect where
  Profile :: String -> m a -> Profiling m a

type instance DispatchOf Profiling = Dynamic

profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
profile label action = send (Profile label action)

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
