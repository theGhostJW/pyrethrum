module WebDriverDemo where

import Data.Text.IO qualified as T
import Effectful as EF
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    runEff,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( interpret, send, localSeqUnliftIO, localSeqUnlift,
  )
import Effectful.TH (makeEffect)
import Web.Api.WebDriver
    ( WebDriverT,
      Key(EnterKey),
      fullscreenWindow,
      navigateTo,
      performActions,
      typeString,
      press,
      wait,
      execWebDriverT,
      defaultWebDriverConfig,
      runIsolated_,
      defaultFirefoxCapabilities )
import GHC.Clock (getMonotonicTime)


{-
demo the following:
  - single test suite with minimal selenium interpreter
  - read a value from "the internet"
  - navigate between pages
  - read a second value
  - validator on value
  - expect issue with laziness (if not why not)
      - solve
  - user steps
  - run with documenter
  - introduce action that uses value read from the internet
    - should blow up documenter
    - fix with doc* functions
  - TODO: Haddock docs for steps
    - effectful supports generating template haskell without type signature
    - manually add type signature and haddock
-}

-- ################### Effectful Demo ##################

theInternet :: Text
theInternet = "https://the-internet.herokuapp.com/"

{-
_endToEnd :: IO ()
_endToEnd = do
    status' <- status
    ses <- newDefaultFirefoxSession
    maximizeWindow ses
    navigateTo ses _theInternet
    link <- findElement ses _checkBoxesLinkCss
    cbTxt <- elementText ses link
    click ses link
    deleteSession ses
    T.putStrLn ""
    T.putStrLn $ "----- " <> "Results" <> " -----"
    T.putStrLn $ txt status'
    T.putStrLn cbTxt
    T.putStrLn ""
-}

type instance DispatchOf WebUI = Dynamic

data WebUI :: Effect where
  Click :: Text -> WebUI m ()
  Go :: Text -> WebUI m ()
  Read :: Text -> WebUI m Text

makeEffect ''WebUI


-- ################### WebDriver Example Using webdriver-w3c Library ##################

-- https://github.com/nbloomf/webdriver-w3c/blob/master/doc/Tutorial.md
-- https://hackage.haskell.org/package/webdriver-w3c

release_the_bats :: WebDriverT IO ()
release_the_bats = do
  fullscreenWindow
  navigateTo "https://www.google.com"
  performActions [typeString "bats"]
  performActions [press EnterKey]
  wait 5000000
  pure ()

-- >>> example1
example1 :: IO ()
example1 = do
  -- start geckodriver first
  -- geckodriver &
  execWebDriverT
    defaultWebDriverConfig
    (runIsolated_ defaultFirefoxCapabilities release_the_bats)
  pure ()


--  ################### Profiling Example ##################

-- profiling demo (from eff docs)
-- timing steps
-- inline drivers
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
   
action' :: forall es. (Profiling :> es, IOE :> es) => Eff es ()
action' = profile "greet" . liftIO $ putStrLn "Hello!"

-- $ > profileAction
profileAction :: IO ()
profileAction = runEff . runProfiling $ action'
