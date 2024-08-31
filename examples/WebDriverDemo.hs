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
1. revisit monad transformers step by step DONE
2. relook at HttpTT
3. relook at Eff
  - THIS !!! :: https://hackage.haskell.org/package/effectful-core-2.3.0.1/docs/Effectful-Dispatch-Dynamic.html#g:4
  - esp lifting
4. try find examples

Control.Monad.Script.Http

A basic type and monad transformer transformer for describing HTTP interactions.

data HttpTT e r w s p t eff a

 demo the following:
  - single test suite with minimal selenium interpreter
    - when web ui interaction is implemented it will probably use a custom / different library due to licence
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

theInternet :: Text
theInternet = "https://the-internet.herokuapp.com/"

-- https://github.com/nbloomf/webdriver-w3c/blob/master/doc/Tutorial.md
-- https://hackage.haskell.org/package/webdriver-w3c

type instance DispatchOf WebUI = Dynamic

data WebUI :: Effect where
  Click :: Text -> WebUI m ()
  Go :: Text -> WebUI m ()
  Read :: Text -> WebUI m Text

makeEffect ''WebUI


-- ##############  Orthogonal Examples ##################
-- | A simple example of using the WebDriver API from the 
-- | from the webdriver-w3c library
release_the_bats :: WebDriverT IO ()
release_the_bats = do
  fullscreenWindow
  navigateTo "https://www.google.com"
  performActions [typeString "bats"]
  performActions [press EnterKey]
  wait 5000000
  pure ()

-- $> example1

-- >>> example1
example1 :: IO ()
example1 = do
  -- start geckodriver first
  -- geckodriver &
  execWebDriverT
    defaultWebDriverConfig
    (runIsolated_ defaultFirefoxCapabilities release_the_bats)
  pure ()

{- from webdriver-w3c 
Options:
      --allow-hosts <ALLOW_HOSTS>...
          List of hostnames to allow. By default the value of --host is allowed,
          and in addition if that's a well known local address, other variations
          on well known local addresses are allowed. If --allow-hosts is
          provided only exactly those hosts are allowed.
      --allow-origins <ALLOW_ORIGINS>...
          List of request origins to allow. These must be formatted as
          scheme://host:port. By default any request with an origin header is
          rejected. If --allow-origins is provided then only exactly those
          origins are allowed.
      --android-storage <ANDROID_STORAGE>
          Selects storage location to be used for test data (deprecated).
          [possible values: auto, app, internal, sdcard]
  -b, --binary <BINARY>
          Path to the Firefox binary
      --connect-existing
          Connect to an existing Firefox instance
      --enable-crash-reporter
          Enable the Firefox crash reporter for diagnostic purposes
  -h, --help
          Prints this message
      --host <HOST>
          Host IP to use for WebDriver server [default: 127.0.0.1]
      --jsdebugger
          Attach browser toolbox debugger for Firefox
      --log <LEVEL>
          Set Gecko log level [possible values: fatal, error, warn, info,
          config, debug, trace]
      --log-no-truncate
          Disable truncation of long log lines
      --marionette-host <HOST>
          Host to use to connect to Gecko [default: 127.0.0.1]
      --marionette-port <PORT>
          Port to use to connect to Gecko [default: system-allocated port]
  -p, --port <PORT>
          Port to use for WebDriver server [default: 4444]
      --profile-root <PROFILE_ROOT>
          Directory in which to create profiles. Defaults to the system
          temporary directory.
  -v...
          Log level verbosity (-v for debug and -vv for trace level)
  -V, --version
          Prints version and copying information
      --websocket-port <PORT>
          Port to use to connect to WebDriver BiDi [default: 9222]

-}


-- 
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
