module WebDriverDemo where

import Data.Aeson
import Data.Text.IO qualified as T
import Debug.Trace.Extended (uu)
import Effectful as EF
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    liftIO,
    runEff,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( interpret,
  )
import Effectful.Reader.Dynamic
import Effectful.TH (makeEffect)
import Network.HTTP.Client qualified as L
import Network.HTTP.Req as R
import Network.HTTP.Types qualified as L
import PyrethrumExtras (getLenient, toS, txt)
import Web.Api.WebDriver

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

release_the_bats :: WebDriverT IO ()
release_the_bats = do
  fullscreenWindow
  navigateTo "https://www.google.com"
  performActions [typeString "bats"]
  performActions [press EnterKey]
  wait 5000000
  pure ()

-- $ > example1

-- >>> example1
example1 :: IO ()
example1 = do
  -- start geckodriver first
  -- geckodriver &
  execWebDriverT
    defaultWebDriverConfig
    (runIsolated_ defaultFirefoxCapabilities release_the_bats)
  pure ()

{-
-- >>> helloWorld
-- "DONE"
helloWorld :: IO Text
helloWorld = do
  putStrLn "What is your name?"
  name <- T.getLine
  putStrLn $ "Hello, " <> toS name <> "!"
  pure "DONE"

-- >>> helloWorldNonInteractive
-- "DONE"
helloWorldNonInteractive :: IO Text
helloWorldNonInteractive = putStrLn "Hello, World!" >> pure "DONE"
-}

{-
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

gheckoUrl :: R.Url 'Http
gheckoUrl = http "127.0.0.1" /: "status" 

driver1 :: Text -> IO WebDriverDemo.HttpResponse
driver1 = driver . pure

driver2 :: Text -> Text -> IO WebDriverDemo.HttpResponse
driver2 s1 s2 = driver [s1, s2]

driver :: [Text] -> IO WebDriverDemo.HttpResponse
driver subDirs = runReq defaultHttpConfig $ do
  r <- req GET url NoReqBody jsonResponse $ port 4444
  liftIO . pure $
    MkHttpResponse
      { statusCode = responseStatusCode r,
        statusMessage = getLenient . toS $ responseStatusMessage r,
        headers = L.responseHeaders . toVanillaResponse $ r,
        body = responseBody r :: Value,
        cookies = responseCookieJar r
      }
  where
    url :: R.Url 'Http
    url =  foldl' (/:) (http "127.0.0.1") subDirs

{-
responseBody :: HttpResponse response => response -> HttpResponseBody response

Get the response body.

responseStatusCode :: HttpResponse response => response -> Int

Get the response status code.

responseStatusMessage :: HttpResponse response => response -> ByteString

Get the response status message.

responseHeader
:: HttpResponse response
=> response

Response interpretation
-> ByteString

Header to lookup
-> Maybe ByteString

Header value if found

Lookup a particular header from a response.

responseCookieJar :: HttpResponse response => response -> CookieJar

-}

data HttpResponse = MkHttpResponse
  { statusCode :: Int,
    statusMessage :: Text,
    headers :: L.ResponseHeaders,
    body :: Value,
    cookies :: L.CookieJar
  }
  deriving (Show)

-- use req to get status of geckodriver

-- $ > example2

-- >>> example2
example2 :: IO () 
example2 = driver1 "status" >>= putStrLn . toS . txt 

-- pure $ (responseBody r :: Value)

-- curl -I http://127.0.0.1:4444/status
