{-# LANGUAGE UndecidableInstances #-}

module WebDriverIO
  ( status,
    maximizeWindow,
    minimizeWindow,
    fullscreenWindow,
    newSession,
    newDefaultFirefoxSession,
    deleteSession,
    navigateTo,
    findElement,
    click,
    elementText,
  )
where

-- import Effectful.Reader.Dynamic

import Data.Aeson (Value, object)
import Data.Text.IO qualified as T
import Network.HTTP.Client qualified as L
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    HttpException,
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Scheme (Http),
    Url,
    defaultHttpConfig,
    http,
    jsonResponse,
    port,
    req,
    responseBody,
    responseCookieJar,
    responseStatusCode,
    responseStatusMessage,
    runReq,
    toVanillaResponse,
    (/:),
  )
import PyrethrumExtras (getLenient, toS, txt)
import UnliftIO (try)
import Web.Api.WebDriver (Capabilities, defaultFirefoxCapabilities)
import WebDriverPure (RequestArgs (..), capsToJson)
import WebDriverSpec as WD
  ( DriverStatus,
    ElementRef,
    HttpResponse (..),
    Selector,
    SessionRef,
    W3Spec (..),
    clickSpec,
    deleteSessionSpec,
    elementTextSpec,
    findElementSpec,
    fullscreenWindowSpec,
    maximizeWindowSpec,
    minimizeWindowSpec,
    mkShowable,
    navigateToSpec,
    newSessionSpec',
    statusSpec,
  )
import Prelude hiding (get, second)

-- ############# IO Implementation #############

status :: IO DriverStatus
status = run statusSpec

maximizeWindow :: SessionRef -> IO ()
maximizeWindow = run . maximizeWindowSpec

minimizeWindow :: SessionRef -> IO ()
minimizeWindow = run . minimizeWindowSpec

fullscreenWindow :: SessionRef -> IO ()
fullscreenWindow = run . fullscreenWindowSpec

newSession :: Capabilities -> IO SessionRef
newSession = run . newSessionSpec' . capsToJson

newDefaultFirefoxSession :: IO SessionRef
newDefaultFirefoxSession = newSession defaultFirefoxCapabilities

deleteSession :: SessionRef -> IO ()
deleteSession = run . deleteSessionSpec

navigateTo :: SessionRef -> Text -> IO ()
navigateTo s = run . navigateToSpec s

findElement :: SessionRef -> Selector -> IO ElementRef
findElement s = run . findElementSpec s

click :: SessionRef -> ElementRef -> IO ()
click s = run . clickSpec s

elementText :: SessionRef -> ElementRef -> IO Text
elementText s = run . elementTextSpec s

-- ############# Utils #############

--------------------------------------------------------------------------------
-- console out (to haskell output window) for debugging
run :: forall a. (Show a) => W3Spec a -> IO a
run = execute'

-- | Execute with logging
execute' :: forall a. (Show a) => W3Spec a -> IO a
execute' spec =
  describe spec.description $ do
    devLog . txt $ mkShowable spec
    r <- callWebDriver True $ mkRequest spec
    parseIO spec r
--------------------------------------------------------------------------------

{-
--------------------------------------------------------------------------------
-- no console out for "production"
run :: W3Spec a -> IO a
run = execute

execute :: forall a. W3Spec a -> IO a
execute spec = do
  r <- callWebDriver False $ mkRequest spec
  parseIO spec r
--------------------------------------------------------------------------------
-}

-- TODO: will neeed to be parameterised later
mkRequest :: forall a. W3Spec a -> RequestArgs
mkRequest = \case
  Get {path} -> RequestParams path GET NoReqBody 4444
  Post {path, body} -> RequestParams path POST (ReqBodyJson body) 4444
  PostEmpty {path} -> RequestParams path POST (ReqBodyJson $ object []) 4444
  Delete {path} -> RequestParams path DELETE NoReqBody 4444

parseIO :: W3Spec a -> WD.HttpResponse -> IO a
parseIO spec r =
  maybe
    (fail . toS $ spec.description <> "\n" <> "Failed to parse response:\n " <> txt r)
    pure
    $ spec.parser r



devLog :: (MonadIO m) => Text -> m ()
devLog = liftIO . T.putStrLn

callWebDriver :: Bool -> RequestArgs -> IO HttpResponse
callWebDriver wantLog RequestParams {subDirs, method, body, port = prt} =
  runReq defaultHttpConfig $ do
    r <- req method url body jsonResponse $ port prt
    log $ "JSON Response:\n" <> txt r
    let fr =
          Response
            { statusCode = responseStatusCode r,
              statusMessage = getLenient . toS $ responseStatusMessage r,
              headers = L.responseHeaders . toVanillaResponse $ r,
              body = responseBody r :: Value,
              cookies = responseCookieJar r
            }
    log $ "Framework Response:\n" <> txt fr
    pure fr
  where
    log = when wantLog . devLog
    url :: Url 'Http
    url = foldl' (/:) (http "127.0.0.1") subDirs

describe :: (Show a) => Text -> IO a -> IO a
describe msg action = do
  T.putStrLn msg
  ethr <- handleEx action
  logResponse ethr
  either (fail . toS . txt) pure ethr

handleEx :: IO a -> IO (Either HttpException a)
handleEx = try

logResponse :: (Show a) => Either HttpException a -> IO ()
logResponse =
  either
    ( \e -> do
        T.putStrLn "!!!!!!!!!! REQUEST FAILED !!!!!!!!!!!"
        T.putStrLn $ txt e
    )
    ( \r -> do
        T.putStrLn "!!!!!!!!!! REQUEST SUCCEEDED !!!!!!!!!!!"
        T.putStrLn $ txt r
    )