{-# LANGUAGE UndecidableInstances #-}

module WebDriverIO where

import Data.Aeson (Value)
import Data.Text.IO qualified as T
-- import Effectful.Reader.Dynamic

import Network.HTTP.Client qualified as L
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    HttpBody,
    HttpBodyAllowed,
    HttpException,
    HttpMethod (AllowsBody),
    NoReqBody (NoReqBody),
    POST (POST),
    ProvidesBody,
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
import Network.HTTP.Types qualified as L
import PyrethrumExtras (delete, getLenient, toS, txt, uu)
import UnliftIO (try)
import Web.Api.WebDriver (Capabilities, WebDriverT, defaultFirefoxCapabilities)
import WebDriverEffect (WebUI (..), SessionId(..))
import Prelude hiding (get, second)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.KeyMap (lookup, singleton)
import Data.Aeson.Types (parseMaybe)
import WebDriverPure
import WebDriverSpec
import Core (Node(path))


data Result a = MkResult
  { response :: HttpResponse,
    value :: a
  }
  deriving (Show)

-- will neeed to be parameterised later
mkRequest :: forall a. W3Spec a -> RequestArgs
mkRequest =  \case 
  Get {path} -> RequestParams path GET NoReqBody 4444
  Post {path, body} -> RequestParams path POST (ReqBodyJson body) 4444
  PostEmpty {path} -> RequestParams path POST NoReqBody 4444
  Delete {path} -> RequestParams path DELETE NoReqBody 4444

execute :: forall a. W3Spec a -> IO (Maybe (Result a))
execute spec = do
  r <- callWebDriver $ mkRequest spec 
  pure $ MkResult r <$> spec.parser r

callWebDriver :: RequestArgs -> IO HttpResponse
callWebDriver RequestParams {subDirs, method, body, port = prt} =
  runReq defaultHttpConfig $ do
    r <- req method url body jsonResponse $ port prt
    liftIO $ T.putStrLn $ txt r
    pure $
      Response
        { statusCode = responseStatusCode r,
          statusMessage = getLenient . toS $ responseStatusMessage r,
          headers = L.responseHeaders . toVanillaResponse $ r,
          body = responseBody r :: Value,
          cookies = responseCookieJar r
        }
  where
    url :: R.Url 'Http
    url = foldl' (/:) (http "127.0.0.1") subDirs

describe_ :: Show a => Text -> IO a -> IO ()
describe_ = (void .) . describe

describe :: Show a => Text -> IO a -> IO a
describe msg action = do
  T.putStrLn msg
  ethr <- handleEx action
  logResponse ethr
  either (fail . toS . txt) pure ethr

handleEx :: IO a -> IO (Either HttpException a)
handleEx = try

logResponse :: Show a => Either HttpException a -> IO ()
logResponse = either
          ( \e -> do
              T.putStrLn "!!!!!!!!!! REQUEST FAILED !!!!!!!!!!!"
              T.putStrLn $ txt e
          )
          ( \r -> do
              T.putStrLn "!!!!!!!!!! REQUEST SUCCEEDED !!!!!!!!!!!"
              T.putStrLn $ txt r
          )


{-
-- $ > driverRunning
driverRunning :: IO Bool
driverRunning = responseCode200 <$> handleEx status

-- mkRequestParams :: HttpPathSpec -> RequestArgs
-- mkRequestParams ps = RequestParams subDirs method body port
-- >>> status
-- Response {statusCode = 200, statusMessage = "OK", headers = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","61"),("date","Sun, 01 Sep 2024 05:33:52 GMT")], body = Object (fromList [("value",Object (fromList [("message",String "Session already started"),("ready",Bool False)]))]), cookies = CJ {expose = []}}
status :: IO HttpResponse
status = get pathStatus

navigateTo :: SessionId -> Text -> IO HttpResponse
navigateTo s url = post (pathNavigateTo s) $ object ["url" .= url]

-- >>> navigateTo_ latestSession theInternet
navigateTo_ :: SessionId -> Text -> IO ()
navigateTo_ s = void . navigateTo s

-- >>> stub (Desc "Find By Css") $ findByCss latestSession checkBoxesLinkCss
-- Just (MkElementRef {id = "e9ec0c9a-2a3d-46f7-a112-b1f4361a419a"})
findByCss :: SessionId -> Text -> IO (Maybe ElementRef)
findByCss sessionId cssSelector = do
  r <- post (pathFindElement sessionId) $
    object ["using" .= ("css selector" :: Text), "value" .= cssSelector]
  pure $ parseElementRef  r
  
-- >>> stub (Desc "Find By Css") $ click latestSession $ MkElementRef {id = "e9ec0c9a-2a3d-46f7-a112-b1f4361a419a"}
-- *** Exception: user error (VanillaHttpException (HttpExceptionRequest Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/c2c16576-baa1-464e-aebb-694ed384017e/element/e9ec0c9a-2a3d-46f7-a112-b1f4361a419a/click"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
--  (StatusCodeException (Response {responseStatus = Status {statusCode = 404, statusMessage = "Not Found"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","932"),("date","Sun, 01 Sep 2024 11:05:41 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/c2c16576-baa1-464e-aebb-694ed384017e/element/e9ec0c9a-2a3d-46f7-a112-b1f4361a419a/click"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
-- , responseEarlyHints = []}) "{\"value\":{\"error\":\"stale element reference\",\"message\":\"The element with the reference e9ec0c9a-2a3d-46f7-a112-b1f4361a419a is stale; either its node document is not the active document, or it is no longer connected to the DOM\",\"stacktrace\":\"RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:193:5\\nStaleElementReferenceError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:725:5\\ngetKnownElement@chrome://remote/content/marionette/json.sys.mjs:401:11\\ndeserializeJSON@chrome://remote/content/marionette/json.sys.mjs:259:20\\ncloneObject@chrome://remote/content/marionette/json.sys.mjs:59:24\\ndeserializeJSON@chrome://remote/content/marionette/json.sys.mjs:289:16\\njson.deserialize@chrome://remote/content/marionette/json.sys.mjs:293:10\\nreceiveMessage@chrome://remote/content/marionette/actors/MarionetteCommandsChild.sys.mjs:73:30\\n\"}}")))
click :: SessionId -> ElementRef -> IO HttpResponse
click s er = post (pathClick s er) $ object []

-- TODO error handling all actions
-- success 
-- Response {statusCode = 200, statusMessage = "OK", headers = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","14"),("date","Sun, 01 Sep 2024 11:03:41 GMT")], body = Object (fromList [("value",Null)]), cookies = CJ {expose = []}}

-- failure 
{-
VanillaHttpException (HttpExceptionRequest Request {
  host                 = "127.0.0.1"
  port                 = 4444
  secure               = False
  requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
  path                 = "/session/c2c16576-baa1-464e-aebb-694ed384017e/element/e9ec0c9a-2a3d-46f7-a112-b1f4361a419a/click"
  queryString          = ""
  method               = "POST"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
  proxySecureMode      = ProxySecureWithConnect
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 404, statusMessage = "Not Found"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","932"),("date","Sun, 01 Sep 2024 11:05:41 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
  host                 = "127.0.0.1"
  port                 = 4444
  secure               = False
  requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
  path                 = "/session/c2c16576-baa1-464e-aebb-694ed384017e/element/e9ec0c9a-2a3d-46f7-a112-b1f4361a419a/click"
  queryString          = ""
  method               = "POST"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
  proxySecureMode      = ProxySecureWithConnect
}
, responseEarlyHints = []}) "{\"value\":{\"error\":\"stale element reference\",\"message\":\"The element with the reference e9ec0c9a-2a3d-46f7-a112-b1f4361a419a is stale; either its node document is not the active document, or it is no longer connected to the DOM\",\"stacktrace\":\"RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:193:5\\nStaleElementReferenceError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:725:5\\ngetKnownElement@chrome://remote/content/marionette/json.sys.mjs:401:11\\ndeserializeJSON@chrome://remote/content/marionette/json.sys.mjs:259:20\\ncloneObject@chrome://remote/content/marionette/json.sys.mjs:59:24\\ndeserializeJSON@chrome://remote/content/marionette/json.sys.mjs:289:16\\njson.deserialize@chrome://remote/content/marionette/json.sys.mjs:293:10\\nreceiveMessage@chrome://remote/content/marionette/actors/MarionetteCommandsChild.sys.mjs:73:30\\n\"}}"))
-}

-- >>> newFirefoxSession
-- Just (Session {id = "c2c16576-baa1-464e-aebb-694ed384017e"})
newFirefoxSession :: IO (Maybe SessionId)
newFirefoxSession = do
  r <- newSession defaultFirefoxCapabilities
  T.putStrLn $ txt r
  pure r


newSession :: Capabilities -> IO (Maybe SessionId)
newSession caps = parseSessionId <$> post pathNewSession (capsToJson caps)

-- >>> deleteSession $ MkSessionId "efc5b851-636e-4122-b2f9-018071f96200"
-- Response {statusCode = 200, statusMessage = "OK", headers = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","14"),("date","Sat, 31 Aug 2024 09:19:50 GMT")], body = Object (fromList [("value",Null)]), cookies = CJ {expose = []}}
deleteSession :: SessionId -> IO HttpResponse
deleteSession session = request $ defaultRequest {subDirs = ["session", session.id], method = DELETE}

deleteSession_ :: SessionId -> IO ()
deleteSession_ = void . deleteSession

-- >>> elementText latestSession (MkElementRef "e9ec0c9a-2a3d-46f7-a112-b1f4361a419a")
-- Just "Checkboxes"
elementText :: SessionId -> ElementRef -> IO (Maybe Text)
elementText s er = do
  r <- get (pathElementText s er)
  pure $ parseElmText r

-- $ > cycleSession

-- >>> cycleSession
cycleSession :: IO ()
cycleSession = do
  mSid <- stub (Desc "New Firefox Session") newFirefoxSession
  stub_ (Desc "Get Status") status
  mSid & maybe (fail "Failed to get session ID") 
       (stub_ (Desc "Delete Session") . deleteSession) 
  stub_ (Desc "Get Status") status

lastSession :: SessionId
lastSession = Session "238b3b38-96de-41e1-8f90-04bd2136e579"

-- >>> killSession
killSession :: IO ()
killSession = stub_ (Desc "Get Status") $ deleteSession lastSession



_theInternet :: Text
_theInternet = "https://the-internet.herokuapp.com/"

_latestSession :: SessionId
_latestSession = Session "c2c16576-baa1-464e-aebb-694ed384017e"

_checkBoxesLinkCss :: Text
_checkBoxesLinkCss = "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"



-}