{-# LANGUAGE UndecidableInstances #-}

module WebDriverIOStubs where

-- import Effectful.Reader.Dynamic

import WebDriverIO
    ( status,
      newDefaultFirefoxSession,
      deleteSession,
      navigateTo,
      findElement,
      click,
      maximizeWindow,
      minimizeWindow,
      fullscreenWindow,
      elementText )
import WebDriverSpec
    ( DriverStatus,
      SessionRef(Session),
      Selector(..),
      ElementRef(Element) )
import Prelude hiding (get, second)

-- ############# IO Implementation #############

_status :: IO DriverStatus
_status = status

-- >>> _status
-- Ready

_newDefaultFirefoxSession :: IO SessionRef
_newDefaultFirefoxSession = newDefaultFirefoxSession

-- >>> newDefaultFirefoxSession

-- *** Exception: VanillaHttpException (HttpExceptionRequest Request {

--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
--  (StatusCodeException (Response {responseStatus = Status {statusCode = 500, statusMessage = "Internal Server Error"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","96"),("date","Thu, 05 Sep 2024 20:53:39 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
-- , responseEarlyHints = []}) "{\"value\":{\"error\":\"session not created\",\"message\":\"Session is already started\",\"stacktrace\":\"\"}}"))

_latestSession :: SessionRef
_latestSession = Session "1beac3df-15c8-490d-8eb5-65691b6e16d0"

_maximizeWindow :: IO ()
_maximizeWindow = maximizeWindow _latestSession

-- >>> _maximizeWindow

_minimizeWindow :: IO ()
_minimizeWindow = minimizeWindow _latestSession

-- >>> _minimizeWindow

_fullscreenWindow :: IO ()
_fullscreenWindow = fullscreenWindow _latestSession

-- >>> _fullscreenWindow

_deleteLastSession_ :: IO ()
_deleteLastSession_ = deleteSession _latestSession

-- >>> _deleteLastSession_

_theInternet :: Text
_theInternet = "https://the-internet.herokuapp.com/"

_navigateToTheInternet :: IO ()
_navigateToTheInternet = navigateTo _latestSession _theInternet

-- >>> _navigateToTheInternet

_checkBoxesLinkCss :: Selector
_checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

_findCheckBoxesLink :: IO ElementRef
_findCheckBoxesLink = findElement _latestSession _checkBoxesLinkCss

-- >>> _findCheckBoxesLink
-- Element {id = "ffef18ab-c34c-48b5-8031-4b762c453b64"}

_checkboxesLinkElement :: ElementRef
_checkboxesLinkElement = Element "ffef18ab-c34c-48b5-8031-4b762c453b64"

_findMissing :: IO ElementRef
_findMissing = findElement _latestSession $ CSS "#notHere"

-- >>> _findMissing

-- *** Exception: user error (VanillaHttpException (HttpExceptionRequest Request {

--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/1beac3df-15c8-490d-8eb5-65691b6e16d0/element"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
--  (StatusCodeException (Response {responseStatus = Status {statusCode = 404, statusMessage = "Not Found"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","392"),("date","Thu, 05 Sep 2024 21:26:42 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/1beac3df-15c8-490d-8eb5-65691b6e16d0/element"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
-- , responseEarlyHints = []}) "{\"value\":{\"error\":\"no such element\",\"message\":\"Unable to locate element: #notHere\",\"stacktrace\":\"RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:193:5\\nNoSuchElementError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:511:5\\ndom.find/</<@chrome://remote/content/shared/DOM.sys.mjs:136:16\\n\"}}")))

_checkBoxesLinkText :: IO Text
_checkBoxesLinkText = elementText _latestSession _checkboxesLinkElement

-- >>> _checkBoxesLinkText
-- "Checkboxes"

_clickCheckBoxesLink :: IO ()
_clickCheckBoxesLink = click _latestSession _checkboxesLinkElement

-- >>> _clickCheckBoxesLink

{-
-- $ > driverRunning
driverRunning :: IO Bool
driverRunning = responseCode200 <$> handleEx status

-- mkRequestParams :: HttpPathSpec -> RequestArgs
-- mkRequestParams ps = RequestParams subDirs method body port

-- >>> stub (Desc "Find By Css") $ findByCss latestSession checkBoxesLinkCss
-- Just (MkElementRef {id = "e9ec0c9a-2a3d-46f7-a112-b1f4361a419a"})
findByCss :: SessionRef -> Text -> IO (Maybe ElementRef)
findByCss SessionRef cssSelector = do
  r <- post (pathFindElement SessionRef) $
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
click :: SessionRef -> ElementRef -> IO HttpResponse
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

-- >>> deleteSession $ MkSessionRef "efc5b851-636e-4122-b2f9-018071f96200"
-- Response {statusCode = 200, statusMessage = "OK", headers = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","14"),("date","Sat, 31 Aug 2024 09:19:50 GMT")], body = Object (fromList [("value",Null)]), cookies = CJ {expose = []}}
deleteSession :: SessionRef -> IO HttpResponse
deleteSession session = request $ defaultRequest {subDirs = ["session", session.id], method = DELETE}

deleteSession_ :: SessionRef -> IO ()
deleteSession_ = void . deleteSession

-- >>> elementText latestSession (MkElementRef "e9ec0c9a-2a3d-46f7-a112-b1f4361a419a")
-- Just "Checkboxes"
elementText :: SessionRef -> ElementRef -> IO (Maybe Text)
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

lastSession :: SessionRef
lastSession = Session "238b3b38-96de-41e1-8f90-04bd2136e579"

-- >>> killSession
killSession :: IO ()
killSession = stub_ (Desc "Get Status") $ deleteSession lastSession

_theInternet :: Text
_theInternet = "https://the-internet.herokuapp.com/"

_latestSession :: SessionRef
_latestSession = Session "c2c16576-baa1-464e-aebb-694ed384017e"

_checkBoxesLinkCss :: Text
_checkBoxesLinkCss = "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

-}
