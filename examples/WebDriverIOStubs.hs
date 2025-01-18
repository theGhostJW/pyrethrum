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
      fullScreenWindow,
      elementText, WindowRect )
import WebDriverSpec
    ( DriverStatus,
      SessionId(Session),
      Selector(..),
      ElementId(Element) )
import Prelude hiding (get, second)
import Data.Text.IO qualified as T
import PyrethrumExtras (txt)

-- ############# IO Implementation #############

_status :: IO DriverStatus
_status = status
-- >>> _status
-- Ready

_newDefaultFirefoxSession :: IO SessionId
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

_latestSession :: SessionId
_latestSession = Session "1beac3df-15c8-490d-8eb5-65691b6e16d0"

_maximizeWindow :: IO WindowRect
_maximizeWindow = maximizeWindow _latestSession

-- >>> _maximizeWindow

_minimizeWindow :: IO WindowRect
_minimizeWindow = minimizeWindow _latestSession

-- >>> _minimizeWindow

_fullscreenWindow :: IO WindowRect
_fullscreenWindow = fullScreenWindow _latestSession

-- >>> _fullscreenWindow

_deleteLastSession :: IO ()
_deleteLastSession = deleteSession _latestSession

-- >>> _deleteLastSession
-- *** Exception: user error (VanillaHttpException (HttpExceptionRequest Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json")]
--   path                 = "/session/1beac3df-15c8-490d-8eb5-65691b6e16d0"
--   queryString          = ""
--   method               = "DELETE"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
--  (StatusCodeException (Response {responseStatus = Status {statusCode = 404, statusMessage = "Not Found"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","131"),("date","Thu, 05 Sep 2024 23:20:05 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json")]
--   path                 = "/session/1beac3df-15c8-490d-8eb5-65691b6e16d0"
--   queryString          = ""
--   method               = "DELETE"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
-- , responseEarlyHints = []}) "{\"value\":{\"error\":\"invalid session id\",\"message\":\"Got unexpected session id 1beac3df-15c8-490d-8eb5-65691b6e16d0\",\"stacktrace\":\"\"}}")))

_theInternet :: Text
_theInternet = "https://the-internet.herokuapp.com/"

_navigateToTheInternet :: IO ()
_navigateToTheInternet = navigateTo _latestSession _theInternet

-- >>> _navigateToTheInternet

_checkBoxesLinkCss :: Selector
_checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

_findCheckBoxesLink :: IO ElementId
_findCheckBoxesLink = findElement _latestSession _checkBoxesLinkCss

-- >>> _findCheckBoxesLink
-- Element {id = "ffef18ab-c34c-48b5-8031-4b762c453b64"}

_checkboxesLinkElement :: ElementId
_checkboxesLinkElement = Element "ffef18ab-c34c-48b5-8031-4b762c453b64"

_findMissing :: IO ElementId
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
-- *** Exception: user error (VanillaHttpException (HttpExceptionRequest Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/1beac3df-15c8-490d-8eb5-65691b6e16d0/element/ffef18ab-c34c-48b5-8031-4b762c453b64/click"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
--  (StatusCodeException (Response {responseStatus = Status {statusCode = 404, statusMessage = "Not Found"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","123"),("date","Thu, 05 Sep 2024 23:36:45 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/1beac3df-15c8-490d-8eb5-65691b6e16d0/element/ffef18ab-c34c-48b5-8031-4b762c453b64/click"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
-- , responseEarlyHints = []}) "{\"value\":{\"error\":\"invalid session id\",\"message\":\"Tried to run command without establishing a connection\",\"stacktrace\":\"\"}}")))

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

-- >>> _endToEnd
