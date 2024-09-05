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
