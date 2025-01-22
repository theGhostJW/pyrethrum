{-# LANGUAGE UndecidableInstances #-}

module WebDriverSpec
  ( W3Spec (..),
    HttpResponse (..),
    ElementId (..),
    SessionId (..),
    DriverStatus (..),
    SameSite (..),
    Selector (..),
    Cookie (..),
    Timeouts (..),
    WindowHandle (..),
    FrameReference (..),
    WindowRect (..),
    -- Capabilities(..),
    mkShowable,
    --- Specs
    status,
    maximizeWindow,
    minimizeWindow,
    fullscreenWindow,
    getTimeouts,
    setTimeouts,
    switchToFrame,
    getCurrentUrl,
    findElementFromElement,
    findElementsFromElement,
    findElements,
    getTitle,
    getWindowHandle,
    isElementSelected,
    closeWindow,
    back,
    forward,
    refresh,
    -- newSessionSpec,
    newSession',
    deleteSession,
    getActiveElement,
    getWindowHandles,
    newWindow,
    switchToWindow,
    navigateTo,
    findElement,
    -- findElement',
    getWindowRect,
    elementClick,
    getElementText,
    switchToParentFrame,
    getElementProperty,
    getElementAttribute,
    getElementCssValue,
    setWindowRect,
    findElementsFromShadowRoot,
    getElementShadowRoot,
    findElementFromShadowRoot,
    getElementTagName,
    getElementRect,
    isElementEnabled,
    getElementComputedRole,
    getElementComputedLabel,
    elementClear,
    elementSendKeys,
    getPageSource,
    takeScreenshot,
    takeElementScreenshot,
    printPage,
    executeScript,
    executeScriptAsync,
    getAllCookies,
    getNamedCookie,
    addCookie,
    deleteCookie,
    deleteAllCookies,
    dismissAlert,
    acceptAlert,
    getAlertText,
    sendAlertText,
  )
where

import BasePrelude (Show (..))
import Data.Aeson
  ( Key,
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (..),
    object,
  )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap qualified as AKM
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text (pack)
import PyrethrumExtras (toS)
import Prelude hiding (get)

-- import Network.HTTP.Types qualified as NT (ResponseHeaders)

{- Pure types and functions used in Webdriver -}

--  TODO: add error handler / classifier
-- (Webdriver errors - library agnostic) vs HTTP errors (eg. driver not runnning - library dependent?)

data W3Spec a
  = Get
      { description :: Text,
        path :: [Text],
        parser :: HttpResponse -> Maybe a
      }
  | Post
      { description :: Text,
        path :: [Text],
        body :: Value,
        parser :: HttpResponse -> Maybe a
      }
  | PostEmpty
      { description :: Text,
        path :: [Text],
        parser :: HttpResponse -> Maybe a
      }
  | Delete
      { description :: Text,
        path :: [Text],
        parser :: HttpResponse -> Maybe a
      }

instance (Show a) => Show (W3Spec a) where
  show :: W3Spec a -> String
  show = Prelude.show . mkShowable

data W3SpecShowable = Request
  { description :: Text,
    method :: Text,
    path :: [Text],
    body :: Maybe Text
  }
  deriving (Show)

data HttpResponse = Response
  { statusCode :: Int,
    statusMessage :: Text,
    body :: Value
    -- not used yet may be able to remove and reduce dependencies
    -- headers :: NT.ResponseHeaders,
    -- cookies :: NC.CookieJar
  }
  deriving (Show)

data WindowHandle = Handle
  { handle :: Text,
    handletype :: Text
  }
  deriving (Show, Eq)

newtype ElementId = Element {id :: Text}
  deriving (Show, Eq)

newtype SessionId = Session {id :: Text}
  deriving (Show)

data DriverStatus
  = Ready
  | Running
  | ServiceError {statusCode :: Int, statusMessage :: Text}
  | Unknown {statusCode :: Int, statusMessage :: Text}
  deriving (Show, Eq)

-- https://www.w3.org/TR/webdriver2/#cookies
data SameSite
  = Lax
  | Strict
  | None
  deriving (Show, Eq)

instance ToJSON SameSite where
  toJSON :: SameSite -> Value
  toJSON = String . pack . BasePrelude.show

data FrameReference
  = TopLevelFrame
  | FrameNumber Word16
  | FrameElementId ElementId
  deriving (Show)

frameJson :: FrameReference -> Value
frameJson fr =
  object
    ["id" .= toJSON (frameVariant fr)]
  where
    frameVariant =
      \case
        TopLevelFrame -> Null
        FrameNumber n -> Number $ fromIntegral n
        FrameElementId elm -> object [elementFieldName .= elm.id]

data Cookie = Cookie
  { name :: Text,
    value :: Text,
    -- optional
    path :: Maybe Text,
    domain :: Maybe Text,
    secure :: Maybe Bool,
    httpOnly :: Maybe Bool,
    sameSite :: Maybe SameSite,
    -- When the cookie expires, specified in seconds since Unix Epoch.
    expiry :: Maybe Int
  }
  deriving (Show)

instance ToJSON Cookie where
  toJSON :: Cookie -> Value
  toJSON Cookie {name, value, path, domain, secure, httpOnly, sameSite, expiry} =
    object $
      [ "name" .= name,
        "value" .= value
      ]
        <> catMaybes
          [ ("path" .=) <$> path,
            ("domain" .=) <$> domain,
            ("secure" .=) <$> secure,
            ("httpOnly" .=) <$> httpOnly,
            ("sameSite" .=) <$> sameSite,
            ("expiry" .=) <$> expiry
          ]

cookieJSON :: Cookie -> Value
cookieJSON c = object ["cookie" .= c]

-- TODO: add more selector types
data Selector
  = CSS Text
  | XPath Text
  | LinkText Text
  | PartialLinkText Text
  | TagName Text
  deriving (Show, Eq)

-- TODO capabilities for all browsers - to annewSessionSpec'd from JSON
-- move to separate module added typed definition to all APIs that require
-- JSON
data Capabilities = MkCapabilities
  {
  }
  -- NOT IMPLEMENTED
  -- browserName :: Text,
  -- browserVersion :: Text,
  -- platformName :: Text

  deriving (Show)

{-
-- TODO: own capabilities type to from Json
-- capsToJson :: Capabilities -> Value
-- capsToJson caps = uu

object
  [ "capabilities"
      .= object
        ["alwaysMatch" .= toJSON caps],
    "desiredCapabilities" .= toJSON caps
  ]

to / from JSON
instance ToJSON Capabilities where
  toJSON Capabilities {browserName, browserVersion, platformName} =
    object
      [ "browserName" .= browserName,
        "browserVersion" .= browserVersion,
        "platformName" .= platformName
      ]

instance FromJSON Capabilities where
  parseJSON = withObject "Capabilities" $ \o ->
    Capabilities
      <$> o .: "browserName"
      <*> o .: "browserVersion"
      <*> o .: "platformName"

-- from w3c library
capsToJson :: Capabilities -> Value
capsToJson caps =
  object
    [ "capabilities"
        .= object
          ["alwaysMatch" .= toJSON caps],
      "desiredCapabilities" .= toJSON caps
    ]
-}

-- ######################################################################
-- ########################### WebDriver API ############################
-- ######################################################################
{- FUll List
https://www.w3.org/TR/2024/WD-webdriver2-20241212/#endpoints
61 endpoints
Method 	URI Template 	Command
POST 	/session 	New Session
DELETE 	/session/{session id} 	Delete Session
GET 	/status 	Status
GET 	/session/{session id}/timeouts 	Get Timeouts
POST 	/session/{session id}/timeouts 	Set Timeouts
POST 	/session/{session id}/url 	Navigate To
GET 	/session/{session id}/url 	Get Current URL
POST 	/session/{session id}/back 	Back
POST 	/session/{session id}/forward 	Forward
POST 	/session/{session id}/refresh 	Refresh
GET 	/session/{session id}/title 	Get Title
GET 	/session/{session id}/window 	Get Window Handle
DELETE 	/session/{session id}/window 	Close Window
POST 	/session/{session id}/window 	Switch To Window
GET 	/session/{session id}/window/handles 	Get Window Handles
POST 	/session/{session id}/window/new 	New Window
POST 	/session/{session id}/frame 	Switch To Frame
POST 	/session/{session id}/frame/parent 	Switch To Parent Frame
GET 	/session/{session id}/window/rect 	Get Window Rect
POST 	/session/{session id}/window/rect 	Set Window Rect
POST 	/session/{session id}/window/maximize 	Maximize Window
POST 	/session/{session id}/window/minimize 	Minimize Window
POST 	/session/{session id}/window/fullscreen 	Fullscreen Window
GET 	/session/{session id}/element/active 	Get Active Element
GET 	/session/{session id}/element/{element id}/shadow 	Get Element Shadow Root
POST 	/session/{session id}/element 	Find Element
POST 	/session/{session id}/elements 	Find Elements
POST 	/session/{session id}/element/{element id}/element 	Find Element From Element
POST 	/session/{session id}/element/{element id}/elements 	Find Elements From Element
POST 	/session/{session id}/shadow/{shadow id}/element 	Find Element From Shadow Root
POST 	/session/{session id}/shadow/{shadow id}/elements 	Find Elements From Shadow Root
GET 	/session/{session id}/element/{element id}/selected 	Is Element Selected
GET 	/session/{session id}/element/{element id}/attribute/{name} 	Get Element Attribute
GET 	/session/{session id}/element/{element id}/property/{name} 	Get Element Property
GET 	/session/{session id}/element/{element id}/css/{property name} 	Get Element CSS Value
GET 	/session/{session id}/element/{element id}/text 	Get Element Text
GET 	/session/{session id}/element/{element id}/name 	Get Element Tag Name
GET 	/session/{session id}/element/{element id}/rect 	Get Element Rect
GET 	/session/{session id}/element/{element id}/enabled 	Is Element Enabled
GET 	/session/{session id}/element/{element id}/computedrole 	Get Computed Role
GET 	/session/{session id}/element/{element id}/computedlabel 	Get Computed Label
POST 	/session/{session id}/element/{element id}/click 	Element Click
POST 	/session/{session id}/element/{element id}/clear 	Element Clear
POST 	/session/{session id}/element/{element id}/value 	Element Send Keys
GET 	/session/{session id}/source 	Get Page Source
POST 	/session/{session id}/execute/sync 	Execute Script
POST 	/session/{session id}/execute/async 	Execute Async Script
GET 	/session/{session id}/cookie 	Get All Cookies
GET 	/session/{session id}/cookie/{name} 	Get Named Cookie
POST 	/session/{session id}/cookie 	Add Cookie
DELETE 	/session/{session id}/cookie/{name} 	Delete Cookie
DELETE 	/session/{session id}/cookie 	Delete All Cookies
POST 	/session/{session id}/actions 	Perform Actions
DELETE 	/session/{session id}/actions 	Release Actions
POST 	/session/{session id}/alert/dismiss 	Dismiss Alert
POST 	/session/{session id}/alert/accept 	Accept Alert
GET 	/session/{session id}/alert/text 	Get Alert Text
POST 	/session/{session id}/alert/text 	Send Alert Text
GET 	/session/{session id}/screenshot 	Take Screenshot
GET 	/session/{session id}/element/{element id}/screenshot 	Take Element Screenshot
POST 	/session/{session id}/print 	Print Page
-}

-- https://www.w3.org/TR/2024/WD-webdriver2-20241212/#endpoints
-- 61 endpoints
-- Method 	URI Template 	Command

-- TODO: native capabilities type - change this to use type
-- newSessionSpec :: Capabilities -> W3Spec SessionRef
-- newSessionSpec capabilities = newSessionSpec' $ capsToJson capabilities

-- POST 	/session 	New Session
newSession' :: Value -> W3Spec SessionId
newSession' capabilities = Post "Create New Session" [session] capabilities parseSessionRef

-- DELETE 	/session/{session id} 	Delete Session
deleteSession :: SessionId -> W3Spec ()
deleteSession sessionRef = Delete "Delete Session" (sessionUri sessionRef.id) voidParser

-- GET 	/status 	Status
status :: W3Spec DriverStatus
status = Get "Get Driver Status" ["status"] parseDriverStatus

-- GET 	/session/{session id}/timeouts 	Get Timeouts
getTimeouts :: SessionId -> W3Spec Timeouts
getTimeouts sessionRef = Get "Get Timeouts" (sessionUri1 sessionRef "timeouts") parseTimeouts

-- POST 	/session/{session id}/timeouts 	Set Timeouts
setTimeouts :: SessionId -> Timeouts -> W3Spec ()
setTimeouts sessionRef Timeouts {implicit, pageLoad, script} =
  Post "Set Timeouts" (sessionUri1 sessionRef "timeouts") (object ["implicit" .= implicit, "pageLoad" .= pageLoad, "script" .= script]) voidParser

-- POST 	/session/{session id}/url 	Navigate To
navigateTo :: SessionId -> Text -> W3Spec ()
navigateTo sessionRef url = Post "Navigate To" (sessionUri1 sessionRef "url") (object ["url" .= url]) voidParser

-- GET 	/session/{session id}/url 	Get Current URL
getCurrentUrl :: SessionId -> W3Spec Text
getCurrentUrl sessionRef = Get "Get Current URL" (sessionUri1 sessionRef "url") parseBodyTxt

-- POST 	/session/{session id}/back 	Back
back :: SessionId -> W3Spec ()
back sessionRef = PostEmpty "Back" (sessionUri1 sessionRef "back") voidParser

-- POST 	/session/{session id}/forward 	Forward
forward :: SessionId -> W3Spec ()
forward sessionRef = PostEmpty "Forward" (sessionUri1 sessionRef "forward") voidParser

-- POST 	/session/{session id}/refresh 	Refresh
refresh :: SessionId -> W3Spec ()
refresh sessionRef = PostEmpty "Refresh" (sessionUri1 sessionRef "refresh") voidParser

-- GET 	/session/{session id}/title 	Get Title
getTitle :: SessionId -> W3Spec Text
getTitle sessionRef = Get "Get Title" (sessionUri1 sessionRef "title") parseBodyTxt

-- GET 	/session/{session id}/window 	Get Window Handle
getWindowHandle :: SessionId -> W3Spec Text
getWindowHandle sessionRef = Get "Get Window Handle" (sessionUri1 sessionRef "window") parseBodyTxt

-- POST 	/session/{session id}/window/new 	New Window
newWindow :: SessionId -> W3Spec WindowHandle
newWindow sessionRef = PostEmpty "New Window" (sessionUri2 sessionRef "window" "new") windowHandleParser

-- DELETE 	/session/{session id}/window 	Close Window
closeWindow :: SessionId -> W3Spec ()
closeWindow sessionRef = Delete "Close Window" (sessionUri1 sessionRef "window") voidParser

-- POST 	/session/{session id}/window 	Switch To Window
switchToWindow :: SessionId -> Text -> W3Spec ()
switchToWindow sessionRef handle = Post "Switch To Window" (sessionUri1 sessionRef "window") (object ["handle" .= handle]) voidParser

-- GET 	/session/{session id}/window/handles 	Get Window Handles
getWindowHandles :: SessionId -> W3Spec [Text]
getWindowHandles sessionRef = Get "Get Window Handles" (sessionUri2 sessionRef "window" "handles") windowHandlesParser

-- POST 	/session/{session id}/frame 	Switch To Frame
switchToFrame :: SessionId -> FrameReference -> W3Spec ()
switchToFrame sessionRef frameRef = Post "Switch To Frame" (sessionUri1 sessionRef "frame") (frameJson frameRef) voidParser

-- POST 	/session/{session id}/frame/parent 	Switch To Parent Frame
switchToParentFrame :: SessionId -> W3Spec ()
switchToParentFrame sessionRef = PostEmpty "Switch To Parent Frame" (sessionUri2 sessionRef "frame" "parent") voidParser

-- GET 	/session/{session id}/window/rect 	Get Window Rect
getWindowRect :: SessionId -> W3Spec WindowRect
getWindowRect sessionRef = Get "Get Window Rect" (sessionUri2 sessionRef "window" "rect") parseWindowRect

-- POST 	/session/{session id}/window/rect 	Set Window Rect
setWindowRect :: SessionId -> WindowRect -> W3Spec WindowRect
setWindowRect sessionRef rect = Post "Set Window Rect" (sessionUri2 sessionRef "window" "rect") (toJSON rect) parseWindowRect

-- POST 	/session/{session id}/window/maximize 	Maximize
maximizeWindow :: SessionId -> W3Spec WindowRect
maximizeWindow sessionRef = PostEmpty "Maximize Window" (windowUri1 sessionRef "maximize") parseWindowRect

-- POST 	/session/{session id}/window/minimize 	Minimize Window
minimizeWindow :: SessionId -> W3Spec WindowRect
minimizeWindow sessionRef = PostEmpty "Minimize Window" (windowUri1 sessionRef "minimize") parseWindowRect

-- POST 	/session/{session id}/window/fullscreen 	Fullscreen Window
fullscreenWindow :: SessionId -> W3Spec WindowRect
fullscreenWindow sessionRef = PostEmpty "Fullscreen Window" (windowUri1 sessionRef "fullscreen") parseWindowRect

-- GET 	/session/{session id}/element/active 	Get Active Element
getActiveElement :: SessionId -> W3Spec ElementId
getActiveElement sessionId = Get "Get Active Element" (sessionUri2 sessionId "element" "active") parseElementRef

-- GET 	/session/{session id}/element/{element id}/shadow 	Get Element Shadow Root
getElementShadowRoot :: SessionId -> ElementId -> W3Spec ElementId
getElementShadowRoot sessionId elementId = Get "Get Element Shadow Root" (elementUri1 sessionId elementId "shadow") parseShadowElementRef

-- POST 	/session/{session id}/element 	Find Element
findElement :: SessionId -> Selector -> W3Spec ElementId
findElement sessionRef = findElement' sessionRef . selectorJson

-- POST 	/session/{session id}/elements 	Find Elements
findElements :: SessionId -> Selector -> W3Spec [ElementId]
findElements sessionRef selector = Post "Find Elements" (sessionUri1 sessionRef "elements") (selectorJson selector) parseElementsRef

-- POST 	/session/{session id}/element/{element id}/element 	Find Element From Element
findElementFromElement :: SessionId -> ElementId -> Selector -> W3Spec ElementId
findElementFromElement sessionId elementId selector = Post "Find Element From Element" (elementUri1 sessionId elementId "element") (selectorJson selector) parseElementRef

-- POST 	/session/{session id}/element/{element id}/elements 	Find Elements From Element
findElementsFromElement :: SessionId -> ElementId -> Selector -> W3Spec [ElementId]
findElementsFromElement sessionId elementId selector = Post "Find Elements From Element" (elementUri1 sessionId elementId "elements") (selectorJson selector) parseElementsRef

-- POST 	/session/{session id}/shadow/{shadow id}/element 	Find Element From Shadow Root
findElementFromShadowRoot :: SessionId -> ElementId -> Selector -> W3Spec ElementId
findElementFromShadowRoot sessionId shadowId selector = Post "Find Element From Shadow Root" (elementUri1 sessionId shadowId "element") (selectorJson selector) parseElementRef

-- POST 	/session/{session id}/shadow/{shadow id}/elements 	Find Elements From Shadow Root
findElementsFromShadowRoot :: SessionId -> ElementId -> Selector -> W3Spec [ElementId]
findElementsFromShadowRoot sessionId shadowId selector = Post "Find Elements From Shadow Root" (elementUri1 sessionId shadowId "elements") (selectorJson selector) parseElementsRef

-- GET 	/session/{session id}/element/{element id}/selected 	Is Element Selected
isElementSelected :: SessionId -> ElementId -> W3Spec Bool
isElementSelected sessionId elementId = Get "Is Element Selected" (elementUri1 sessionId elementId "selected") parseBodyBool

-- GET 	/session/{session id}/element/{element id}/attribute/{name} 	Get Element Attribute
getElementAttribute :: SessionId -> ElementId -> Text -> W3Spec Text
getElementAttribute sessionId elementId attributeName = Get "Get Element Attribute" (elementUri2 sessionId elementId "attribute" attributeName) parseBodyTxt

-- GET 	/session/{session id}/element/{element id}/property/{name} 	Get Element Property
getElementProperty :: SessionId -> ElementId -> Text -> W3Spec Value
getElementProperty sessionId elementId propertyName = Get "Get Element Property" (elementUri2 sessionId elementId "property" propertyName) bodyValue

-- GET 	/session/{session id}/element/{element id}/css/{property name} 	Get Element CSS Value
getElementCssValue :: SessionId -> ElementId -> Text -> W3Spec Text
getElementCssValue sessionId elementId propertyName = Get "Get Element CSS Value" (elementUri2 sessionId elementId "css" propertyName) parseBodyTxt

-- GET 	/session/{session id}/element/{element id}/text 	Get Element Text
getElementText :: SessionId -> ElementId -> W3Spec Text
getElementText sessionId elementId = Get "Get Element Text" (elementUri1 sessionId elementId "text") parseBodyTxt

-- GET 	/session/{session id}/element/{element id}/name 	Get Element Tag Name
getElementTagName :: SessionId -> ElementId -> W3Spec Text
getElementTagName sessionId elementId = Get "Get Element Tag Name" (elementUri1 sessionId elementId "name") parseBodyTxt

-- GET 	/session/{session id}/element/{element id}/rect 	Get Element Rect
getElementRect :: SessionId -> ElementId -> W3Spec WindowRect
getElementRect sessionId elementId = Get "Get Element Rect" (elementUri1 sessionId elementId "rect") parseWindowRect

-- GET 	/session/{session id}/element/{element id}/enabled 	Is Element Enabled
isElementEnabled :: SessionId -> ElementId -> W3Spec Bool
isElementEnabled sessionId elementId = Get "Is Element Enabled" (elementUri1 sessionId elementId "enabled") parseBodyBool

-- GET 	/session/{session id}/element/{element id}/computedrole 	Get Computed Role
getElementComputedRole :: SessionId -> ElementId -> W3Spec Text
getElementComputedRole sessionId elementId = Get "Get Computed Role" (elementUri1 sessionId elementId "computedrole") parseBodyTxt

-- GET 	/session/{session id}/element/{element id}/computedlabel 	Get Computed Label
getElementComputedLabel :: SessionId -> ElementId -> W3Spec Text
getElementComputedLabel sessionId elementId = Get "Get Computed Label" (elementUri1 sessionId elementId "computedlabel") parseBodyTxt

-- POST 	/session/{session id}/element/{element id}/click 	Element Click
elementClick :: SessionId -> ElementId -> W3Spec ()
elementClick sessionId elementId = PostEmpty "Click Element" (elementUri1 sessionId elementId "click") voidParser

-- POST 	/session/{session id}/element/{element id}/clear 	Element Clear
elementClear :: SessionId -> ElementId -> W3Spec ()
elementClear sessionId elementId = PostEmpty "Clear Element" (elementUri1 sessionId elementId "clear") voidParser

-- POST 	/session/{session id}/element/{element id}/value 	Element Send Keys
elementSendKeys :: SessionId -> ElementId -> Text -> W3Spec ()
elementSendKeys sessionId elementId keysToSend = Post "Send Keys to Element" (elementUri1 sessionId elementId "value") (keysJson keysToSend) voidParser

-- GET 	/session/{session id}/source 	Get Page Source
getPageSource :: SessionId -> W3Spec Text
getPageSource sessionId = Get "Get Page Source" (sessionUri1 sessionId "source") parseBodyTxt

-- POST 	/session/{session id}/execute/sync 	Execute Script
executeScript :: SessionId -> Text -> [Value] -> W3Spec Value
executeScript sessionId script args = Post "Execute Script" (sessionUri2 sessionId "execute" "sync") (mkScript script args) bodyValue

-- POST 	/session/{session id}/execute/async 	Execute Async Script
executeScriptAsync :: SessionId -> Text -> [Value] -> W3Spec Value
executeScriptAsync sessionId script args = Post "Execute Async Script" (sessionUri2 sessionId "execute" "async") (mkScript script args) bodyValue

-- GET 	/session/{session id}/cookie 	Get All Cookies
getAllCookies :: SessionId -> W3Spec [Cookie]
getAllCookies sessionId = Get "Get All Cookies" (sessionUri1 sessionId "cookie") parseCookies

-- GET 	/session/{session id}/cookie/{name} 	Get Named Cookie
getNamedCookie :: SessionId -> Text -> W3Spec Cookie
getNamedCookie sessionId cookieName = Get "Get Named Cookie" (sessionUri2 sessionId "cookie" cookieName) parseCookie

-- POST 	/session/{session id}/cookie 	Add Cookie
addCookie :: SessionId -> Cookie -> W3Spec ()
addCookie sessionId cookie = Post "Add Cookie" (sessionUri1 sessionId "cookie") (cookieJSON cookie) voidParser

-- DELETE 	/session/{session id}/cookie/{name} 	Delete Cookie
deleteCookie :: SessionId -> Text -> W3Spec ()
deleteCookie sessionId cookieName = Delete "Delete Cookie" (sessionUri2 sessionId "cookie" cookieName) voidParser

-- DELETE 	/session/{session id}/cookie 	Delete All Cookies
deleteAllCookies :: SessionId -> W3Spec ()
deleteAllCookies sessionId = Delete "Delete All Cookies" (sessionUri1 sessionId "cookie") voidParser

-- POST 	/session/{session id}/actions 	Perform Actions
-- DELETE 	/session/{session id}/actions 	Release Actions

-- POST 	/session/{session id}/alert/dismiss 	Dismiss Alert
dismissAlert :: SessionId -> W3Spec ()
dismissAlert sessionId = PostEmpty "Dismiss Alert" (sessionUri2 sessionId "alert" "dismiss") voidParser

-- POST 	/session/{session id}/alert/accept 	Accept Alert
acceptAlert :: SessionId -> W3Spec ()
acceptAlert sessionId = PostEmpty "Accept Alert" (sessionUri2 sessionId "alert" "accept") voidParser

-- GET 	/session/{session id}/alert/text 	Get Alert Text
getAlertText :: SessionId -> W3Spec Text
getAlertText sessionId = Get "Get Alert Text" (sessionUri2 sessionId "alert" "text") parseBodyTxt

-- POST 	/session/{session id}/alert/text 	Send Alert Text
sendAlertText :: SessionId -> Text -> W3Spec ()
sendAlertText sessionId text = Post "Send Alert Text" (sessionUri2 sessionId "alert" "text") (object ["text" .= text]) voidParser

-- GET 	/session/{session id}/screenshot 	Take Screenshot
takeScreenshot :: SessionId -> W3Spec Text
takeScreenshot sessionId = Get "Take Screenshot" (sessionUri1 sessionId "screenshot") parseBodyTxt

-- GET 	/session/{session id}/element/{element id}/screenshot 	Take Element Screenshot
takeElementScreenshot :: SessionId -> ElementId -> W3Spec Text
takeElementScreenshot sessionId elementId = Get "Take Element Screenshot" (elementUri1 sessionId elementId "screenshot") parseBodyTxt

-- POST 	/session/{session id}/print 	Print Page
printPage :: SessionId -> W3Spec Text
printPage sessionId = PostEmpty "Print Page" (sessionUri1 sessionId "print") parseBodyTxt

findElement' :: SessionId -> Value -> W3Spec ElementId
findElement' sessionRef selector = Post "Find Element" (sessionUri1 sessionRef "element") selector parseElementRef

-- #### Utils ####

data WindowRect = Rect
  { x :: Int,
    y :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show, Eq)

instance ToJSON WindowRect where
  toJSON :: WindowRect -> Value
  toJSON Rect {x, y, width, height} =
    object
      [ "x" .= x,
        "y" .= y,
        "width" .= width,
        "height" .= height
      ]

parseWindowRect :: HttpResponse -> Maybe WindowRect
parseWindowRect r =
  Rect
    <$> bdyInt "x"
    <*> bdyInt "y"
    <*> bdyInt "width"
    <*> bdyInt "height"
  where
    bdyInt = bodyInt r

mkScript :: Text -> [Value] -> Value
mkScript script args = object ["script" .= script, "args" .= args]

data Timeouts = Timeouts
  { implicit :: Int,
    pageLoad :: Int,
    script :: Int
  }
  deriving (Show)

windowHandleParser :: HttpResponse -> Maybe WindowHandle
windowHandleParser r =
  bodyValue r
    >>= windowHandleFromValue

windowHandlesParser :: HttpResponse -> Maybe [Text]
windowHandlesParser r = do
  bodyValue r
    >>= \case
      Array a -> Just $ catMaybes $ toList $ asText <$> a
      _ -> Nothing

windowHandleFromValue :: Value -> Maybe WindowHandle
windowHandleFromValue v =
  liftA2 Handle (lookupTxt "handle" v) (lookupTxt "type" v)

parseCookies :: HttpResponse -> Maybe [Cookie]
parseCookies r =
  bodyValue r
    >>= \case
      Array a -> Just $ mapMaybe cookieFromBody (toList a)
      _ -> Nothing

parseCookie :: HttpResponse -> Maybe Cookie
parseCookie r =
  bodyValue r
    >>= cookieFromBody

cookieFromBody :: Value -> Maybe Cookie
cookieFromBody b = do
  name <- lookupTxt "name" b
  value <- lookupTxt "value" b
  let path = lookupTxt "path" b
      domain = lookupTxt "domain" b
      secure = lookupBool "secure" b
      httpOnly = lookupBool "httpOnly" b
      sameSite = lookupSameSite "sameSite" b
      expiry = lookupInt "expiry" b
  pure $ Cookie {..}

parseTimeouts :: HttpResponse -> Maybe Timeouts
parseTimeouts r =
  liftA3
    Timeouts
    (bdyInt "implicit")
    (bdyInt "pageLoad")
    (bdyInt "script")
  where
    bdyInt = bodyInt r

selectorJson :: Selector -> Value
selectorJson = \case
  CSS css -> sJSON "css selector" css
  XPath xpath -> sJSON "xpath" xpath
  LinkText lt -> sJSON "link text" lt
  PartialLinkText plt -> sJSON "partial link text" plt
  TagName tn -> sJSON "tag name" tn
  where
    sJSON using value = object ["using" .= using, "value" .= value]

voidParser :: HttpResponse -> Maybe ()
voidParser _ = Just ()

bodyText' :: Maybe Value -> Key -> Maybe Text
bodyText' v k = v >>= lookupTxt k

bodyText :: HttpResponse -> Key -> Maybe Text
bodyText r = bodyText' (bodyValue r)

bodyInt' :: Maybe Value -> Key -> Maybe Int
bodyInt' v k = v >>= lookupInt k

bodyInt :: HttpResponse -> Key -> Maybe Int
bodyInt r = bodyInt' (bodyValue r)

parseBodyTxt :: HttpResponse -> Maybe Text
parseBodyTxt r = bodyValue r >>= asText

parseBodyBool :: HttpResponse -> Maybe Bool
parseBodyBool r =
  bodyValue r >>= \case
    Bool b -> Just b
    _ -> Nothing

parseElementsRef :: HttpResponse -> Maybe [ElementId]
parseElementsRef r =
  bodyValue r
    >>= \case
      Array a -> Just $ mapMaybe elemtRefFromBody (toList a)
      _ -> Nothing

-- TODO Aeson helpers separate module
lookup :: Key -> Value -> Maybe Value
lookup k = \case
  Object o -> AKM.lookup k o
  _ -> Nothing

lookupTxt :: Key -> Value -> Maybe Text
lookupTxt k v = lookup k v >>= asText

lookupBool :: Key -> Value -> Maybe Bool
lookupBool k v =
  lookup k v >>= \case
    Bool b -> Just b
    _ -> Nothing

lookupSameSite :: Key -> Value -> Maybe SameSite
lookupSameSite k v =
  lookup k v >>= \case
    String "Lax" -> Just Lax
    String "Strict" -> Just Strict
    String "None" -> Just None
    _ -> Nothing

lookupInt :: Key -> Value -> Maybe Int
lookupInt k v = lookup k v >>= asInt

asText :: Value -> Maybe Text
asText = \case
  String t -> Just t
  _ -> Nothing

asInt :: Value -> Maybe Int
asInt = \case
  Number t -> Just $ floor t
  _ -> Nothing

parseSessionRef :: HttpResponse -> Maybe SessionId
parseSessionRef r =
  Session
    <$> bodyText r "sessionId"

bodyValue :: HttpResponse -> Maybe Value
bodyValue r = lookup "value" r.body

-- https://www.w3.org/TR/webdriver2/#elements
elementFieldName :: Key
elementFieldName = "element-6066-11e4-a52e-4f735466cecf"

-- https://www.w3.org/TR/webdriver2/#shadow-root
shadowRootFieldName :: Key
shadowRootFieldName = "shadow-6066-11e4-a52e-4f735466cecf"

parseElementRef :: HttpResponse -> Maybe ElementId
parseElementRef r =
  Element <$> bodyText r elementFieldName

parseShadowElementRef :: HttpResponse -> Maybe ElementId
parseShadowElementRef r =
  Element <$> bodyText r shadowRootFieldName

elemtRefFromBody :: Value -> Maybe ElementId
elemtRefFromBody b = Element <$> lookupTxt elementFieldName b

session :: Text
session = "session"

sessionUri :: Text -> [Text]
sessionUri sp = [session, sp]

sessionUri1 :: SessionId -> Text -> [Text]
sessionUri1 sr sp = [session, sr.id, sp]

sessionUri2 :: SessionId -> Text -> Text -> [Text]
sessionUri2 sr sp sp2 = [session, sr.id, sp, sp2]

window :: Text
window = "window"

windowUri1 :: SessionId -> Text -> [Text]
windowUri1 sr sp = [session, sr.id, window, sp]

elementUri1 :: SessionId -> ElementId -> Text -> [Text]
elementUri1 sr er sp = [session, sr.id, "element", er.id, sp]

elementUri2 :: SessionId -> ElementId -> Text -> Text -> [Text]
elementUri2 sr er sp sp2 = [session, sr.id, "element", er.id, sp, sp2]

mkShowable :: W3Spec a -> W3SpecShowable
mkShowable = \case
  Get d p _ -> Request d "GET" p Nothing
  Post d p b _ -> Request d "POST" p (Just . toS . unpack $ encodePretty b)
  PostEmpty d p _ -> Request d "POST" p Nothing
  Delete d p _ -> Request d "DELETE" p Nothing

parseDriverStatus :: HttpResponse -> Maybe DriverStatus
parseDriverStatus Response {statusCode, statusMessage} =
  Just $
    statusCode & \case
      200 -> Ready
      500 -> ServiceError {statusCode, statusMessage}
      501 -> Running
      _ -> Unknown {statusCode, statusMessage}

keysJson :: Text -> Value
keysJson keysToSend = object ["text" .= keysToSend]