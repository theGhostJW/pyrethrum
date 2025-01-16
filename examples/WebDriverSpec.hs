{-# LANGUAGE UndecidableInstances #-}

module WebDriverSpec
  ( W3Spec (..),
    HttpResponse (..),
    ElementRef (..),
    SessionRef (..),
    DriverStatus (..),
    Selector (..),
    Cookie (..),
    Timeouts (..),
    WindowHandle (..),
    -- Capabilities(..),
    mkShowable,
    --- Specs
    status,
    maximizeWindow,
    minimizeWindow,
    fullscreenWindow,
    getTimeouts,
    setTimeouts,
    getCurrentUrl,
    getTitle,
    getWindowHandle,
    closeWindow,
    back,
    forward,
    refresh,
    -- newSessionSpec,
    newSession',
    deleteSession,
    getWindowHandles,
    newWindow,
    switchToWindow,
    navigateTo,
    findElement,
    findElement',
    click,
    elementText,
  )
where

import Data.Aeson
  ( Key,
    KeyValue ((.=)),
    Value (..),
    object,
  )
import Data.Aeson.KeyMap qualified as AKM
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

data W3SpecShowable = Request
  { description :: Text,
    method :: Text,
    path :: [Text],
    body :: Maybe Value
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

newtype ElementRef = Element {id :: Text}
  deriving (Show, Eq)

newtype SessionRef = Session {id :: Text}
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

data Cookie = Cookie
  { name :: Text,
    value :: Text,
    -- optional
    path :: Maybe Text,
    domain :: Maybe Text,
    secureOnly :: Maybe Bool,
    httpOnly :: Maybe Bool,
    sameSite :: Maybe SameSite,
    -- When the cookie expires, specified in seconds since Unix Epoch.
    expiry :: Maybe Int
  }
  deriving (Show)

-- TODO: add more selector types
newtype Selector = CSS Text
  deriving (Show)

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
newSession' :: Value -> W3Spec SessionRef
newSession' capabilities = Post "Create New Session" [session] capabilities parseSessionRef

-- DELETE 	/session/{session id} 	Delete Session
deleteSession :: SessionRef -> W3Spec ()
deleteSession sessionRef = Delete "Delete Session" (session1 sessionRef.id) voidParser

-- GET 	/status 	Status
status :: W3Spec DriverStatus
status = Get "Get Driver Status" ["status"] parseDriverStatus

-- GET 	/session/{session id}/timeouts 	Get Timeouts
getTimeouts :: SessionRef -> W3Spec Timeouts
getTimeouts sessionRef = Get "Get Timeouts" (sessionId1 sessionRef "timeouts") parseTimeouts

-- POST 	/session/{session id}/timeouts 	Set Timeouts
setTimeouts :: SessionRef -> Timeouts -> W3Spec ()
setTimeouts sessionRef Timeouts {implicit, pageLoad, script} =
  Post "Set Timeouts" (sessionId1 sessionRef "timeouts") (object ["implicit" .= implicit, "pageLoad" .= pageLoad, "script" .= script]) voidParser

-- POST 	/session/{session id}/url 	Navigate To
navigateTo :: SessionRef -> Text -> W3Spec ()
navigateTo sessionRef url = Post "Navigate To" (sessionId1 sessionRef "url") (object ["url" .= url]) voidParser

-- GET 	/session/{session id}/url 	Get Current URL
getCurrentUrl :: SessionRef -> W3Spec Text
getCurrentUrl sessionRef = Get "Get Current URL" (sessionId1 sessionRef "url") parseValueTxt

-- POST 	/session/{session id}/back 	Back
back :: SessionRef -> W3Spec ()
back sessionRef = PostEmpty "Back" (sessionId1 sessionRef "back") voidParser

-- POST 	/session/{session id}/forward 	Forward
forward :: SessionRef -> W3Spec ()
forward sessionRef = PostEmpty "Forward" (sessionId1 sessionRef "forward") voidParser

-- POST 	/session/{session id}/refresh 	Refresh
refresh :: SessionRef -> W3Spec ()
refresh sessionRef = PostEmpty "Refresh" (sessionId1 sessionRef "refresh") voidParser

-- GET 	/session/{session id}/title 	Get Title
getTitle :: SessionRef -> W3Spec Text
getTitle sessionRef = Get "Get Title" (sessionId1 sessionRef "title") parseValueTxt

-- GET 	/session/{session id}/window 	Get Window Handle
getWindowHandle :: SessionRef -> W3Spec Text
getWindowHandle sessionRef = Get "Get Window Handle" (sessionId1 sessionRef "window") parseValueTxt

-- POST 	/session/{session id}/window/new 	New Window
newWindow :: SessionRef -> W3Spec WindowHandle
newWindow sessionRef = PostEmpty "New Window" (sessionId2 sessionRef "window" "new") windowHandleParser

-- DELETE 	/session/{session id}/window 	Close Window
closeWindow :: SessionRef -> W3Spec ()
closeWindow sessionRef = Delete "Close Window" (sessionId1 sessionRef "window") voidParser

-- POST 	/session/{session id}/window 	Switch To Window
switchToWindow :: SessionRef -> Text -> W3Spec ()
switchToWindow sessionRef handle = Post "Switch To Window" (sessionId1 sessionRef "window") (object ["handle" .= handle]) voidParser

-- GET 	/session/{session id}/window/handles 	Get Window Handles
getWindowHandles :: SessionRef -> W3Spec [Text]
getWindowHandles sessionRef = Get "Get Window Handles" (sessionId2 sessionRef "window" "handles") windowHandlesParser

-- POST 	/session/{session id}/frame 	Switch To Frame
-- POST 	/session/{session id}/frame/parent 	Switch To Parent Frame
-- GET 	/session/{session id}/window/rect 	Get Window Rect
-- POST 	/session/{session id}/window/rect 	Set Window Rect

-- POST 	/session/{session id}/window/maximize 	Maximize
maximizeWindow :: SessionRef -> W3Spec ()
maximizeWindow sessionRef = PostEmpty "Maximize Window" (window1 sessionRef "maximize") voidParser

-- POST 	/session/{session id}/window/minimize 	Minimize Window
minimizeWindow :: SessionRef -> W3Spec ()
minimizeWindow sessionRef = PostEmpty "Minimize Window" (window1 sessionRef "minimize") voidParser

-- POST 	/session/{session id}/window/fullscreen 	Fullscreen Window
fullscreenWindow :: SessionRef -> W3Spec ()
fullscreenWindow sessionRef = PostEmpty "Fullscreen Window" (window1 sessionRef "fullscreen") voidParser

-- GET 	/session/{session id}/element/active 	Get Active Element
-- GET 	/session/{session id}/element/{element id}/shadow 	Get Element Shadow Root

-- POST 	/session/{session id}/element 	Find Element
findElement :: SessionRef -> Selector -> W3Spec ElementRef
findElement sessionRef = findElement' sessionRef . selectorJson

-- POST 	/session/{session id}/elements 	Find Elements
-- POST 	/session/{session id}/element/{element id}/element 	Find Element From Element
-- POST 	/session/{session id}/element/{element id}/elements 	Find Elements From Element
-- POST 	/session/{session id}/shadow/{shadow id}/element 	Find Element From Shadow Root
-- POST 	/session/{session id}/shadow/{shadow id}/elements 	Find Elements From Shadow Root
-- GET 	/session/{session id}/element/{element id}/selected 	Is Element Selected
-- GET 	/session/{session id}/element/{element id}/attribute/{name} 	Get Element Attribute
-- GET 	/session/{session id}/element/{element id}/property/{name} 	Get Element Property
-- GET 	/session/{session id}/element/{element id}/css/{property name} 	Get Element CSS Value

-- GET 	/session/{session id}/element/{element id}/text 	Get Element Text
elementText :: SessionRef -> ElementRef -> W3Spec Text
elementText sessionRef elementRef = Get "Get Element Text" (element1 sessionRef elementRef "text") parseElmText

-- GET 	/session/{session id}/element/{element id}/name 	Get Element Tag Name
-- GET 	/session/{session id}/element/{element id}/rect 	Get Element Rect
-- GET 	/session/{session id}/element/{element id}/enabled 	Is Element Enabled
-- GET 	/session/{session id}/element/{element id}/computedrole 	Get Computed Role
-- GET 	/session/{session id}/element/{element id}/computedlabel 	Get Computed Label

-- POST 	/session/{session id}/element/{element id}/click 	Element Click
click :: SessionRef -> ElementRef -> W3Spec ()
click sessionRef elementRef = PostEmpty "Click Element" (element1 sessionRef elementRef "click") voidParser

-- POST 	/session/{session id}/element/{element id}/clear 	Element Clear
-- POST 	/session/{session id}/element/{element id}/value 	Element Send Keys
-- GET 	/session/{session id}/source 	Get Page Source
-- POST 	/session/{session id}/execute/sync 	Execute Script
-- POST 	/session/{session id}/execute/async 	Execute Async Script
-- GET 	/session/{session id}/cookie 	Get All Cookies
-- GET 	/session/{session id}/cookie/{name} 	Get Named Cookie
-- POST 	/session/{session id}/cookie 	Add Cookie
-- DELETE 	/session/{session id}/cookie/{name} 	Delete Cookie
-- DELETE 	/session/{session id}/cookie 	Delete All Cookies
-- POST 	/session/{session id}/actions 	Perform Actions
-- DELETE 	/session/{session id}/actions 	Release Actions
-- POST 	/session/{session id}/alert/dismiss 	Dismiss Alert
-- POST 	/session/{session id}/alert/accept 	Accept Alert
-- GET 	/session/{session id}/alert/text 	Get Alert Text
-- POST 	/session/{session id}/alert/text 	Send Alert Text
-- GET 	/session/{session id}/screenshot 	Take Screenshot
-- GET 	/session/{session id}/element/{element id}/screenshot 	Take Element Screenshot
-- POST 	/session/{session id}/print 	Print Page

findElement' :: SessionRef -> Value -> W3Spec ElementRef
findElement' sessionRef selector = Post "Find Element" (sessionId1 sessionRef "element") selector parseElementRef

-- #### Utils ####

data Timeouts = Timeouts
  { implicit :: Int,
    pageLoad :: Int,
    script :: Int
  }
  deriving (Show)

  {-
  Response
  { statusCode = 200
  , statusMessage = "OK"
  , body =
      Object
        (fromList
           [ ( "value"
             , Array
                 [ String "38573ea0-0402-4845-9186-54ad3dc874b1"
                 , String "f145ab04-c1ab-48d1-8c62-1f73d43650d7"
                 ]
             )
           ])
  }
  
  
  
  -}

windowHandleParser :: HttpResponse -> Maybe WindowHandle
windowHandleParser r =
  lookup "value" r.body
    >>= windowHandleFromValue

windowHandlesParser :: HttpResponse -> Maybe [Text]
windowHandlesParser r = do
  lookup "value" r.body
    >>= \case
      Array a -> Just $ catMaybes $ toList $ asText <$> a
      _ -> Nothing

windowHandleFromValue :: Value -> Maybe WindowHandle
windowHandleFromValue v =
  Handle
    <$> ( lookup "handle" v
            >>= asText
        )
    <*> ( lookup "type" v
            >>= asText
        )

parseTimeouts :: HttpResponse -> Maybe Timeouts
parseTimeouts r =
  Timeouts
    <$> ( lookup "value" r.body
            >>= lookup "implicit"
            >>= asInt
        )
    <*> ( lookup "value" r.body
            >>= lookup "pageLoad"
            >>= asInt
        )
    <*> ( lookup "value" r.body
            >>= lookup "script"
            >>= asInt
        )

selectorJson :: Selector -> Value
selectorJson = \case
  CSS css -> object ["using" .= ("css selector" :: Text), "value" .= css]

voidParser :: HttpResponse -> Maybe ()
voidParser _ = Just ()

parseValueTxt :: HttpResponse -> Maybe Text
parseValueTxt r =
  lookup "value" r.body
    >>= asText

-- TODO Ason helpers separate module
lookup :: Key -> Value -> Maybe Value
lookup k = \case
  Object o -> AKM.lookup k o
  _ -> Nothing

asText :: Value -> Maybe Text
asText = \case
  String t -> Just t
  _ -> Nothing

asInt :: Value -> Maybe Int
asInt = \case
  Number t -> Just $ floor t
  _ -> Nothing

parseSessionRef :: HttpResponse -> Maybe SessionRef
parseSessionRef r =
  Session
    <$> ( lookup "value" r.body
            >>= lookup "sessionId"
            >>= asText
        )

parseElmText :: HttpResponse -> Maybe Text
parseElmText r =
  lookup "value" r.body
    >>= asText

parseElementRef :: HttpResponse -> Maybe ElementRef
parseElementRef r =
  Element
    <$> ( lookup "value" r.body
            -- very strange choice for prop name - in response and sane as webdriver-w3c
            >>= lookup "element-6066-11e4-a52e-4f735466cecf"
            >>= asText
        )

session :: Text
session = "session"

session1 :: Text -> [Text]
session1 sp = [session, sp]

sessionId1 :: SessionRef -> Text -> [Text]
sessionId1 sr sp = [session, sr.id, sp]

sessionId2 :: SessionRef -> Text -> Text -> [Text]
sessionId2 sr sp sp2 = [session, sr.id, sp, sp2]

window :: Text
window = "window"

window1 :: SessionRef -> Text -> [Text]
window1 sr sp = [session, sr.id, window, sp]

element1 :: SessionRef -> ElementRef -> Text -> [Text]
element1 sr er sp = [session, sr.id, "element", er.id, sp]

mkShowable :: W3Spec a -> W3SpecShowable
mkShowable = \case
  Get d p _ -> Request d "GET" p Nothing
  Post d p b _ -> Request d "POST" p (Just b)
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