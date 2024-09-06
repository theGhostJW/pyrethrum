{-# LANGUAGE UndecidableInstances #-}

module WebDriverSpec
  ( W3Spec (..),
    HttpResponse (..),
    ElementRef (..),
    SessionRef (..),
    DriverStatus (..),
    Selector (..),
    -- Capabilities(..),
    mkShowable,
    --- Specs
    statusSpec,
    maximizeWindowSpec,
    minimizeWindowSpec,
    fullscreenWindowSpec,
    -- newSessionSpec,
    newSessionSpec',
    deleteSessionSpec,
    navigateToSpec,
    findElementSpec,
    findElementSpec',
    clickSpec,
    elementTextSpec,
  )
where
import Data.Aeson
  ( Key,
    KeyValue ((.=)),
    Value (Object, String),
    object,
  )
import Data.Aeson.KeyMap qualified as AKM
import Network.HTTP.Client qualified as NC
import Network.HTTP.Types qualified as NT
import Prelude hiding (get)

{- Pure types and functions used in Webdriver -}

--  TODO: add error handler
data W3Spec a
  = Get
      { description :: ~Text,
        path :: [Text],
        parser :: HttpResponse -> Maybe a
      }
  | Post
      { description :: ~Text,
        path :: [Text],
        body :: Value,
        parser :: HttpResponse -> Maybe a
      }
  | PostEmpty
      { description :: ~Text,
        path :: [Text],
        parser :: HttpResponse -> Maybe a
      }
  | Delete
      { description :: ~Text,
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

mkShowable :: W3Spec a -> W3SpecShowable
mkShowable = \case
  Get d p _ -> Request d "GET" p Nothing
  Post d p b _ -> Request d "POST" p (Just b)
  PostEmpty d p _ -> Request d "POST" p Nothing
  Delete d p _ -> Request d "DELETE" p Nothing

data HttpResponse = Response
  { statusCode :: Int,
    statusMessage :: Text,
    headers :: NT.ResponseHeaders,
    body :: Value,
    cookies :: NC.CookieJar
  }
  deriving (Show)

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

parseDriverStatus :: HttpResponse -> Maybe DriverStatus
parseDriverStatus Response {statusCode, statusMessage} =
  Just $
    statusCode & \case
      200 -> Ready
      500 -> ServiceError {statusCode, statusMessage}
      501 -> Running
      _ -> Unknown {statusCode, statusMessage}

-- TODO: add more selector types
newtype Selector = CSS Text
  deriving (Show)

-- TODO capabilities for all browsers - to and from JSON
-- move to separate module
data Capabilities = MkCapabilities
  {
  }
  -- NOT IMPLEMENTED
  -- browserName :: Text,
  -- browserVersion :: Text,
  -- platformName :: Text

  deriving (Show)

{-
-- TODO: capabilities type
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

session :: Text
session = "session"

session1 :: Text -> [Text]
session1 sp = [session, sp]

sessionId1 :: SessionRef -> Text -> [Text]
sessionId1 sr sp = [session, sr.id, sp]

window :: Text
window = "window"

window1 :: SessionRef -> Text -> [Text]
window1 sr sp = [session, sr.id, window, sp]

element1 :: SessionRef -> ElementRef -> Text -> [Text]
element1 sr er sp = [session, sr.id, "element", er.id, sp]

-- https://www.w3.org/TR/2024/WD-webdriver2-20240723/
{-
Method 	URI Template 	Command
GET 	/status 	Status
-}
statusSpec :: W3Spec DriverStatus
statusSpec = Get "Get Driver Status" ["status"] parseDriverStatus

{-
GET 	/session/{session id}/timeouts 	Get Timeouts
GET 	/session/{session id}/url 	Get Current URL
GET 	/session/{session id}/title 	Get Title
GET 	/session/{session id}/window 	Get Window Handle
GET 	/session/{session id}/window/handles 	Get Window Handles
GET 	/session/{session id}/window/rect 	Get Window Rect
GET 	/session/{session id}/element/active 	Get Active Element
GET 	/session/{session id}/element/{element id}/shadow 	Get Element Shadow Root
GET 	/session/{session id}/element/{element id}/selected 	Is Element Selected
GET 	/session/{session id}/element/{element id}/attribute/{name} 	Get Element Attribute
-}

-- GET 	/session/{session id}/element/{element id}/text 	Get Element Text
elementTextSpec :: SessionRef -> ElementRef -> W3Spec Text
elementTextSpec sessionRef elementRef = Get "Get Element Text" (element1 sessionRef elementRef "text") parseElmText

{-
GET 	/session/{session id}/element/{element id}/property/{name} 	Get Element Property
GET 	/session/{session id}/element/{element id}/css/{property name} 	Get Element CSS Value
GET 	/session/{session id}/element/{element id}/name 	Get Element Tag Name
GET 	/session/{session id}/element/{element id}/rect 	Get Element Rect
GET 	/session/{session id}/element/{element id}/enabled 	Is Element Enabled
GET 	/session/{session id}/element/{element id}/computedrole 	Get Computed Role
GET 	/session/{session id}/element/{element id}/computedlabel 	Get Computed Label
GET 	/session/{session id}/source 	Get Page Source
GET 	/session/{session id}/cookie 	Get All Cookies
GET 	/session/{session id}/alert/text 	Get Alert Text
GET 	/session/{session id}/cookie/{name} 	Get Named Cookie
GET 	/session/{session id}/screenshot 	Take Screenshot
GET 	/session/{session id}/element/{element id}/screenshot 	Take Element Screenshot
-}

-- POST 	/session 	New Session
-- TODO: native capabilities type
-- newSessionSpec :: Capabilities -> W3Spec SessionRef
-- newSessionSpec capabilities = newSessionSpec' $ capsToJson capabilities

newSessionSpec' :: Value -> W3Spec SessionRef
newSessionSpec' capabilities = Post "Create New Session" [session] capabilities parseSessionRef

{-
POST 	/session/{session id}/timeouts 	Set Timeouts
-}

-- POST 	/session/{session id}/url 	Navigate To
navigateToSpec :: SessionRef -> Text -> W3Spec ()
navigateToSpec sessionRef url = Post "Navigate To" (sessionId1 sessionRef "url") (object ["url" .= url]) voidParser

{-
POST 	/session/{session id}/back 	Back
POST 	/session/{session id}/forward 	Forward
POST 	/session/{session id}/refresh 	Refresh
POST 	/session/{session id}/window 	Switch To Window
POST 	/session/{session id}/window/new 	New Window
POST 	/session/{session id}/frame 	Switch To Frame
POST 	/session/{session id}/frame/parent 	Switch To Parent Frame
POST 	/session/{session id}/window/rect 	Set Window Rect
-}

-- POST 	/session/{session id}/window/maximize 	Maximize Window
maximizeWindowSpec :: SessionRef -> W3Spec ()
maximizeWindowSpec sessionRef = PostEmpty "Maximize Window" (window1 sessionRef "maximize") voidParser

-- POST 	/session/{session id}/window/minimize 	Minimize Window
minimizeWindowSpec :: SessionRef -> W3Spec ()
minimizeWindowSpec sessionRef = PostEmpty "Minimize Window" (window1 sessionRef "minimize") voidParser

-- POST 	/session/{session id}/window/fullscreen 	Fullscreen Window
fullscreenWindowSpec :: SessionRef -> W3Spec ()
fullscreenWindowSpec sessionRef = PostEmpty "Fullscreen Window" (window1 sessionRef "fullscreen") voidParser

-- POST 	/session/{session id}/element 	Find Element
findElementSpec :: SessionRef -> Selector -> W3Spec ElementRef
findElementSpec sessionRef = findElementSpec' sessionRef . selectorJson

findElementSpec' :: SessionRef -> Value -> W3Spec ElementRef
findElementSpec' sessionRef selector = Post "Find Element" (sessionId1 sessionRef "element") selector parseElementRef

{-
POST 	/session/{session id}/elements 	Find Elements
POST 	/session/{session id}/element/{element id}/element 	Find Element From Element
POST 	/session/{session id}/element/{element id}/elements 	Find Elements From Element
POST 	/session/{session id}/shadow/{shadow id}/element 	Find Element From Shadow Root
POST 	/session/{session id}/shadow/{shadow id}/elements 	Find Elements From Shadow Root
-}

-- POST 	/session/{session id}/element/{element id}/click 	Element Click
clickSpec :: SessionRef -> ElementRef -> W3Spec ()
clickSpec sessionRef elementRef = PostEmpty "Click Element" (element1 sessionRef elementRef "click") voidParser

{-
POST 	/session/{session id}/element/{element id}/clear 	Element Clear
POST 	/session/{session id}/element/{element id}/value 	Element Send Keys
POST 	/session/{session id}/execute/sync 	Execute Script
POST 	/session/{session id}/execute/async 	Execute Async Script
POST 	/session/{session id}/cookie 	Add Cookie
POST 	/session/{session id}/actions 	Perform Actions
POST 	/session/{session id}/alert/dismiss 	Dismiss Alert
POST 	/session/{session id}/alert/accept 	Accept Alert
POST 	/session/{session id}/alert/text 	Send Alert Text
POST 	/session/{session id}/print 	Print Page
-}

-- DELETE /session/{session id} 	Delete Session
deleteSessionSpec :: SessionRef -> W3Spec ()
deleteSessionSpec sessionRef = Delete "Delete Session" (session1 sessionRef.id) voidParser

{-
DELETE /session/{session id}/window 	Close Window
DELETE /session/{session id}/cookie/{name} 	Delete Cookie
DELETE /session/{session id}/cookie 	Delete All Cookies
DELETE /session/{session id}/actions 	Release Actions
-}

-- #### Utils ####

selectorJson :: Selector -> Value
selectorJson = \case
  CSS css -> object ["using" .= ("css selector" :: Text), "value" .= css]

voidParser :: HttpResponse -> Maybe ()
voidParser _ = Just ()

-- TODO Ason helpers separate module
lookup :: Key -> Value -> Maybe Value
lookup k = \case
  Object o -> AKM.lookup k o
  _ -> Nothing

asText :: Value -> Maybe Text
asText = \case
  String t -> Just t
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