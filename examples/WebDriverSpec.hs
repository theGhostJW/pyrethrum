{-# LANGUAGE UndecidableInstances #-}

module WebDriverSpec
  ( W3Spec (..),
    HttpResponse (..),
    ElementRef (..),
    SessionRef (..),
    DriverStatus (..),
    Selector(..),
    mkShowable,

    --- Specs
    statusSpec,
    newSessionSpec,
    newSessionSpec',
    deleteSessionSpec,
    navigateToSpec,
    findElementSpec,
    findElementSpec'
    -- responseCode200,
    -- capsToJson,
    -- pathStatus,
    -- pathDeleteSession,
    -- pathNewSession,
    -- parseSessionRef,
    -- defaultRequest,
    -- Log (..),
    -- pathNavigateTo,
    -- pathClick,
    -- pathElementText,
    -- pathFindElement,
    -- parseElementRef,
    -- parseElmText,
  )
where

-- import Effectful.Reader.Dynamic

import Core (Node (path))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as T
import Effectful as EF
  ( Eff,
    IOE,
    liftIO,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( interpret,
  )
-- import Network.HTTP.Client qualified as L
-- import Network.HTTP.Req as R
--   ( DELETE (DELETE),
--     GET (GET),
--     HttpBody,
--     HttpBodyAllowed,
--     HttpException,
--     HttpMethod (AllowsBody),
--     NoReqBody (NoReqBody),
--     POST (POST),
--     ProvidesBody,
--     ReqBodyJson (ReqBodyJson),
--     Scheme (Http),
--     Url,
--     defaultHttpConfig,
--     http,
--     jsonResponse,
--     port,
--     req,
--     responseBody,
--     responseCookieJar,
--     responseStatusCode,
--     responseStatusMessage,
--     runReq,
--     toVanillaResponse,
--     (/:),
--   )

import Network.HTTP.Client qualified as NC
import Network.HTTP.Types qualified as NT
import PyrethrumExtras (getLenient, toS, txt, uu)
import UnliftIO (try)
import Prelude hiding (get)

{- Pure types and functions used in Webdriver -}

--  todo: add error handler
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

data W3SpecShowable = Request {
  description :: Text,
  method :: Text,
  path :: [Text],
  body :: Maybe Value
} deriving (Show)

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
  deriving (Show)

parseDriverStatus :: HttpResponse -> Maybe DriverStatus
parseDriverStatus Response {statusCode, statusMessage} =
  Just $ statusCode & \case
    200 -> Ready
    500 -> ServiceError {statusCode, statusMessage}
    501 -> Running
    _ -> Unknown {statusCode, statusMessage}


-- TODO: add more selector types
newtype Selector = CSS Text 
  deriving (Show) 

-- TODO capabilities for all browsers - to and from JSON
data Capabilities = MkCapabilities
  { 
    -- NOT IMPLEMENTED
    -- browserName :: Text,
    -- browserVersion :: Text,
    -- platformName :: Text
  }
  deriving (Show)

capsToJson :: Capabilities -> Value
capsToJson caps = uu
  -- object
  --   [ "capabilities"
  --       .= object
  --         ["alwaysMatch" .= toJSON caps],
  --     "desiredCapabilities" .= toJSON caps
  --   ]

{-


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
GET 	/session/{session id}/element/{element id}/text 	Get Element Text
-}

-- pathElementText :: SessionRef -> ElementRef -> HttpPathSpec GET
-- pathElementText s r = getElement1 s r "text"

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

-- TODO: capabilities type
-- Change response to 
-- POST 	/session 	New Session
newSessionSpec :: Capabilities -> W3Spec SessionRef
newSessionSpec capabilities = newSessionSpec' $ capsToJson capabilities

newSessionSpec' :: Value -> W3Spec SessionRef
newSessionSpec' capabilities = Post "Create New Session" [session] capabilities parseSessionRef


{-
POST 	/session/{session id}/timeouts 	Set Timeouts
POST 	/session/{session id}/url 	Navigate To
-}

-- pathNavigateTo :: SessionRef -> HttpPathSpec POST
-- pathNavigateTo s = postSession1 s "url"

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
POST 	/session/{session id}/window/maximize 	Maximize Window
POST 	/session/{session id}/window/minimize 	Minimize Window
POST 	/session/{session id}/window/fullscreen 	Fullscreen Window
-}

-- POST 	/session/{session id}/element 	Find Element
findElementSpec :: SessionRef -> Selector -> W3Spec ElementRef
findElementSpec sessionRef = findElementSpec' sessionRef . selectorVal 

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
-- pathClick :: SessionRef -> ElementRef -> HttpPathSpec POST
-- pathClick s r = postElement1 s r "click"

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
{-
DELETE /session/{session id} 	Delete Session
-}


deleteSessionSpec :: SessionRef -> W3Spec ()
deleteSessionSpec sessionRef = Delete "Delete Session" (session1 sessionRef.id) voidParser

{-
DELETE /session/{session id}/window 	Close Window
DELETE /session/{session id}/cookie/{name} 	Delete Cookie
DELETE /session/{session id}/cookie 	Delete All Cookies
DELETE /session/{session id}/actions 	Release Actions
-}

selectorVal :: Selector -> Value
selectorVal = \case
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

-- Aeson stuff to help debugging
-- https://blog.ssanj.net/posts/2019-09-24-pretty-printing-json-in-haskell.html
-- lsbToText :: LBS.ByteString -> Text
-- lsbToText = E.decodeUtf8 . LBS.toStrict

-- jsonToText :: Value -> Text
-- jsonToText = lsbToText . encodePretty

-- prettyPrint :: Value -> IO ()
-- prettyPrint = T.putStrLn . jsonToText