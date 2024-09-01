{-# LANGUAGE UndecidableInstances #-}

module WebDriverPure
  ( RequestArgs (..),
    HttpPathSpec (..),
    HttpResponse (..),
    responseCode200,
    capsToJson,
    pathStatus,
    pathDeleteSession,
    pathNewSession,
    parseSessionId,
    defaultRequest,
    Log (..),
    pathNavigateTo,
    pathClick,
    pathElementText,
    ElementRef (..),
    pathFindElement,
    parseElementRef,
    parseElmText,
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
import PyrethrumExtras (getLenient, toS, txt, uu)
import UnliftIO (try)
import Web.Api.WebDriver (Capabilities, WebDriverT, defaultFirefoxCapabilities)
import WebDriverEffect (SessionId (..), WebUI (..))
import Prelude hiding (get)

{- Pure types and functions used in Webdriver -}

-- todo stand alone instance of Show
data RequestArgs where
  RequestParams ::
    (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
    { subDirs :: [Text],
      method :: method,
      body :: body,
      port :: Int
    } ->
    RequestArgs


-- add Get | Post {path , body , later errorParser} | Delete {path }
-- todo stand alone instance of Show
newtype HttpPathSpec method
  = PathSpec {subDirs :: [Text]}

data HttpResponse = Response
  { statusCode :: Int,
    statusMessage :: Text,
    headers :: L.ResponseHeaders,
    body :: Value,
    cookies :: L.CookieJar
  }
  deriving (Show)

newtype ElementRef = MkElementRef {id :: Text}
  deriving (Show, Eq)

-- $ > driverRunning

responseCode200 :: Either a HttpResponse -> Bool
responseCode200 = either (const False) ((==) 200 . (.statusCode))

capsToJson :: Capabilities -> Value
capsToJson caps =
  object
    [ "capabilities"
        .= object
          ["alwaysMatch" .= toJSON caps],
      "desiredCapabilities" .= toJSON caps
    ]

get :: [Text] -> HttpPathSpec GET
get = PathSpec

delete :: [Text] -> HttpPathSpec DELETE
delete = PathSpec

post :: [Text] -> HttpPathSpec POST
post = PathSpec

session' :: Text
session' = "session"

session :: SessionId -> [Text]
session s = [session', s.id]

element :: SessionId -> [Text]
element s = [session', s.id, "element"]

element1 :: SessionId -> ElementRef -> Text -> [Text]
element1 s r p = [session', s.id, "element", r.id, p]

session1 :: SessionId -> Text -> [Text]
session1 s p = [session', s.id, p]

postSession1 :: SessionId -> Text -> HttpPathSpec POST
postSession1 s = post . session1 s

postElement1 :: SessionId -> ElementRef -> Text -> HttpPathSpec POST
postElement1 s r = post . element1 s r

getElement1 :: SessionId -> ElementRef -> Text -> HttpPathSpec GET
getElement1 s r = get . element1 s r

{-
Method 	URI Template 	Command
GET 	/status 	Status
-}
pathStatus :: HttpPathSpec GET
pathStatus = get ["status"]

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

pathElementText :: SessionId -> ElementRef -> HttpPathSpec GET
pathElementText s r = getElement1 s r "text"

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

{- POST 	/session 	New Session -}
pathNewSession :: HttpPathSpec POST
pathNewSession = post [session']

{-
POST 	/session/{session id}/timeouts 	Set Timeouts
POST 	/session/{session id}/url 	Navigate To
-}
pathNavigateTo :: SessionId -> HttpPathSpec POST
pathNavigateTo s = postSession1 s "url"

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
pathFindElement :: SessionId -> HttpPathSpec POST
pathFindElement = post . element

{-
POST 	/session/{session id}/elements 	Find Elements
POST 	/session/{session id}/element/{element id}/element 	Find Element From Element
POST 	/session/{session id}/element/{element id}/elements 	Find Elements From Element
POST 	/session/{session id}/shadow/{shadow id}/element 	Find Element From Shadow Root
POST 	/session/{session id}/shadow/{shadow id}/elements 	Find Elements From Shadow Root
-}

-- POST 	/session/{session id}/element/{element id}/click 	Element Click
pathClick :: SessionId -> ElementRef -> HttpPathSpec POST
pathClick s r = postElement1 s r "click"

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
pathDeleteSession :: SessionId -> HttpPathSpec DELETE
pathDeleteSession s = delete $ session s

{-
DELETE /session/{session id}/window 	Close Window
DELETE /session/{session id}/cookie/{name} 	Delete Cookie
DELETE /session/{session id}/cookie 	Delete All Cookies
DELETE /session/{session id}/actions 	Release Actions
-}

-- TODO Ason helpers separate module
lookup :: Key -> Value -> Maybe Value
lookup k = \case
  Object o -> AKM.lookup k o
  _ -> Nothing

asText :: Value -> Maybe Text
asText = \case
  String t -> Just t
  _ -> Nothing

parseSessionId :: HttpResponse -> Maybe SessionId
parseSessionId r =
  Session
    <$> ( lookup "value" r.body
            >>= lookup "sessionId"
            >>= asText
        )

parseElmText :: HttpResponse -> Maybe Text
parseElmText r =
  lookup "value" r.body
    >>= asText

--  Probably wrong
parseElementRef :: HttpResponse -> Maybe ElementRef
parseElementRef r =
  MkElementRef
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

defaultRequest :: RequestArgs
defaultRequest = RequestParams [] GET NoReqBody 4444

data Log
  = Desc Text
  | None
  deriving (Show)
