{-# LANGUAGE UndecidableInstances #-}

module WebDriverPure
  ( RequestArgs (..),
    capsToJson,
    defaultRequest,
    second,
    seconds,  
    minute, 
    minutes,
    hour,
    hours
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
import Prelude hiding (get, second)

{- Pure types and functions used in Webdriver -}


second :: Int
second = 1_000

seconds :: Int
seconds = second

minute :: Int
minute = 60 * seconds

minutes :: Int
minutes = minute

hour :: Int
hour = 60 * minutes

hours :: Int
hours = hour

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

defaultRequest :: RequestArgs
defaultRequest = RequestParams [] GET NoReqBody 4444

capsToJson :: Capabilities -> Value
capsToJson caps =
  object
    [ "capabilities"
        .= object
          ["alwaysMatch" .= toJSON caps],
      "desiredCapabilities" .= toJSON caps
    ]




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




-- Aeson stuff to help debugging
-- https://blog.ssanj.net/posts/2019-09-24-pretty-printing-json-in-haskell.html
-- lsbToText :: LBS.ByteString -> Text
-- lsbToText = E.decodeUtf8 . LBS.toStrict

-- jsonToText :: Value -> Text
-- jsonToText = lsbToText . encodePretty

-- prettyPrint :: Value -> IO ()
-- prettyPrint = T.putStrLn . jsonToText



