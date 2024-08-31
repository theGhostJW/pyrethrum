{-# LANGUAGE UndecidableInstances #-}

module WebDriverPure (
 RequestArgs(..),
HttpPathSpec(..),
HttpResponse(..),
responseCode200,
capsToJson,
pathStatus,
pathKillSession,
pathNewSession,
parseSessionId,
defaultRequest,
Log(..)
)
where

import Data.Aeson
import Data.Text.IO qualified as T
import Effectful as EF
  ( Eff,
    IOE,
    liftIO,
    type (:>),
  )
-- import Effectful.Reader.Dynamic
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
import WebDriverEffect (WebUI (..), SessionId(..))
import Prelude hiding (get)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.KeyMap qualified as AKM 
import Data.Aeson.Types (parseMaybe)

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

-- todo stand alone instance of Show
data HttpPathSpec method =
 PathSpec {
      method :: method,
      path :: [Text]
    }

data HttpResponse = Response
  { statusCode :: Int,
    statusMessage :: Text,
    headers :: L.ResponseHeaders,
    body :: Value,
    cookies :: L.CookieJar
  }
  deriving (Show)

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
get = PathSpec GET
get1 p1 = get [p1]
get2 p1 p2 = get [p1, p2]

delete :: [Text] -> HttpPathSpec DELETE
delete = PathSpec DELETE
delete1 p1 = delete [p1]
delete2 p1 p2 = delete [p1, p2]

post :: [Text] -> HttpPathSpec POST
post = PathSpec POST

post1 p1 = post [p1]
post2 p1 p2 = post [p1, p2]

pathStatus :: HttpPathSpec GET
pathStatus = get1 "status"

pathKillSession :: SessionId -> HttpPathSpec DELETE
pathKillSession sessionId = delete2 "session" sessionId.id

pathNewSession :: HttpPathSpec POST
pathNewSession = post1 "session"

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
    MkSessionId <$> (lookup "value"  r.body 
                      >>= lookup "sessionId" 
                      >>= asText )

-- Aeson stuff to help debugging
-- https://blog.ssanj.net/posts/2019-09-24-pretty-printing-json-in-haskell.html
lsbToText :: LBS.ByteString -> Text
lsbToText = E.decodeUtf8 . LBS.toStrict

jsonToText :: Value -> Text
jsonToText = lsbToText . encodePretty

prettyPrint :: Value -> IO ()
prettyPrint = T.putStrLn . jsonToText

defaultRequest :: RequestArgs
defaultRequest = RequestParams [] GET NoReqBody 4444

data Log
  = Desc Text
  | None
  deriving (Show)
