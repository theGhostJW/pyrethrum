{-# LANGUAGE UndecidableInstances #-}

module WebDriverRawIO where

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
import PyrethrumExtras (delete, getLenient, toS, txt, uu)
import UnliftIO (try)
import Web.Api.WebDriver (Capabilities, WebDriverT, defaultFirefoxCapabilities)
import WebDriverEffect (WebUI (..), SessionId(..))
import Prelude hiding (get)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.KeyMap (lookup, singleton)
import Data.Aeson.Types (parseMaybe)

type MyWebDriver eff a = WebDriverT (Eff eff) a

-- $ > driverRunning

driverRunning :: IO Bool
driverRunning = either (const False) ((==) 200 . (.statusCode)) <$> handleEx status

capsToJson :: Capabilities -> Value
capsToJson caps =
  object
    [ "capabilities"
        .= object
          ["alwaysMatch" .= toJSON caps],
      "desiredCapabilities" .= toJSON caps
    ]

-- >>> status
-- Response {statusCode = 200, statusMessage = "OK", headers = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","37"),("date","Sat, 31 Aug 2024 08:16:30 GMT")], body = Object (fromList [("value",Object (fromList [("message",String ""),("ready",Bool True)]))]), cookies = CJ {expose = []}}
status :: IO HttpResponse
status = get1 "status"

-- >>> newFirefoxSession
-- "Just MkSessionId { id = \"238b3b38-96de-41e1-8f90-04bd2136e579\" }"
newFirefoxSession :: IO (Maybe SessionId)
newFirefoxSession = do
  r <- newSession defaultFirefoxCapabilities
  T.putStrLn $ txt r
  pure r

parseSessionId :: HttpResponse -> Maybe SessionId
parseSessionId r =
    MkSessionId <$> (rslt >>= asText )
    where
      tryLookUp :: Key -> Value -> Maybe Value
      tryLookUp k = \case
        Object o -> lookup k o
        _ -> Nothing

      asText :: Value -> Maybe Text
      asText = \case
        String t -> Just t
        _ -> Nothing

      rslt :: Maybe Value
      rslt = tryLookUp "value"  r.body >>= tryLookUp "sessionId"

  -- MkSessionId . fromMaybe "" . lookup "sessionId" . fromMaybe "" . lookup "value" . (.body)

-- Aeson stuff to help debugging
-- https://blog.ssanj.net/posts/2019-09-24-pretty-printing-json-in-haskell.html
lsbToText :: LBS.ByteString -> Text
lsbToText = E.decodeUtf8 . LBS.toStrict

jsonToText :: Value -> Text
jsonToText = lsbToText . encodePretty

prettyPrint :: Value -> IO ()
prettyPrint = T.putStrLn . jsonToText

newSession :: Capabilities -> IO (Maybe SessionId)
newSession caps = parseSessionId <$> post1 "session" (capsToJson caps)

-- >>> deleteSession $ MkSessionId "efc5b851-636e-4122-b2f9-018071f96200"
-- Response {statusCode = 200, statusMessage = "OK", headers = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","14"),("date","Sat, 31 Aug 2024 09:19:50 GMT")], body = Object (fromList [("value",Null)]), cookies = CJ {expose = []}}
deleteSession :: SessionId -> IO HttpResponse
deleteSession session = request $ defaultRequest {subDirs = ["session", session.id], method = DELETE}

-- $ > cycleSession

-- >>> cycleSession
cycleSession :: IO ()
cycleSession = do
  mSid <- stub (Desc "New Firefox Session") newFirefoxSession
  stub_ (Desc "Get Status") status
  mSid & maybe (fail "Failed to get session ID") 
       (stub_ (Desc "Delete Session") . deleteSession) 
  stub_ (Desc "Get Status") status

lastSession :: SessionId
lastSession = MkSessionId "238b3b38-96de-41e1-8f90-04bd2136e579"

-- >>> killSession
killSession :: IO ()
killSession = stub_ (Desc "Get Status") $ deleteSession lastSession


-- parseSessionId :: HttpResponse -> Text
-- parseSessionId = fromMaybe "" . lookup "sessionId" . fromMaybe [] . lookup "value" . fromMaybe [] . lookup "capabilities" . body

logging :: Bool
logging = True

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

get :: [Text] -> IO HttpResponse
get subDirs = request $ defaultRequest {subDirs}

get1 :: Text -> IO HttpResponse
get1 = get . pure

get2 :: Text -> Text -> IO HttpResponse
get2 s1 s2 = get [s1, s2]

request :: RequestArgs -> IO HttpResponse
request RequestParams {subDirs, method, body, port = prt} =
  runReq defaultHttpConfig $ do
    r <- req method url body jsonResponse $ port prt
    pure $
      Response
        { statusCode = responseStatusCode r,
          statusMessage = getLenient . toS $ responseStatusMessage r,
          headers = L.responseHeaders . toVanillaResponse $ r,
          body = responseBody r :: Value,
          cookies = responseCookieJar r
        }
  where
    url :: R.Url 'Http
    url = foldl' (/:) (http "127.0.0.1") subDirs

data Log
  = Desc Text
  | None
  deriving (Show)

handleEx :: IO a -> IO (Either HttpException a)
handleEx = try @_ @HttpException

stub_ :: Show a => Log -> IO a -> IO ()
stub_ = (void .) . stub

stub :: Show a => Log -> IO a -> IO a
stub desc action = do
  ethr <- handleEx action
  desc & \case
    Desc d -> do
      T.putStrLn ""
      T.putStrLn d
      ethr
        & either
          ( \e -> do
              T.putStrLn "!!!!!!!!!! REQUEST FAILED !!!!!!!!!!!"
              T.putStrLn (txt e)
              fail $ show e
          )
          ( \r -> do
              T.putStrLn "!!!!!!!!!! REQUEST SUCCEEDED !!!!!!!!!!!"
              T.putStrLn $ txt r
              pure r
          )
    None -> either (fail . show) pure ethr

data HttpResponse = Response
  { statusCode :: Int,
    statusMessage :: Text,
    headers :: L.ResponseHeaders,
    body :: Value,
    cookies :: L.CookieJar
  }
  deriving (Show)

post :: [Text] -> Value -> IO HttpResponse
post subDirs jsonBody =
  request
    defaultRequest
      { subDirs,
        method = POST,
        body = ReqBodyJson jsonBody
      }

post1 :: Text -> Value -> IO HttpResponse
post1 subDir = post [subDir]

post2 :: Text -> Text -> Value -> IO HttpResponse
post2 s1 s2 = post [s1, s2]

{-
-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#new-session>. This generalizes `newSession'` by taking an additional function @Value -> Value@ that is applied to the `Capabilities` parameter after it is converted to JSON, but before it is passed to the HTTP call.
newSession'
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (Value -> Value)
  -> Capabilities
  -> WebDriverTT t eff SessionId
newSession' f caps = do
  baseUrl <- theRemoteUrl
  format <- fromEnv (_responseFormat . _env)
  let
    !payload = encode $ f $ object
      [ "capabilities" .= object
        [ "alwaysMatch" .= toJSON caps ]
      , "desiredCapabilities" .= toJSON caps
      ]
  httpPost (baseUrl <> "/session") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= case format of
          SpecFormat -> lookupKeyJson "value"
          ChromeFormat -> return
    >>= lookupKeyJson "sessionId"
    >>= constructFromJson
-}

-- curl -I http://127.0.0.1:4444/status

-- /session/{session id}/window/fullscreen
