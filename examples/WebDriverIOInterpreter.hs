module WebDriverIOInterpreter where

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
import WebDriverPure

type MyWebDriver eff a = WebDriverT (Eff eff) a

runWebDriverIO' :: forall es a. ( IOE :> es) => Eff (WebUI : es) a  -> Eff es a
runWebDriverIO' =
  interpret $ \_ ->
    EF.liftIO . \case
      NewSession -> uu
      KillSession sessionId -> uu
      Click sessionId css -> uu
      Go sessionId url -> uu -- navigateTo url
      Sleep ms -> uu -- wait i
      Read sessionId css -> uu -- findElement CssSelector css >>= getElementText)

-- $ > driverRunning
driverRunning :: IO Bool
driverRunning = responseCode200 <$> handleEx status

-- mkRequestParams :: HttpPathSpec -> RequestArgs
-- mkRequestParams ps = RequestParams subDirs method body port

-- >>> status
-- Response {statusCode = 200, statusMessage = "OK", headers = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","37"),("date","Sat, 31 Aug 2024 08:16:30 GMT")], body = Object (fromList [("value",Object (fromList [("message",String ""),("ready",Bool True)]))]), cookies = CJ {expose = []}}
status :: IO HttpResponse
status = get pathStatus

-- >>> newFirefoxSession
-- "Just MkSessionId { id = \"238b3b38-96de-41e1-8f90-04bd2136e579\" }"
newFirefoxSession :: IO (Maybe SessionId)
newFirefoxSession = do
  r <- newSession defaultFirefoxCapabilities
  T.putStrLn $ txt r
  pure r


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


get :: HttpPathSpec GET -> IO HttpResponse
get s = request $ defaultRequest {subDirs = s.subDirs}

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


post :: [Text] -> Value -> IO HttpResponse
post subDirs jsonBody =
  request
    defaultRequest
      { subDirs,
        method = POST,
        body = ReqBodyJson jsonBody
      }


