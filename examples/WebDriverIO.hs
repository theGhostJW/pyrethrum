{-# LANGUAGE UndecidableInstances #-}

module WebDriverIO
  ( status,
    maximizeWindow,
    minimizeWindow,
    fullscreenWindow,
    newSession,
    newDefaultFirefoxSession,
    deleteSession,
    navigateTo,
    findElement,
    click,
    elementText,
    sleepMilliSecs
  )
where

-- import Effectful.Reader.Dynamic

import Data.Aeson (Value, object)
import Data.Text.IO qualified as T
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Scheme (Http),
    Url,
    defaultHttpConfig,
    http,
    jsonResponse,
    port,
    req,
    responseBody,
    responseStatusCode,
    responseStatusMessage,
    runReq,
    (/:),
  )
import PyrethrumExtras (getLenient, toS, txt)
-- import UnliftIO (try)
-- TODO deprecate
import Web.Api.WebDriver (Capabilities, defaultFirefoxCapabilities)
import WebDriverPure (capsToJson, RequestArgs (..))
import WebDriverSpec qualified as W
import WebDriverSpec (DriverStatus,SessionRef, Selector, ElementRef, W3Spec(..), HttpResponse (..))

import Prelude hiding (get, second)
import UnliftIO.Concurrent (threadDelay)

-- ############# IO Implementation #############

status :: IO DriverStatus
status = run W.status

maximizeWindow :: SessionRef -> IO ()
maximizeWindow = run . W.maximizeWindow

minimizeWindow :: SessionRef -> IO ()
minimizeWindow = run . W.minimizeWindow

fullscreenWindow :: SessionRef -> IO ()
fullscreenWindow = run . W.fullscreenWindow

newSession :: Capabilities -> IO SessionRef
newSession = run . W.newSession' . capsToJson

newDefaultFirefoxSession :: IO SessionRef
newDefaultFirefoxSession = newSession defaultFirefoxCapabilities

deleteSession :: SessionRef -> IO ()
deleteSession = run . W.deleteSession

navigateTo :: SessionRef -> Text -> IO ()
navigateTo s = run . W.navigateTo s

findElement :: SessionRef -> Selector -> IO ElementRef
findElement s = run . W.findElement s

click :: SessionRef -> ElementRef -> IO ()
click s = run . W.click s

elementText :: SessionRef -> ElementRef -> IO Text
elementText s = run . W.elementText s

-- ############# Utils #############

sleepMilliSecs :: Int -> IO ()
sleepMilliSecs = threadDelay . (* 1_000)

-- no console out for "production"
run :: W3Spec a -> IO a
run spec = callWebDriver False (mkRequest spec) >>= parseIO spec

-- TODO: will neeed to be parameterised later
mkRequest :: forall a. W3Spec a -> RequestArgs
mkRequest = \case
  Get {path} -> RequestParams path GET NoReqBody 4444
  Post {path, body} -> RequestParams path POST (ReqBodyJson body) 4444
  PostEmpty {path} -> RequestParams path POST (ReqBodyJson $ object []) 4444
  Delete {path} -> RequestParams path DELETE NoReqBody 4444

parseIO :: W3Spec a -> W.HttpResponse -> IO a
parseIO spec r =
  spec.parser r
    & maybe
      (fail . toS $ spec.description <> "\n" <> "Failed to parse response:\n " <> txt r)
      pure

devLog :: (MonadIO m) => Text -> m ()
devLog = liftIO . T.putStrLn

callWebDriver :: Bool -> RequestArgs -> IO HttpResponse
callWebDriver wantLog RequestParams {subDirs, method, body, port = prt} =
  runReq defaultHttpConfig $ do
    r <- req method url body jsonResponse $ port prt
    log $ "JSON Response:\n" <> txt r
    let fr =
          Response
            { statusCode = responseStatusCode r,
              statusMessage = getLenient . toS $ responseStatusMessage r,
              body = responseBody r :: Value
              -- not used yet may be able to remove and reduce dependncies
              -- headers = L.responseHeaders . toVanillaResponse $ r,
              -- cookies = responseCookieJar r
            }
    log $ "Framework Response:\n" <> txt fr
    pure fr
  where
    log = when wantLog . devLog
    url :: Url 'Http
    url = foldl' (/:) (http "127.0.0.1") subDirs


--------------------------------------------------------------------------------
-- console out (to haskell output window) for debugging
-- run :: forall a. (Show a) =>  W3Spec a -> IO a
-- run spec =
--   describe spec.description $ do
--     devLog . txt $ mkShowable spec
--     r <- callWebDriver True $ mkRequest spec
--     parseIO spec r


-- describe :: (Show a) => Text -> IO a -> IO a
-- describe msg action = do
--   T.putStrLn ""
--   T.putStrLn $ "########### " <> msg <> " ###########"
--   ethr <- handleEx action
--   logResponse ethr
--   either (fail . toS . txt) pure ethr

-- handleEx :: IO a -> IO (Either HttpException a)
-- handleEx = try

-- logResponse :: (Show a) => Either HttpException a -> IO ()
-- logResponse =
--   either
--     ( \e -> do
--         T.putStrLn "!!!!!!!!!! REQUEST FAILED !!!!!!!!!!!"
--         T.putStrLn $ txt e
--     )
--     ( \r -> do
--         T.putStrLn "!!!!!!!!!! REQUEST SUCCEEDED !!!!!!!!!!!"
--         T.putStrLn $ txt r
--     )

--------------------------------------------------------------------------------