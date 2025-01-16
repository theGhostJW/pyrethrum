{-# LANGUAGE UndecidableInstances #-}

module WebDriverIO
  ( W.Timeouts (..),
    W.WindowHandle(..),
    status,
    getTimeouts,
    setTimeouts,
    back,
    forward,
    refresh,
    getCurrentUrl,
    getTitle,
    getWindowHandles,
    maximizeWindow,
    minimizeWindow,
    fullScreenWindow,
    getWindowHandle,
    closeWindow,
    newWindow,
    newSession,
    newDefaultFirefoxSession,
    deleteSession,
    navigateTo,
    findElement,
    click,
    elementText,
    sleepMilliSecs,
    switchToWindow
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

import UnliftIO.Concurrent (threadDelay)
import Web.Api.WebDriver (Capabilities, defaultFirefoxCapabilities)
import WebDriverPure (RequestArgs (..), capsToJson)
import WebDriverSpec (DriverStatus, ElementRef, HttpResponse (..), Selector, SessionRef, W3Spec (..))
import WebDriverSpec qualified as W
import Prelude hiding (get, second)

-- ############# IO Implementation #############

status :: IO DriverStatus
status = run W.status

newSession :: Capabilities -> IO SessionRef
newSession = run . W.newSession' . capsToJson

getTimeouts :: SessionRef -> IO W.Timeouts
getTimeouts = run . W.getTimeouts

setTimeouts :: SessionRef -> W.Timeouts -> IO ()
setTimeouts s = run . W.setTimeouts s

getCurrentUrl :: SessionRef -> IO Text
getCurrentUrl = run . W.getCurrentUrl

getTitle :: SessionRef -> IO Text
getTitle = run . W.getTitle

maximizeWindow :: SessionRef -> IO ()
maximizeWindow = run . W.maximizeWindow

minimizeWindow :: SessionRef -> IO ()
minimizeWindow = run . W.minimizeWindow

fullScreenWindow :: SessionRef -> IO ()
fullScreenWindow = run . W.fullscreenWindow

getWindowHandle :: SessionRef -> IO Text
getWindowHandle = run . W.getWindowHandle

getWindowHandles :: SessionRef -> IO [Text]
getWindowHandles = run . W.getWindowHandles

newWindow :: SessionRef -> IO W.WindowHandle
newWindow = run . W.newWindow

switchToWindow :: SessionRef -> Text -> IO ()
switchToWindow s = run . W.switchToWindow s

closeWindow :: SessionRef -> IO ()
closeWindow = run . W.closeWindow

back :: SessionRef -> IO ()
back = run . W.back

forward :: SessionRef -> IO ()
forward = run . W.forward

refresh :: SessionRef -> IO ()
refresh = run . W.refresh

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