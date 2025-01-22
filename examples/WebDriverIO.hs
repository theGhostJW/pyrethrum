{-# LANGUAGE UndecidableInstances #-}

module WebDriverIO
  ( W.Timeouts (..),
    W.WindowHandle (..),
    W.SameSite(..),
    W.Selector(..),
    W.SessionId (..),
    W.FrameReference (..),
    W.WindowRect (..),
    W.Cookie (..),
    status,
    findElementFromElement,
    findElementsFromElement,
    findElements,
    getTimeouts,
    setTimeouts,
    back,
    forward,
    getActiveElement,
    refresh,
    getCurrentUrl,
    getElementAttribute,
    getElementShadowRoot,
    findElementFromShadowRoot,
    findElementsFromShadowRoot,
    getTitle,
    getWindowHandles,
    isElementSelected,
    maximizeWindow,
    minimizeWindow,
    fullScreenWindow,
    getWindowHandle,
    getWindowRect,
    closeWindow,
    newWindow,
    newSession,
    newDefaultFirefoxSession,
    deleteSession,
    navigateTo,
    findElement,
    elementClick,
    getElementText,
    setWindowRect,
    sleepMs,
    switchToWindow,
    switchToFrame,
    switchToParentFrame,
    getElementProperty,
    getElementCssValue,
    getElementTagName,
    getElementRect,
    isElementEnabled,
    getElementComputedRole,
    getElementComputedLabel,
    elementClear,
    elementSendKeys,
    printPage,
    getPageSource,
    takeScreenshot,
    takeElementScreenshot,
    executeScript,
    executeScriptAsync,
    getAllCookies,
    getNamedCookie,
    addCookie,
    deleteCookie,
    deleteAllCookies
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
import WebDriverSpec (DriverStatus, ElementId, HttpResponse (..), Selector, SessionId, W3Spec (..))
import WebDriverSpec qualified as W
import Prelude hiding (get, second)

-- ############# IO Implementation #############

status :: IO DriverStatus
status = run W.status

newSession :: Capabilities -> IO SessionId
newSession = run . W.newSession' . capsToJson

getTimeouts :: SessionId -> IO W.Timeouts
getTimeouts = run . W.getTimeouts

setTimeouts :: SessionId -> W.Timeouts -> IO ()
setTimeouts s = run . W.setTimeouts s

getCurrentUrl :: SessionId -> IO Text
getCurrentUrl = run . W.getCurrentUrl

getTitle :: SessionId -> IO Text
getTitle = run . W.getTitle

maximizeWindow :: SessionId -> IO W.WindowRect
maximizeWindow = run . W.maximizeWindow

minimizeWindow :: SessionId -> IO W.WindowRect
minimizeWindow = run . W.minimizeWindow

fullScreenWindow :: SessionId -> IO W.WindowRect
fullScreenWindow = run . W.fullscreenWindow

getWindowHandle :: SessionId -> IO Text
getWindowHandle = run . W.getWindowHandle

getWindowRect :: SessionId -> IO W.WindowRect
getWindowRect = run . W.getWindowRect

getWindowHandles :: SessionId -> IO [Text]
getWindowHandles = run . W.getWindowHandles

newWindow :: SessionId -> IO W.WindowHandle
newWindow = run . W.newWindow

switchToWindow :: SessionId -> Text -> IO ()
switchToWindow s = run . W.switchToWindow s

switchToFrame :: SessionId -> W.FrameReference -> IO ()
switchToFrame s = run . W.switchToFrame s

switchToParentFrame :: SessionId -> IO ()
switchToParentFrame = run . W.switchToParentFrame

closeWindow :: SessionId -> IO ()
closeWindow = run . W.closeWindow

back :: SessionId -> IO ()
back = run . W.back

forward :: SessionId -> IO ()
forward = run . W.forward

refresh :: SessionId -> IO ()
refresh = run . W.refresh

setWindowRect :: SessionId -> W.WindowRect -> IO W.WindowRect
setWindowRect s = run . W.setWindowRect s

newDefaultFirefoxSession :: IO SessionId
newDefaultFirefoxSession = newSession defaultFirefoxCapabilities

deleteSession :: SessionId -> IO ()
deleteSession = run . W.deleteSession

navigateTo :: SessionId -> Text -> IO ()
navigateTo s = run . W.navigateTo s

findElement :: SessionId -> Selector -> IO ElementId
findElement s = run . W.findElement s

findElementFromElement :: SessionId -> ElementId -> Selector -> IO ElementId
findElementFromElement s eid = run . W.findElementFromElement s eid

findElementsFromElement :: SessionId -> ElementId -> Selector -> IO [ElementId]
findElementsFromElement s eid = run . W.findElementsFromElement s eid

getActiveElement :: SessionId -> IO ElementId
getActiveElement = run . W.getActiveElement

isElementSelected :: SessionId -> ElementId -> IO Bool
isElementSelected s = run . W.isElementSelected s

getElementShadowRoot :: SessionId -> ElementId -> IO ElementId
getElementShadowRoot s = run . W.getElementShadowRoot s

findElementFromShadowRoot :: SessionId -> ElementId -> Selector -> IO ElementId
findElementFromShadowRoot s e = run . W.findElementFromShadowRoot s e

getElementTagName :: SessionId -> ElementId -> IO Text
getElementTagName s = run . W.getElementTagName s

getElementRect :: SessionId -> ElementId -> IO W.WindowRect
getElementRect s = run . W.getElementRect s

isElementEnabled :: SessionId -> ElementId -> IO Bool
isElementEnabled s = run . W.isElementEnabled s

getElementComputedRole :: SessionId -> ElementId -> IO Text
getElementComputedRole s = run . W.getElementComputedRole s

getElementComputedLabel :: SessionId -> ElementId -> IO Text
getElementComputedLabel s = run . W.getElementComputedLabel s

findElements :: SessionId -> Selector -> IO [ElementId]
findElements s = run . W.findElements s

findElementsFromShadowRoot :: SessionId -> ElementId -> Selector -> IO [ElementId]
findElementsFromShadowRoot s e = run . W.findElementsFromShadowRoot s e

elementClick :: SessionId -> ElementId -> IO ()
elementClick s = run . W.elementClick s

getElementText :: SessionId -> ElementId -> IO Text
getElementText s = run . W.getElementText s

getElementProperty :: SessionId -> ElementId -> Text -> IO Value
getElementProperty s eid = run . W.getElementProperty s eid

getElementAttribute :: SessionId -> ElementId -> Text -> IO Text
getElementAttribute s eid = run . W.getElementAttribute s eid

getElementCssValue :: SessionId -> ElementId -> Text -> IO Text
getElementCssValue s eid = run . W.getElementCssValue s eid

elementClear :: SessionId -> ElementId -> IO ()
elementClear s = run . W.elementClear s

elementSendKeys :: SessionId -> ElementId -> Text -> IO ()
elementSendKeys s eid = run . W.elementSendKeys s eid

getPageSource :: SessionId -> IO Text
getPageSource = run . W.getPageSource

takeScreenshot :: SessionId -> IO Text
takeScreenshot = run . W.takeScreenshot

takeElementScreenshot :: SessionId -> ElementId -> IO Text
takeElementScreenshot s = run . W.takeElementScreenshot s

printPage :: SessionId -> IO Text
printPage = run . W.printPage

executeScript :: SessionId -> Text -> [Value] -> IO Value
executeScript ses script = run . W.executeScript ses script

executeScriptAsync :: SessionId -> Text -> [Value] -> IO Value
executeScriptAsync ses script = run . W.executeScriptAsync ses script

getAllCookies :: SessionId -> IO [W.Cookie]
getAllCookies = run . W.getAllCookies

getNamedCookie :: SessionId -> Text -> IO W.Cookie
getNamedCookie s = run . W.getNamedCookie s

addCookie :: SessionId -> W.Cookie -> IO ()
addCookie s = run . W.addCookie s

deleteCookie :: SessionId -> Text -> IO ()
deleteCookie s = run . W.deleteCookie s

deleteAllCookies :: SessionId -> IO ()
deleteAllCookies = run . W.deleteAllCookies

-- ############# Utils #############

sleepMs :: Int -> IO ()
sleepMs = threadDelay . (* 1_000)

debug :: Bool
debug = False

-- no console out for "production"
run :: (Show a) => W3Spec a -> IO a
run spec = do
  when debug $ do
    devLog "Request"
    devLog . txt $ spec
  callWebDriver debug (mkRequest spec) >>= parseIO spec

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