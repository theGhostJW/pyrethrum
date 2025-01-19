module WebDriverPlainIODemo where

import Data.Text.IO qualified as TIO
import PyrethrumExtras (txt)
import WebDriverDemoUtils
  ( anyElmCss,
    bottomFrameCss,
    checkBoxesCss,
    checkBoxesLinkCss,
    contentCss,
    divCss,
    framesUrl,
    inputTagCss,
    inputsUrl,
    midFrameCss,
    midFrameTitle,
    myTextCss,
    shadowDomUrl,
    theInternet,
    topFrameCSS,
  )
import WebDriverIO
  ( FrameReference (FrameElementId, FrameNumber, TopLevelFrame),
    SessionId,
    Timeouts (..),
    WindowHandle (..),
    WindowRect (..),
    back,
    click,
    closeWindow,
    deleteSession,
    findElement,
    findElementFromElement,
    findElementFromShadowRoot,
    findElements,
    findElementsFromElement,
    forward,
    fullScreenWindow,
    getActiveElement,
    getCurrentUrl,
    getElementCssValue,
    getElementProperty,
    getElementShadowRoot,
    getElementText,
    getTimeouts,
    getTitle,
    getWindowHandle,
    getWindowHandles,
    getWindowRect,
    maximizeWindow,
    minimizeWindow,
    navigateTo,
    newDefaultFirefoxSession,
    newWindow,
    refresh,
    setTimeouts,
    setWindowRect,
    sleepMs,
    status,
    switchToFrame,
    switchToParentFrame,
    switchToWindow,
  )
import WebDriverPure (seconds)
import WebDriverSpec (Selector (..))

logTxt :: Text -> IO ()
logTxt = TIO.putStrLn

log :: Text -> Text -> IO ()
log l t = logTxt $ l <> ": " <> t

logShow :: (Show a) => Text -> a -> IO ()
logShow l = log l . txt

logM :: Text -> IO Text -> IO ()
logM l t = t >>= log l

logShowM :: (Show a) => Text -> IO a -> IO ()
logShowM l t = t >>= logShow l

sleep1 :: IO ()
sleep1 = sleepMs $ 1 * seconds

-- >>> demo
demo :: IO ()
demo = do
  logShowM "driver status" status

  ses <- newDefaultFirefoxSession
  log "new session" $ txt ses
  ---
  logShowM "timeouts" $ getTimeouts ses

  let timeouts =
        Timeouts
          { pageLoad = 30 * seconds,
            script = 11 * seconds,
            implicit = 12 * seconds
          }
  setTimeouts ses timeouts
  logShowM "timeouts" $ getTimeouts ses
  ---
  maximizeWindow ses
  navigateTo ses theInternet
  logM "current url" $ getCurrentUrl ses

  logShowM "minimizeWindow" $ minimizeWindow ses
  sleep1

  logShowM "fullscreen" $ fullScreenWindow ses
  sleep1

  logShowM "maximizeWindow" $ maximizeWindow ses

  link <- findElement ses checkBoxesLinkCss
  logM "check box link text" $ getElementText ses link
  click ses link

  sleepMs $ 5 * seconds
  cbs <- findElements ses checkBoxesCss
  forM_ cbs $ \cb ->
    logShowM "checkBox checked property" $ getElementProperty ses cb "checked"

  divs <- findElements ses divCss
  forM_ divs $ \d ->
    logShowM "div overflow value" $ getElementCssValue ses d "overflow"

  -- so we can see the navigation worked
  sleep1

  back ses
  sleep1

  logM "title" $ getTitle ses

  forward ses
  sleep1

  refresh ses
  sleep1

  logM "window Handle" $ getWindowHandle ses

  w <- newWindow ses
  log "new window Handle" $ txt w
  sleep1

  switchToWindow ses w.handle

  logShowM "all windows handles" $ getWindowHandles ses

  closeWindow ses
  log "windows closed" $ txt ses

  deleteSession ses

-- >>> demo2
demo2 :: IO ()
demo2 = do
  logShowM "driver status" status

  ses <- newDefaultFirefoxSession
  setTimeouts
    ses
    Timeouts
      { pageLoad = 30 * seconds,
        script = 11 * seconds,
        implicit = 12 * seconds
      }
  ---
  logShowM "window rect" $ getWindowRect ses
  sleepMs $ 2 * seconds
  logShowM "set window rect" $ setWindowRect ses $ Rect 500 300 500 500
  sleepMs $ 2 * seconds

  navigateTo ses inputsUrl
  div' <- findElement ses contentCss
  input <- findElementFromElement ses div' inputTagCss
  logShow "input tag" input

  els <- findElementsFromElement ses div' anyElmCss
  logShow "elements in div" els

  deleteSession ses

-- >>> demoFrames
demoFrames :: IO ()
demoFrames = do
  ses <- newDefaultFirefoxSession
  navigateTo ses framesUrl

  logTxt "At top level frame"
  logShowM "bottom frame exists" $ bottomFameExists ses

  -- switch frames using element id
  tf <- findElement ses topFrameCSS
  logShow "switch to top frame" tf
  switchToFrame ses (FrameElementId tf)

  logShowM "bottom frame exists after switching to top frame" $ bottomFameExists ses

  mf <- findElement ses midFrameCss
  switchToFrame ses (FrameElementId mf)

  fTitle <- findElement ses midFrameTitle
  logM "middle frame title" $ getElementText ses fTitle

  logTxt "switch to top level frame"
  switchToFrame ses TopLevelFrame
  logShowM "bottom frame exists" $ bottomFameExists ses
  logShowM "active element" $ getActiveElement ses

  -- drill back down to middle frame (repeat the above steps)
  tf' <- findElement ses topFrameCSS
  logShow "switch back to top frame" tf'
  switchToFrame ses (FrameElementId tf')
  logShowM "active element" $ getActiveElement ses

  mf' <- findElement ses midFrameCss
  logShow "drill back down to middle frame" mf'
  switchToFrame ses (FrameElementId mf')
  logShowM "active element" $ getActiveElement ses

  logTxt "switch to parent frame"
  switchToParentFrame ses
  logShowM "active element" $ getActiveElement ses

  logTxt "switch to parent frame again"
  switchToParentFrame ses
  logShowM "active element" $ getActiveElement ses

  logTxt "Switch to frame 1"
  switchToFrame ses $ FrameNumber 1

  logShowM "bottom frame exists" $ bottomFameExists ses
  logShowM "active element" $ getActiveElement ses

  deleteSession ses

bottomFameExists :: SessionId -> IO Bool
bottomFameExists ses = not . null <$> findElements ses bottomFrameCss

-- >>> demoShadowDom
-- *** Exception: VanillaHttpException (HttpExceptionRequest Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/a14a4aae-f94b-481b-9ae7-fcbcc14f716c/element/acc1c176-9a0f-4f99-9bdd-5f0a66cb9c0a/element"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
--  (StatusCodeException (Response {responseStatus = Status {statusCode = 404, statusMessage = "Not Found"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","980"),("date","Sun, 19 Jan 2025 07:29:28 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/a14a4aae-f94b-481b-9ae7-fcbcc14f716c/element/acc1c176-9a0f-4f99-9bdd-5f0a66cb9c0a/element"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
-- , responseEarlyHints = []}) "{\"value\":{\"error\":\"no such element\",\"message\":\"The element with the reference acc1c176-9a0f-4f99-9bdd-5f0a66cb9c0a is not of type HTMLElement\",\"stacktrace\":\"RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:193:5\\nNoSuchElementError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:511:5\\ngetKnownElement@chrome://remote/content/marionette/json.sys.mjs:397:11\\ndeserializeJSON@chrome://remote/content/marionette/json.sys.mjs:263:20\\ncloneObject@chrome://remote/content/marionette/json.sys.mjs:59:24\\ndeserializeJSON@chrome://remote/content/marionette/json.sys.mjs:293:16\\ncloneObject@chrome://remote/content/marionette/json.sys.mjs:59:24\\ndeserializeJSON@chrome://remote/content/marionette/json.sys.mjs:293:16\\njson.deserialize@chrome://remote/content/marionette/json.sys.mjs:297:10\\nreceiveMessage@chrome://remote/content/marionette/actors/MarionetteCommandsChild.sys.mjs:195:30\\n\"}}"))

-- *** Exception: VanillaHttpException (HttpExceptionRequest Request {

--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
--  (StatusCodeException (Response {responseStatus = Status {statusCode = 500, statusMessage = "Internal Server Error"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","96"),("date","Sun, 19 Jan 2025 07:11:41 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
-- , responseEarlyHints = []}) "{\"value\":{\"error\":\"session not created\",\"message\":\"Session is already started\",\"stacktrace\":\"\"}}"))
demoShadowDom :: IO ()
demoShadowDom = do
  ses <- newDefaultFirefoxSession
  navigateTo ses shadowDomUrl

  -- Find the custom element:
  myParagraphId <- findElement ses (CSS "my-paragraph")
  logShow "my-paragraph" myParagraphId
  -- Get its shadow root:
  shadowRootId <- getElementShadowRoot ses myParagraphId
  logShow "shadowRootId" shadowRootId

  -- From the shadow root, find the <p> element inside:
  pInsideShadow <- findElementFromShadowRoot ses shadowRootId (CSS "p")
  logShow "pInsideShadow" pInsideShadow

  -- Retrieve text from the shadow element:
  logShowM "shadow text" $ getElementText ses pInsideShadow
  deleteSession ses
