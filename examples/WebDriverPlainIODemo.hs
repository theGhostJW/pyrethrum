module WebDriverPlainIODemo where

import Data.Aeson (Value (..))
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import PyrethrumExtras (txt)
import WebDriverDemoUtils
  ( alertsUrl,
    anyElmCss,
    bottomFrameCss,
    checkBoxesCss,
    checkBoxesLinkCss,
    checkBoxesUrl,
    contentCss,
    divCss,
    framesUrl,
    h3TagCss,
    inputTagCss,
    inputsUrl,
    jsAlertXPath,
    jsPromptXPath,
    loginUrl,
    midFrameCss,
    midFrameTitle,
    shadowDomUrl,
    theInternet,
    topFrameCSS,
    userNameCss,
  )
import WebDriverIO
  ( Action (..),
    Actions (..),
    Cookie (..),
    FrameReference (..),
    KeyAction (..),
    Pointer (..),
    PointerAction (..),
    PointerOrigin (..),
    SameSite (..),
    Selector (..),
    SessionId (..),
    Timeouts (..),
    WindowHandle (..),
    WindowRect (..),
    acceptAlert,
    addCookie,
    back,
    closeWindow,
    deleteAllCookies,
    deleteCookie,
    deleteSession,
    dismissAlert,
    elementClear,
    elementClick,
    elementSendKeys,
    executeScript,
    executeScriptAsync,
    findElement,
    findElementFromElement,
    findElementFromShadowRoot,
    findElements,
    findElementsFromElement,
    findElementsFromShadowRoot,
    forward,
    fullScreenWindow,
    getActiveElement,
    getAlertText,
    getAllCookies,
    getCurrentUrl,
    getElementAttribute,
    getElementComputedLabel,
    getElementComputedRole,
    getElementCssValue,
    getElementProperty,
    getElementRect,
    getElementShadowRoot,
    getElementTagName,
    getElementText,
    getNamedCookie,
    getPageSource,
    getTimeouts,
    getTitle,
    getWindowHandle,
    getWindowHandles,
    getWindowRect,
    isElementEnabled,
    isElementSelected,
    maximizeWindow,
    minimizeWindow,
    navigateTo,
    newDefaultFirefoxSession,
    newWindow,
    performActions,
    printPage,
    refresh,
    sendAlertText,
    setTimeouts,
    setWindowRect,
    sleepMs,
    status,
    switchToFrame,
    switchToParentFrame,
    switchToWindow,
    takeElementScreenshot,
    takeScreenshot,
  )
import WebDriverPure (seconds)

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

sleep2 :: IO ()
sleep2 = sleepMs $ 2 * seconds

mkExtendedTimeoutsSession :: IO SessionId
mkExtendedTimeoutsSession = do
  ses <- newDefaultFirefoxSession
  setTimeouts ses $
    Timeouts
      { pageLoad = 30 * seconds,
        script = 11 * seconds,
        implicit = 12 * seconds
      }
  pure ses

-- >>> demoSessionDriverStatus
demoSessionDriverStatus :: IO ()
demoSessionDriverStatus = do
  ses <- newDefaultFirefoxSession
  log "new session" $ txt ses
  logShowM "driver status" status
  deleteSession ses

-- >>> demoSendKeysClear
demoSendKeysClear :: IO ()
demoSendKeysClear = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses loginUrl
  usr <- findElement ses userNameCss

  logTxt "fill in user name"
  elementSendKeys ses usr "user name"
  sleep2

  logTxt "clear user name"
  elementClear ses usr
  sleep2
  deleteSession ses

-- >>> demoForwardBackRefresh
demoForwardBackRefresh :: IO ()
demoForwardBackRefresh = do
  ses <- mkExtendedTimeoutsSession

  navigateTo ses theInternet
  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses

  sleep1

  link <- findElement ses checkBoxesLinkCss
  logTxt "navigating to check boxes page"
  elementClick ses link

  sleep1
  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses
  logTxt "navigating back"
  back ses
  sleep1

  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses
  logTxt "navigating forward"

  forward ses
  sleep1

  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses
  logTxt "refreshing"
  refresh ses
  sleep1

  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses

  deleteSession ses

-- >>> demoWindowHandles
demoWindowHandles :: IO ()
demoWindowHandles = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet

  logM "window Handle" $ getWindowHandle ses

  w <- newWindow ses
  log "new window Handle" $ txt w
  sleep1

  switchToWindow ses w.handle

  logShowM "all windows handles" $ getWindowHandles ses

  closeWindow ses
  log "windows closed" $ txt ses

  logShowM "all windows handles" $ getWindowHandles ses
  deleteSession ses

-- >>> demoWindowSizes
demoWindowSizes :: IO ()
demoWindowSizes = do
  ses <- mkExtendedTimeoutsSession
  ---
  maximizeWindow ses
  navigateTo ses theInternet
  sleep1

  logShowM "minimizeWindow" $ minimizeWindow ses
  sleep1

  logShowM "fullscreen" $ fullScreenWindow ses
  sleep1

  logShowM "maximizeWindow" $ maximizeWindow ses
  sleep1

  deleteSession ses

-- >>> demoElementPageProps
demoElementPageProps :: IO ()
demoElementPageProps = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  logM "current url" $ getCurrentUrl ses
  logM "title" $ getTitle ses

  link <- findElement ses checkBoxesLinkCss
  logM "check box link text" $ getElementText ses link
  elementClick ses link

  cbs <- findElements ses checkBoxesCss
  forM_ cbs $ \cb -> do
    logShowM "checkBox checked property" $ getElementProperty ses cb "checked"
    logShowM "getElementAttribute type" $ getElementAttribute ses cb "type"
    logShowM "getElementCssValue display" $ getElementCssValue ses cb "display"
    logShowM "getElementTagName" $ getElementTagName ses cb
    logShowM "getElementRect" $ getElementRect ses cb
    logShowM "isElementEnabled" $ isElementEnabled ses cb
    logShowM "getElementComputedRole" $ getElementComputedRole ses cb
    logShowM "getElementComputedLabel" $ getElementComputedLabel ses cb

  header <- findElement ses h3TagCss
  logShowM "header computed role" $ getElementComputedRole ses header
  logShowM "header computed label" $ getElementComputedLabel ses header

  divs <- findElements ses divCss
  forM_ divs $ \d ->
    logShowM "div overflow value" $ getElementCssValue ses d "overflow"

  deleteSession ses

-- >>> demoTimeouts
demoTimeouts :: IO ()
demoTimeouts = do
  ses <- newDefaultFirefoxSession
  log "new session" $ txt ses
  ---
  logShowM "timeouts" $ getTimeouts ses
  setTimeouts ses $
    Timeouts
      { pageLoad = 50 * seconds,
        script = 11 * seconds,
        implicit = 12 * seconds
      }
  logShowM "updated timeouts" $ getTimeouts ses
  deleteSession ses

-- >>> demoWindowRecs
demoWindowRecs :: IO ()
demoWindowRecs = do
  ses <- mkExtendedTimeoutsSession
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

-- >>> demoWindowFindElement
demoWindowFindElement :: IO ()
demoWindowFindElement = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses inputsUrl
  allElms <- findElements ses anyElmCss
  logShow "all elements" allElms
  div' <- findElement ses contentCss
  input <- findElementFromElement ses div' inputTagCss
  logShow "input tag" input

  els <- findElementsFromElement ses div' anyElmCss
  logShow "elements in div" els

  deleteSession ses

-- >>> demoFrames
demoFrames :: IO ()
demoFrames = do
  ses <- mkExtendedTimeoutsSession
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
demoShadowDom :: IO ()
demoShadowDom = do
  -- TODO: Session deletion causes gheckoDriver to thow an error
  -- even though the steps all work - driver bug? investiggate log
  ses <- mkExtendedTimeoutsSession
  navigateTo ses shadowDomUrl

  -- Find the custom element:
  myParagraphId <- findElement ses (CSS "my-paragraph")
  logShow "my-paragraph" myParagraphId

  -- Get its shadow root:
  shadowRootId <- getElementShadowRoot ses myParagraphId
  logShow "shadowRootId" shadowRootId

  -- From the shadow root, find all elements
  -- allInsideShadow <- findElementsFromShadowRoot ses shadowRootId (CSS "*")
  allInsideShadow <- findElementsFromShadowRoot ses myParagraphId anyElmCss
  logShow "shadow root elements" allInsideShadow

  logTxt "got root elements"

  srootElm <- findElementFromShadowRoot ses myParagraphId anyElmCss
  logShow "shadow root element" srootElm

  -- Retrieve text from the shadow element:
  logShowM "shadow text" $ getElementText ses srootElm
  deleteSession ses

-- >>> demoIsElementSelected
demoIsElementSelected :: IO ()
demoIsElementSelected = do
  ses <- mkExtendedTimeoutsSession
  logShowM "driver status" status
  navigateTo ses checkBoxesUrl
  allCbs <- findElements ses checkBoxesCss
  forM_ allCbs $ \cb -> do
    logShowM "checkBox isElementSelected" $ isElementSelected ses cb
    elementClick ses cb
    logTxt "clicked"
    logShowM "checkBox isElementSelected" $ isElementSelected ses cb
    logTxt "------------------"

  deleteSession ses

-- >>> demoGetPageSourceScreenShot
demoGetPageSourceScreenShot :: IO ()
demoGetPageSourceScreenShot = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  logTxt "!!!!! Page Source !!!!!"
  logShowM "page source" $ getPageSource ses

  logTxt "!!!!! Screenshot!!!!!"
  logShowM "take screenshot" $ takeScreenshot ses

  logTxt "!!!!! Screenshot Element !!!!!"
  chkBoxLink <- findElement ses checkBoxesLinkCss
  logShowM "take element screenshot" $ takeElementScreenshot ses chkBoxLink
  deleteSession ses

-- >>> demoPrintPage
demoPrintPage :: IO ()
demoPrintPage = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  -- pdf (encoded string)
  logM "print page" $ printPage ses
  deleteSession ses

--- >>> demoExecuteScript
demoExecuteScript :: IO ()
demoExecuteScript = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  logShowM "executeScript" $ executeScript ses "return arguments[0];" [String "Hello from Pyrethrum!", Number 2000]
  sleep2
  logTxt "executing asynch alert"
  executeScriptAsync ses "setTimeout(() => alert('Hello from Pyrethrum!'), 2000); return 5;" []
  logTxt "after asynch alert"
  sleep2
  deleteSession ses

-- >>> demoCookies
demoCookies :: IO ()
demoCookies = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  logShowM "cookies" $ getAllCookies ses

  logShowM "getNamedCookie: optimizelyEndUserId" $ getNamedCookie ses "optimizelyEndUserId"

  let myCookie =
        Cookie
          { name = "myCookie",
            value = "myCookieValue",
            path = Just "/",
            domain = Just "the-internet.herokuapp.com",
            secure = Just True,
            sameSite = Just Strict,
            httpOnly = Just False,
            expiry = Just 2772072677
          }

  logShow "cookie to add" myCookie
  logShowM "addCookie" $ addCookie ses myCookie
  logShowM "cookies after add" $ getAllCookies ses

  logShowM "deleteCookie (myCookie)" $ deleteCookie ses "myCookie"
  logShowM "cookies after delete" $ getAllCookies ses

  logShowM "deleteAllCookies" $ deleteAllCookies ses
  logShowM "cookies after delete all" $ getAllCookies ses
  deleteSession ses

-- >>> demoAlerts
demoAlerts :: IO ()
demoAlerts = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses alertsUrl

  alert <- findElement ses jsAlertXPath
  elementClick ses alert

  sleep2
  logShowM "get alert text" $ getAlertText ses

  sleep2
  logShowM "acceptAlert" $ acceptAlert ses

  sleep1
  prompt <- findElement ses jsPromptXPath
  elementClick ses prompt

  sleep1
  logShowM "sendAlertText: I am Dave" $ sendAlertText ses "I am Dave"

  sleep2
  dismissAlert ses

  sleep1
  deleteSession ses

-- >>> demoActions
-- *** Exception: VanillaHttpException (HttpExceptionRequest Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/202bc6fe-1d9c-49b4-9107-cb3dafb7727b/actions"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
--  (StatusCodeException (Response {responseStatus = Status {statusCode = 400, statusMessage = "Bad Request"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=utf-8"),("cache-control","no-cache"),("content-length","93"),("date","Thu, 30 Jan 2025 01:00:00 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/202bc6fe-1d9c-49b4-9107-cb3dafb7727b/actions"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
-- , responseEarlyHints = []}) "{\"value\":{\"error\":\"invalid argument\",\"message\":\"Body was not a JSON Object\",\"stacktrace\":\"\"}}"))
demoActions :: IO ()
demoActions = do
  ses <- mkExtendedTimeoutsSession
  navigateTo ses theInternet
  let actions =
        MkActions
          [ [ Pointer
                { subType = Mouse,
                  pointerAction =
                    Move
                      { origin = Viewport,
                        duration = Just $ 4 * seconds,
                        x = 150,
                        y = 150
                      },
                  pointerId = 0,
                  pressed = Set.empty,
                  x = 0,
                  y = 0
                }
            ]
          ]

  performActions ses actions

  sleep2
  deleteSession ses
