module WebDriverPlainIODemo where

import Data.Text.IO qualified as TIO
import PyrethrumExtras (txt)
import WebDriverDemoUtils
  ( anyElmCss,
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
    loginUrl,
    midFrameCss,
    midFrameTitle,
    myTextCss,
    shadowDomUrl,
    theInternet,
    topFrameCSS,
    userNameCss,
  )
import WebDriverIO
  ( FrameReference (FrameElementId, FrameNumber, TopLevelFrame),
    SessionId,
    Timeouts (..),
    WindowHandle (..),
    WindowRect (..),
    back,
    closeWindow,
    deleteSession,
    elementClick,
    findElement,
    findElementFromElement,
    findElementFromShadowRoot,
    findElements,
    findElementsFromElement,
    findElementsFromShadowRoot,
    forward,
    fullScreenWindow,
    getActiveElement,
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
    refresh,
    setTimeouts,
    setWindowRect,
    sleepMs,
    status,
    switchToFrame,
    switchToParentFrame,
    switchToWindow,
    elementClear,
    elementSendKeys
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

sleep2 :: IO ()
sleep2 = sleepMs $ 2 * seconds

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
