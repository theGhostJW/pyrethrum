module WebDriverPlainIODemo where

import Data.Text.IO qualified as TIO
import PyrethrumExtras (txt)
import WebDriverDemoUtils (bottomFrameCss, checkBoxesCss, checkBoxesLinkCss, divCss, framesUrl, midFrameCss, midFrameTitle, theInternet, topFrameCSS)
import WebDriverIO
  ( FrameReference (FrameElementId, FrameNumber, TopLevelFrame),
    SessionId,
    Timeouts (..),
    WindowHandle (..),
    back,
    click,
    closeWindow,
    deleteSession,
    elementText,
    findElement,
    findElements,
    forward,
    fullScreenWindow,
    getActiveElement,
    getCurrentUrl,
    getElementCssValue,
    getElementProperty,
    getTimeouts,
    getTitle,
    getWindowHandle,
    getWindowHandles,
    maximizeWindow,
    minimizeWindow,
    navigateTo,
    newDefaultFirefoxSession,
    newWindow,
    refresh,
    setTimeouts,
    sleepMs,
    status,
    switchToFrame,
    switchToParentFrame,
    switchToWindow, getWindowRect,
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

  logShowM "minimizeWindow" $  minimizeWindow ses
  sleep1

  logShowM "fullscreen" $ fullScreenWindow ses
  sleep1

  logShowM "maximizeWindow" $ maximizeWindow ses

  link <- findElement ses checkBoxesLinkCss
  logM "check box link text" $ elementText ses link
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
  logM "middle frame title" $ elementText ses fTitle

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
