module WebDriverPlainIODemo where

import Data.Text.IO qualified as TIO
import PyrethrumExtras (txt)
import WebDriverDemoUtils (checkBoxesLinkCss, theInternet, framesUrl, topFrameCSS, midFrameTitle, bottomFrameCss, midFrameCss)
import WebDriverIO
  ( Timeouts (..),
    WindowHandle (..),
    back,
    click,
    closeWindow,
    deleteSession,
    elementText,
    findElement,
    forward,
    fullScreenWindow,
    getCurrentUrl,
    getTimeouts,
    getTitle,
    getWindowHandle,
    maximizeWindow,
    minimizeWindow,
    navigateTo,
    newDefaultFirefoxSession,
    refresh,
    setTimeouts,
    sleepMilliSecs,
    status, newWindow, switchToWindow, switchToFrame, getWindowHandles, SessionId, findElements, FrameReference (FrameElementId, TopLevelFrame, FrameNumber), getActiveElement,
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
sleep1 = sleepMilliSecs $ 1 * seconds

-- >>> demo
demo :: IO ()
demo = do
  logTxt "WebDriverPlainIODemo action"

  logShowM "driver status" status

  ses <- newDefaultFirefoxSession
  log "new session" $ txt ses
  ---
  logShowM "timeouts" $ getTimeouts ses

  let timeouts =
        Timeouts
          { pageLoad = 5 * seconds,
            script = 6 * seconds,
            implicit = 7 * seconds
          }
  setTimeouts ses timeouts
  logShowM "timeouts" $ getTimeouts ses
  ---
  maximizeWindow ses
  navigateTo ses theInternet
  logM "current url" $ getCurrentUrl ses

  minimizeWindow ses
  sleep1

  fullScreenWindow ses
  sleep1

  maximizeWindow ses

  link <- findElement ses checkBoxesLinkCss
  logM "checkButtonText" $ elementText ses link
  click ses link

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

  logTxt "Switch to frame 1"
  switchToFrame ses $ FrameNumber 1

  logShowM "bottom frame exists" $ bottomFameExists ses
  logShowM "active element" $ getActiveElement ses

  deleteSession ses

bottomFameExists :: SessionId -> IO Bool
bottomFameExists ses = not . null <$> findElements ses bottomFrameCss 
  
