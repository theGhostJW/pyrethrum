module WebDriverPlainIODemo where

import Data.Text.IO qualified as TIO
import PyrethrumExtras (txt)
import WebDriverDemoUtils (checkBoxesLinkCss, theInternet)
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
    status, newWindow, switchToWindow, getWindowHandles,
  )
import WebDriverPure (seconds)

logTxt :: Text -> IO ()
logTxt = TIO.putStrLn

log :: Text -> Text -> IO ()
log l t = logTxt $ l <> ": " <> t

logM :: Text -> IO Text -> IO ()
logM l t = t >>= log l

logShowM :: (Show a) => Text -> IO a -> IO ()
logShowM l t = t >>= log l . txt

-- >>> demo
demo :: IO ()
demo = do
  let sleep1 = sleepMilliSecs $ 1 * seconds
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
  navigateTo ses "https://the-internet.herokuapp.com/nested_frames"
