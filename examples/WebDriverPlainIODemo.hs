module WebDriverPlainIODemo where

import Data.Text.IO qualified as TIO
import PyrethrumExtras (txt)
import WebDriverDemoUtils (checkBoxesLinkCss, theInternet, framesUrl, topFrameCSS, midFrameTitle, bottomFrameCss)
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
    status, newWindow, switchToWindow, switchToFrame, getWindowHandles, SessionRef, findElements,
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
-- *** Exception: VanillaHttpException (HttpExceptionRequest Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/58167c4f-8540-42e9-b6c9-f8f0cfa93816/element/8c99d472-4b87-4bfb-b5df-5f236680dc1f/frame"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
--  (StatusCodeException (Response {responseStatus = Status {statusCode = 405, statusMessage = "Method Not Allowed"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","text/plain; charset=utf-8"),("content-length","23"),("date","Thu, 16 Jan 2025 21:59:02 GMT")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
--   host                 = "127.0.0.1"
--   port                 = 4444
--   secure               = False
--   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--   path                 = "/session/58167c4f-8540-42e9-b6c9-f8f0cfa93816/element/8c99d472-4b87-4bfb-b5df-5f236680dc1f/frame"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
--   proxySecureMode      = ProxySecureWithConnect
-- }
-- , responseEarlyHints = []}) "HTTP method not allowed"))
demoFrames :: IO ()
demoFrames = do
  ses <- newDefaultFirefoxSession
  navigateTo ses framesUrl

  sleep1

  logShowM "bottom frame exists" $ bottomFameExists ses
  
  tf <- findElement ses topFrameCSS
  switchToFrame ses tf

  logShowM "bottom frame exists after switching to top frame" $ bottomFameExists ses

  fTitle <- findElement ses midFrameTitle
  logM "checkButtonText" $ elementText ses fTitle

  deleteSession ses

bottomFameExists :: SessionRef -> IO Bool
bottomFameExists ses = not . null <$> findElements ses bottomFrameCss 
  
