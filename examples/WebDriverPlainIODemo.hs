module WebDriverPlainIODemo where

import PyrethrumExtras (txt)
import WebDriverPure (seconds)
import WebDriverDemoUtils ( theInternet, checkBoxesLinkCss )
import WebDriverIO (maximizeWindow, navigateTo, findElement, elementText, click, deleteSession, status, newDefaultFirefoxSession, sleepMilliSecs)
import Data.Text.IO qualified as TIO


log :: Text -> IO ()
log = TIO.putStrLn

-- >>> demo
demo :: IO ()
demo = do
  log "WebDriverPlainIODemo action"
  status' <- status
  log $ "the driver status is (from test): " <> txt status'
  ses <- newDefaultFirefoxSession
  maximizeWindow ses
  navigateTo ses theInternet
  link <- findElement ses checkBoxesLinkCss
  checkButtonText <- elementText ses link
  log $ "checkButtonText: " <> txt checkButtonText
  click ses link
  -- so we can see the navigation worked
  sleepMilliSecs $ 5 * seconds
  deleteSession ses
 
