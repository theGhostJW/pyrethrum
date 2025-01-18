
module WebDriverEffect
  ( 
    WebUI (..),
    WebDriver,
    -- driver
    driverStatus,
    -- session
    newSession,
    killSession,
    -- window
    fullscreenWindow,
    maximiseWindow,
    minimiseWindow,
    -- navigate
    go,
    -- element interactions
    findElem,
    clickElem,
    readElem,
    sleep
  )
where

import Effectful as EF ( Effect, DispatchOf, Dispatch(Dynamic))

import Effectful.Reader.Static as ERS
import Effectful.TH (makeEffect)
import Prelude hiding (second)
import WebDriverSpec (ElementId, DriverStatus, Selector, SessionId, WindowRect)

-- Effect

type WebDriver = ERS.Reader

type instance DispatchOf WebUI = Dynamic

-- todo: reexport types utils from WebDriverSpec / webdriver Pure
{-
TODO: this is just a minimal POC
will need to make this MUCH more sophisticated later
split effects and new session creating a page effect
rather than having the session id leak through to
every element interaction
-}
data WebUI :: Effect where
  -- driver 
  DriverStatus :: WebUI m  DriverStatus
  -- session
  NewSession :: WebUI m SessionId
  KillSession :: SessionId -> WebUI m ()
  -- window
  FullscreenWindow :: SessionId -> WebUI m WindowRect
  MaximiseWindow :: SessionId -> WebUI m WindowRect
  MinimiseWindow :: SessionId -> WebUI m WindowRect
  -- navigate
  Go :: SessionId -> Text -> WebUI m ()
  -- element interactions
  -- toDO Click, Read... (take selector param)
  FindElem :: SessionId -> Selector -> WebUI m ElementId
  ClickElem :: SessionId -> ElementId -> WebUI m ()
  ReadElem :: SessionId -> ElementId -> WebUI m Text
  -- TODO move this its more generic (eg. used in REST wait loops)
  Sleep :: Int -> WebUI m ()

makeEffect ''WebUI