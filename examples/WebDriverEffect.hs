{-# OPTIONS_GHC -Wno-strict-data #-}

module WebDriverEffect
  ( SessionRef (..),
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

import Effectful as EF ( Effect, DispatchOf, Dispatch(Dynamic) )

import Effectful.Reader.Static as ERS
import Effectful.TH (makeEffect)
import Prelude hiding (second)
import WebDriverSpec (SessionRef(..), ElementRef, DriverStatus, Selector)

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
  DriverStatus :: ~Text -> WebUI m  DriverStatus
  -- DriverStatus :: WebUI m  DriverStatus
  -- session
  NewSession :: WebUI m SessionRef
  KillSession :: SessionRef -> WebUI m ()
  -- window
  FullscreenWindow :: SessionRef -> WebUI m ()
  MaximiseWindow :: SessionRef -> WebUI m ()
  MinimiseWindow :: SessionRef -> WebUI m ()
  -- navigate
  Go :: SessionRef -> Text -> WebUI m ()
  -- element interactions
  -- toDO Click, Read... (take selector param)
  FindElem :: SessionRef -> Selector -> WebUI m ElementRef
  ClickElem :: SessionRef -> ElementRef -> WebUI m ()
  ReadElem :: SessionRef -> ElementRef -> WebUI m Text
  -- TODO move this its more generic (eg. used in REST wait loops)
  Sleep :: Int -> WebUI m ()

makeEffect ''WebUI

-- todo add newtype later and don't export type constructor to make
-- sleep wait typesafe

