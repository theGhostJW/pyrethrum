module WebDriverEffect
  ( SessionRef (..),
    WebUI (..),
    WebDriver,
    clickElem,
    go,
    killSession,
    newSession,
    readElem,
    sleep
  )
where

import Effectful as EF ( Effect, DispatchOf, Dispatch(Dynamic) )

import Effectful.Reader.Static as ERS
import Effectful.TH (makeEffect)
import Prelude hiding (second)
import WebDriverSpec (SessionRef(..), ElementRef)

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
  -- session
  NewSession :: WebUI m SessionRef
  KillSession :: SessionRef -> WebUI m ()
  -- navigate
  Go :: SessionRef -> Text -> WebUI m ()
  -- page
  -- toDO Click, Read (take selector)
  ClickElem :: SessionRef -> ElementRef -> WebUI m ()
  ReadElem :: SessionRef -> ElementRef -> WebUI m Text
  -- TODO move this its more generic (eg. used in REST wait loops)
  Sleep :: Int -> WebUI m ()

makeEffect ''WebUI

-- todo add newtype later and don't export type constructor to make
-- sleep wait typesafe

