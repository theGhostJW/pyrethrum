module WebDriverEffect
  ( SessionRef (..),
    WebUI (..),
    WebDriver,
    click,
    go,
    killSession,
    newSession,
    read,
    sleep
  )
where

import Effectful as EF ( Effect, DispatchOf, Dispatch(Dynamic) )

import Effectful.Reader.Static as ERS
import Effectful.TH (makeEffect)
import Prelude hiding (second)
import WebDriverSpec (SessionRef(..))

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
  NewSession :: WebUI m (Maybe SessionRef)
  KillSession :: SessionRef -> WebUI m ()
  -- page
  Click :: SessionRef -> Text -> WebUI m ()
  Go :: SessionRef -> Text -> WebUI m ()
  Read :: SessionRef -> Text -> WebUI m Text
  Sleep :: Int -> WebUI m ()

makeEffect ''WebUI

-- todo add newtype later and don't export type constructor to make
-- sleep wait typesafe

