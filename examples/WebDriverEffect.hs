module WebDriverEffect
  ( SessionId (..),
    WebUI (..),
    WebDriver,
    click,
    go,
    killSession,
    newSession,
    read,
    sleep,
    -- Int constants used in wait finctions
    -- milliseconds per (eg second = 1000, seconds = 1000)
    second,
    seconds,
    minute,
    minutes,
    hour,
    hours
  )
where

import Data.Text.IO qualified as T
import Effectful as EF
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    liftIO,
    runEff,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( interpret,
  )
import Effectful.Reader.Dynamic
import Effectful.Reader.Static as ERS
import Effectful.TH (makeEffect)
import Prelude hiding (second)

-- Effect

type WebDriver = ERS.Reader

type instance DispatchOf WebUI = Dynamic

newtype SessionId = MkSessionId {id :: Text}
  deriving (Show)

{-
TODO: this is just a minimal POC
will need to make this MUCH more sophisticated later
split effects and new session creating a page effect
rather than having the session id leak through to
every element interaction
-}
data WebUI :: Effect where
  -- session
  NewSession :: WebUI m (Maybe SessionId)
  KillSession :: SessionId -> WebUI m ()
  -- page
  Click :: SessionId -> Text -> WebUI m ()
  Go :: SessionId -> Text -> WebUI m ()
  Read :: SessionId -> Text -> WebUI m Text
  Sleep :: Int -> WebUI m ()

makeEffect ''WebUI

-- todo add newtype later and don't export type constructor to make
-- sleep wait typesafe

second :: Int
second = 1_000

seconds :: Int
seconds = second

minute :: Int
minute = 60 * seconds

minutes :: Int
minutes = minute

hour :: Int
hour = 60 * minutes

hours :: Int
hours = hour
