module WebDriverEffect where

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
