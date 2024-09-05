module WebDriverIOInterpreter where

import Effectful as EF
  ( Eff,
    IOE,
    liftIO,
    type (:>),
  )
-- import Effectful.Reader.Dynamic
import Effectful.Dispatch.Dynamic
  ( interpret,
  )
import Web.Api.WebDriver (WebDriverT)
import WebDriverEffect (WebUI (..))
import Prelude hiding (get)

import UnliftIO.Concurrent (threadDelay)
import WebDriverIO

type MyWebDriver eff a = WebDriverT (Eff eff) a

runWebDriverIO :: forall es a. ( IOE :> es) => Eff (WebUI : es) a  -> Eff es a
runWebDriverIO =
  interpret $ \_ ->
    EF.liftIO . \case
      -- session
      NewSession -> newDefaultFirefoxSession
      KillSession sessionRef -> deleteSession sessionRef
      -- navigate
      Go sessionRef url -> navigateTo sessionRef url
      -- page
      ClickElem sessionRef elemRef  -> click sessionRef elemRef
      ReadElem sessionRef elemRef -> elementText sessionRef elemRef
      -- TODO move this its more generic (eg. used in REST wait loops)
      Sleep milliSec -> threadDelay $ 1_000 * milliSec



