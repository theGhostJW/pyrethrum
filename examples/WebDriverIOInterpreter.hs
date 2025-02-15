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
import WebDriverEffect (WebUI (..))
import WebDriverIO
  ( elementClick,
    deleteSession,
    getElementText,
    navigateTo,
    minFirefoxSession,
    status, findElement, fullScreenWindow, maximizeWindow, minimizeWindow, sleepMs,
  )

runWebDriver :: forall es a. (IOE :> es) => Eff (WebUI : es) a -> Eff es a
runWebDriver =
  interpret $ \_ ->
    EF.liftIO . \case
      -- driver
      DriverStatus -> status
      -- session
      NewSession -> minFirefoxSession
      KillSession sessionRef -> deleteSession sessionRef
      -- window
      FullscreenWindow sessionRef -> fullScreenWindow sessionRef
      MaximiseWindow sessionRef -> maximizeWindow sessionRef
      MinimiseWindow sessionRef -> minimizeWindow sessionRef
      -- navigate
      Go sessionRef url -> navigateTo sessionRef url
      -- page
      FindElem sessionRef selector -> findElement sessionRef selector
      ClickElem sessionRef elemRef -> elementClick sessionRef elemRef
      ReadElem sessionRef elemRef -> getElementText sessionRef elemRef
      -- TODO move this its more generic (eg. used in REST wait loops)
      Sleep milliSec -> sleepMs milliSec
