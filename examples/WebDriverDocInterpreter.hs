{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- TODO - work out how to get rid of this 
-- Eff has been updated and no longer needs this

module WebDriverDocInterpreter where

import Effectful as EF
  ( Eff,
    IOE,
    type (:>),
  )
-- import Effectful.Reader.Dynamic
import Effectful.Dispatch.Dynamic
  ( interpret, LocalEnv,
  )
import WebDriverEffect (WebUI (..))
import DSL.DocInterpreterUtils (docErr, docErr2)
import DSL.Internal.NodeEvent (NodeEvent)
import DSL.Out ( Out )
import PyrethrumExtras (txt)

runWebDriver :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es{- , E.Error DocException :> es -}) => Eff (WebUI : es) a -> Eff es a
runWebDriver =
   interpret handler
 where
  handler ::
    forall a' localEs.
    (HasCallStack, WebUI :> localEs) =>
    LocalEnv localEs es ->
    WebUI (Eff localEs) a' ->
    Eff es a'
  handler _env  =
      \case
        -- driver
        DriverStatus i -> docErr2 "driverStatus" "get driver status" $ txt i
        -- session
        NewSession ->  docErr "newSession" "create new driver session"
        KillSession _sessionRef -> docErr "killSession" "kill driver session"
        -- window
        FullscreenWindow _sessionRef -> docErr "fullscreenWindow" "make browser fullscreen"
        MaximiseWindow _sessionRef -> docErr "maximiseWindow" "maximise browser window"
        MinimiseWindow _sessionRef -> docErr "minimiseWindow" "minimise browser window"
        -- navigate
        Go _sessionRef url -> docErr2 "go" "navigate to:" url
        -- page
        FindElem _sessionRef selector -> docErr2 "findElem" "find element:" $ txt selector
        ClickElem _sessionRef _elemRef -> docErr "clickElem" "click element (by element reference)"
        ReadElem _sessionRef _elemRef -> docErr "readElem" "get element text (by element reference)"
        -- TODO move this its more generic (eg. used in REST wait loops)
        Sleep milliSec ->  docErr2 "go" "navigate to:" $ txt milliSec
