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
import DSL.DocInterpreterUtils (docAction, docFake, docAction2, docFake2)
import DSL.Internal.NodeEvent (NodeEvent)
import DSL.OutEffect ( Out )
import PyrethrumExtras (txt)
import WebDriverSpec (ElementRef(..), DriverStatus (Ready), SessionRef (Session))

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
        DriverStatus -> docFake Ready "get driver status"
        -- session
        NewSession ->  docFake (Session "new-session-id" )"create new driver session"
        KillSession _sessionRef -> docAction "kill driver session"
        -- window
        FullscreenWindow _sessionRef -> docAction "make browser fullscreen"
        MaximiseWindow _sessionRef -> docAction "maximise browser window"
        MinimiseWindow _sessionRef -> docAction"minimise browser window"
        -- navigate
        Go _sessionRef url -> docAction2 "navigate to:" url
        -- page
        FindElem _sessionRef selector -> docFake2 Element {id = "Fake Element Id"} "find element:" $ txt selector
        ClickElem _sessionRef _elemRef -> docAction "click element (by element reference)"
        ReadElem _sessionRef _elemRef -> docFake "Fake Element Text" "get element text (by element reference)"
        -- TODO move this its more generic (eg. used in REST wait loops)
        Sleep milliSec -> docAction2 "sleep for:" $ txt milliSec
