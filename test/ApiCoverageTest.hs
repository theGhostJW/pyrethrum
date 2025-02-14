module ApiCoverageTest where

import Test.Tasty.HUnit as HUnit
import Text.RawString.QQ (r)
import WebDriverSpec
import Data.Text
import Capabilities


-- todo: test extras - split off

(===) :: (Eq a, Show a, HasCallStack)
  => a -- ^ The actual value
  -> a -- ^ The expected value
  -> Assertion
(===) = (@=?)


-- >>> unit_temp_demo
-- *** Exception: HUnitFailure (Just (SrcLoc {srcLocPackage = "pyrethrum-0.1.0.0-inplace-test", srcLocModule = "ApiCoverageTest", srcLocFile = "/workspaces/pyrethrum/test/ApiCoverageTest.hs", srcLocStartLine = 16, srcLocStartCol = 20, srcLocEndLine = 16, srcLocEndCol = 23})) "expected: 1\n but got: 2"
unit_temp_demo :: IO ()
unit_temp_demo = 1 === 2


{-
!! Replace this the endepoints from the spec with every release
https://w3c.github.io/webdriver/#endpoints - W3C Editor's Draft 10 February 2025
61 endpoints
Method 	URI Template 	Command
POST 	/session 	New Session
-}
endPointsCopiedFromSpc :: String
endPointsCopiedFromSpc = [r|DELETE 	/session/{session id} 	Delete Session
GET 	/status 	Status
GET 	/session/{session id}/timeouts 	Get Timeouts
POST 	/session/{session id}/timeouts 	Set Timeouts
POST 	/session/{session id}/url 	Navigate To
GET 	/session/{session id}/url 	Get Current URL
POST 	/session/{session id}/back 	Back
POST 	/session/{session id}/forward 	Forward
POST 	/session/{session id}/refresh 	Refresh
GET 	/session/{session id}/title 	Get Title
GET 	/session/{session id}/window 	Get Window Handle
DELETE 	/session/{session id}/window 	Close Window
POST 	/session/{session id}/window 	Switch To Window
GET 	/session/{session id}/window/handles 	Get Window Handles
POST 	/session/{session id}/window/new 	New Window
POST 	/session/{session id}/frame 	Switch To Frame
POST 	/session/{session id}/frame/parent 	Switch To Parent Frame
GET 	/session/{session id}/window/rect 	Get Window Rect
POST 	/session/{session id}/window/rect 	Set Window Rect
POST 	/session/{session id}/window/maximize 	Maximize Window
POST 	/session/{session id}/window/minimize 	Minimize Window
POST 	/session/{session id}/window/fullscreen 	Fullscreen Window
GET 	/session/{session id}/element/active 	Get Active Element
GET 	/session/{session id}/element/{element id}/shadow 	Get Element Shadow Root
POST 	/session/{session id}/element 	Find Element
POST 	/session/{session id}/elements 	Find Elements
POST 	/session/{session id}/element/{element id}/element 	Find Element From Element
POST 	/session/{session id}/element/{element id}/elements 	Find Elements From Element
POST 	/session/{session id}/shadow/{shadow id}/element 	Find Element From Shadow Root
POST 	/session/{session id}/shadow/{shadow id}/elements 	Find Elements From Shadow Root
GET 	/session/{session id}/element/{element id}/selected 	Is Element Selected
GET 	/session/{session id}/element/{element id}/attribute/{name} 	Get Element Attribute
GET 	/session/{session id}/element/{element id}/property/{name} 	Get Element Property
GET 	/session/{session id}/element/{element id}/css/{property name} 	Get Element CSS Value
GET 	/session/{session id}/element/{element id}/text 	Get Element Text
GET 	/session/{session id}/element/{element id}/name 	Get Element Tag Name
GET 	/session/{session id}/element/{element id}/rect 	Get Element Rect
GET 	/session/{session id}/element/{element id}/enabled 	Is Element Enabled
GET 	/session/{session id}/element/{element id}/computedrole 	Get Computed Role
GET 	/session/{session id}/element/{element id}/computedlabel 	Get Computed Label
POST 	/session/{session id}/element/{element id}/click 	Element Click
POST 	/session/{session id}/element/{element id}/clear 	Element Clear
POST 	/session/{session id}/element/{element id}/value 	Element Send Keys
GET 	/session/{session id}/source 	Get Page Source
POST 	/session/{session id}/execute/sync 	Execute Script
POST 	/session/{session id}/execute/async 	Execute Async Script
GET 	/session/{session id}/cookie 	Get All Cookies
GET 	/session/{session id}/cookie/{name} 	Get Named Cookie
POST 	/session/{session id}/cookie 	Add Cookie
DELETE 	/session/{session id}/cookie/{name} 	Delete Cookie
DELETE 	/session/{session id}/cookie 	Delete All Cookies
POST 	/session/{session id}/actions 	Perform Actions
DELETE 	/session/{session id}/actions 	Release Actions
POST 	/session/{session id}/alert/dismiss 	Dismiss Alert
POST 	/session/{session id}/alert/accept 	Accept Alert
GET 	/session/{session id}/alert/text 	Get Alert Text
POST 	/session/{session id}/alert/text 	Send Alert Text
GET 	/session/{session id}/screenshot 	Take Screenshot
GET 	/session/{session id}/element/{element id}/screenshot 	Take Element Screenshot
POST 	/session/{session id}/print 	Print Page
|]

session_id :: Text
session_id = "session_id"

session :: SessionId
session = Session session_id

element_id :: Text
element_id = "element_id"

element :: ElementId
element = Element element_id


selector :: Selector
selector = CSS "Blahh"

data SpecLine = MkSpecLine {
  method :: Text,
  uriTemplate :: Text,
  command :: Text
} deriving (Show, Eq, Ord)

toSpecLine :: W3Spec a -> SpecLine
toSpecLine w3 = case w3 of
  Get {} -> MkSpecLine "GET" path command
  Post {} -> MkSpecLine "POST" path command
  PostEmpty {} -> MkSpecLine "POST" path command
  Delete {} -> MkSpecLine "DELETE" path command
  where 
    command = w3.description
    path = 
      replace "{element id}" element_id
      . replace "{session id}" session_id 
      $ "/" <> intercalate "/" w3.path

allSpecsSample :: [SpecLine]
allSpecsSample = [
  toSpecLine $ newSession minFirefoxCapabilities,
  toSpecLine status,
  toSpecLine $ maximizeWindow session,
  toSpecLine $ minimizeWindow session,
  toSpecLine $ fullscreenWindow session,
  toSpecLine $ getTimeouts session,
  toSpecLine $ setTimeouts session $ MkTimeouts Nothing Nothing Nothing,
  toSpecLine $ switchToFrame session TopLevelFrame,
  toSpecLine $ getCurrentUrl session,
  toSpecLine $ findElementFromElement session element selector,
  toSpecLine $ findElementsFromElement session element selector,
  toSpecLine $ findElements session selector,
  toSpecLine $ getTitle session,
  toSpecLine $ getWindowHandle session,
  toSpecLine $ isElementSelected session element,
  toSpecLine $ closeWindow session,
  toSpecLine $ back session,
  toSpecLine $ forward session,
  toSpecLine $ refresh session,
  toSpecLine $ newSession minFirefoxCapabilities,
  toSpecLine $ deleteSession session,
  toSpecLine $ getActiveElement session,
  toSpecLine $ getWindowHandles session,
  toSpecLine $ newWindow session --,
  -- toSpecLine $ switchToWindow session,
  -- toSpecLine $ navigateTo,
  -- toSpecLine $ findElement,
  -- toSpecLine $ getWindowRect,
  -- toSpecLine $ elementClick,
  -- toSpecLine $ getElementText,
  -- toSpecLine $ switchToParentFrame,
  -- toSpecLine $ getElementProperty,
  -- toSpecLine $ getElementAttribute,
  -- toSpecLine $ getElementCssValue,
  -- toSpecLine $ setWindowRect,
  -- toSpecLine $ findElementsFromShadowRoot,
  -- toSpecLine $ getElementShadowRoot,
  -- toSpecLine $ findElementFromShadowRoot,
  -- toSpecLine $ getElementTagName,
  -- toSpecLine $ getElementRect,
  -- toSpecLine $ isElementEnabled,
  -- toSpecLine $ getElementComputedRole,
  -- toSpecLine $ getElementComputedLabel,
  -- toSpecLine $ elementClear,
  -- toSpecLine $ elementSendKeys,
  -- toSpecLine $ getPageSource,
  -- toSpecLine $ takeScreenshot,
  -- toSpecLine $ takeElementScreenshot,
  -- toSpecLine $ performActions',
  -- toSpecLine $ printPage,
  -- toSpecLine $ executeScript,
  -- toSpecLine $ executeScriptAsync,
  -- toSpecLine $ getAllCookies,
  -- toSpecLine $ getNamedCookie,
  -- toSpecLine $ addCookie,
  -- toSpecLine $ deleteCookie,
  -- toSpecLine $ deleteAllCookies,
  -- toSpecLine $ dismissAlert,
  -- toSpecLine $ acceptAlert,
  -- toSpecLine $ getAlertText,
  -- toSpecLine $ sendAlertText,
  -- toSpecLine $ performActions,
  -- toSpecLine $ releaseActions
 ]


