module WebDriverDemo where

import Check
import Core (ParseException)
import DSL.Internal.NodeLog (NodeLog (User), Path (NodePath), UserLog (Info))
import DSL.OutEffect (Out, out)
import Effectful as EF
  ( Eff,
    type (:>),
  )
import PyrethrumBase
import PyrethrumExtras (txt)
import WebDriverEffect as WE
import WebDriverSpec (DriverStatus (Ready), Selector (CSS))
import Filter (Filters(..))
import Internal.SuiteRuntime (ThreadCount(..))
import Internal.Logging qualified as L
import WebDriverPure (seconds)
import DSL.Logging (log)


-- ################### Effectful Demo ##################

_theInternet :: Text
_theInternet = "https://the-internet.herokuapp.com/"

_checkBoxesLinkCss :: Selector
_checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

suite :: Suite
suite =
  [Fixture (NodePath "WebDriverDemo" "test") test]

runDemo :: SuiteRunner -> Suite -> IO ()
runDemo runner suite' = do 
  (logControls, _logLst) <- L.testLogActions True
  runner suite' Unfiltered defaultRunConfig (ThreadCount 1) logControls

-- start geckodriver first: geckodriver &
runIODemo :: Suite -> IO ()
runIODemo = runDemo ioRunner

-- ############### Test Case ###################

-- >>> runIODemo suite

test :: Fixture ()
test = Full config action parse items

config :: FixtureConfig
config = FxCfg "test" DeepRegression

driver_status :: (WebUI :> es, Out NodeLog :> es) => Eff es DriverStatus
driver_status = do 
  status <- driverStatus
  log $ "the driver status is: " <> txt status
  pure status

action :: (WebUI :> es, Out NodeLog :> es) => RunConfig -> Data -> Eff es AS
action _rc i = do
  log i.title
  status <- driver_status
  log $ "the driver status is (from test): " <> txt status
  ses <- newSession
  maximiseWindow ses
  go ses _theInternet
  link <- findElem ses _checkBoxesLinkCss
  checkButtonText <- readElem ses link
  clickElem ses link
  -- so we can see the navigation worked
  sleep $ 5 * seconds
  killSession ses
  pure $ AS {status, checkButtonText}
 
data AS = AS
  { status :: DriverStatus,
    checkButtonText :: Text
  }
  deriving (Show)

data DS = DS
  { status :: DriverStatus,
    checkButtonText :: Text
  }
  deriving (Show)

data Data = Item
  { id :: Int,
    title :: Text,
    checks :: Checks DS
  }
  deriving (Show, Read)

parse :: AS -> Either ParseException DS
parse AS {..} = pure $ DS {..}

items :: RunConfig -> DataSource Data
items _rc =
  ItemList
    [ Item
        { id = 1,
          title = "test the internet",
          checks =
            chk "Driver is ready" ((== Ready) . (.status))
              <> chk "Checkboxes text as expected" ((== "Checkboxes") . (.checkButtonText))
        }
    ]

-- ############### Test Case With Lazy Errors ###################

{-
todo: 
 - exceptions in selenium
 - check why no callstack
 - laziness - esp hooks
 - finish doc interpreter poc
 - merge
-}

--- >>> lazyDemo
lazyDemo :: IO ()
lazyDemo = runIODemo suiteLzFail

suiteLzFail :: Suite
suiteLzFail =
  [Fixture (NodePath "WebDriverDemo" "test") testLazy]


testLazy :: Fixture ()
testLazy = Full config action_fail parseLzFail itemsLzFail

driver_status_fail :: (WebUI :> es, Out NodeLog :> es) => Eff es DriverStatus
driver_status_fail = do 
  status <- driverStatus
  log $ "the driver status is: " <> txt status
  pure $ error "BANG !!!! driver status failed !!!"

action_fail :: (WebUI :> es, Out NodeLog :> es) => RunConfig -> Data -> Eff es AS
action_fail _rc i = do
  log i.title
  status <- driver_status_fail
  -- log $ "the driver status is (from test): " <> txt status
  ses <- newSession
  maximiseWindow ses
  go ses _theInternet
  link <- findElem ses _checkBoxesLinkCss
  checkButtonText <- readElem ses link
  clickElem ses link
  -- so we can see the navigation worked
  sleep $ 5 * seconds
  killSession ses
  pure $ AS {status, checkButtonText}
 
parseLzFail :: AS -> Either ParseException DS
parseLzFail AS {..} = pure $ DS {..}

itemsLzFail :: RunConfig -> DataSource Data
itemsLzFail _rc =
  ItemList
    [ Item
        { id = 1,
          title = "test the internet",
          checks =
            chk "Driver is ready" ((== Ready) . (.status))
              <> chk "Checkboxes text as expected" ((== "Checkboxes") . (.checkButtonText))
        }
    ]
