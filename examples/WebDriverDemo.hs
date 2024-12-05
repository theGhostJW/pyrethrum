module WebDriverDemo where

import Check
import Core (ParseException)
import DSL.Internal.NodeLog (NodeLog, Path (NodePath))
import DSL.OutEffect (Out)
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
import CoreTypeFamilies (DataSource (..))


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
test = mkFull config action parse dataSource

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

data VS = VS
  { status :: DriverStatus,
    checkButtonText :: Text
  }
  deriving (Show)

data Data = Item
  { id :: Int,
    title :: Text,
    checks :: Checks VS
  }
  deriving (Show, Read)

parse :: AS -> Either ParseException VS
parse AS {..} = pure $ VS {..}

dataSource :: RunConfig -> DataSource Data VS 
dataSource _rc =
  Items
    [ Item
        { id = 1,
          title = "test the internet",
          checks =
            chk "Driver is ready" ((== Ready) . (.status))
              <> chk "Checkboxes text as expected" ((== "Checkboxes") . (.checkButtonText))
        }
    ]