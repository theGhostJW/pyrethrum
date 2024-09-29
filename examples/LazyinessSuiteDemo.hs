module LazyinessSuiteDemo where

import Check
import Core (ParseException, Once, Before)
import DSL.Internal.NodeLog (NodeLog (User), Path (NodePath), UserLog (Info))
import DSL.OutEffect (Out, out)
import Effectful as EF
  ( Eff,
    type (:>),
  )
import PyrethrumBase
import PyrethrumExtras (txt, (?))
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

runDemo :: SuiteRunner -> Suite -> IO ()
runDemo runner suite' = do 
  (logControls, _logLst) <- L.testLogActions True
  runner suite' Unfiltered defaultRunConfig (ThreadCount 1) logControls

-- start geckodriver first: geckodriver &
runIODemo :: Suite -> IO ()
runIODemo = runDemo ioRunner

-- ############### Test Case With Lazy Errors ###################

{-
todo: 
 - check why no callstack :: skip wait till ghc upgrade
 - laziness - esp hooks
   - exceptions  from hooks and actions
 - finish doc interpreter poc
 - merge
-}

blowUpInGetStatus :: Bool
blowUpInGetStatus = True

lazyDemo :: IO ()
lazyDemo = runIODemo suiteLzFail
--- >>> lazyDemo


-- $> lazyDemo

suiteLzFail :: Suite
suiteLzFail =
  [ Hook
      (NodePath "WebDriverDemo" "before")
      nothingBefore
      [ Hook
          (NodePath "WebDriverDemo" "beforeInner")
          driverStatusOnceHook
          [ Fixture (NodePath "WebDriverDemo" "test") testLazy
          ]
      ]
  ]

  -- [Fixture (NodePath "WebDriverDemo" "test") testLazy]


config :: FixtureConfig
config = FxCfg "test" DeepRegression

testLazy :: Fixture DriverStatus
testLazy = Full' config driverStatusOnceHook action_fail parseLzFail itemsLzFail

--- Hook ---

nothingBefore :: Hook Once Before () ()
nothingBefore =
  BeforeHook
    { action = \_rc -> do
        log "This is the outer hook"
        log "Run once before the test"
    }

driverStatusOnceHook :: Hook Once Before () DriverStatus
driverStatusOnceHook =
  BeforeHook'
    { depends = nothingBefore,
      action' = \_rc _void -> do
        log "This is the inner hook"
        log "Run once before the test"
        -- driver_status_fail
        pure $ error "BANG !!!! Hook  failed !!!"
    }

driver_status_fail :: (WebUI :> es, Out NodeLog :> es) => Eff es DriverStatus
driver_status_fail = do 
  
  status <- driverStatus
  -- fails here when driver not running status forced
  -- log $ "the driver status is: " <> txt status
  -- pure $ blowUpInGetStatus ? status $ Ready
  pure $ error "BANG !!!! driver status failed !!!"

action_fail :: (WebUI :> es, Out NodeLog :> es) => RunConfig -> DriverStatus -> Data -> Eff es AS
action_fail _rc i itm = do
  log itm.title
  log $ txt i
  status <- driver_status_fail
  pure $ AS {status, checkButtonText = "Checkboxes"}

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
