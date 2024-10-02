module LazyinessSuiteDemo where

import Check
import Core (ParseException, Once, Before)
import DSL.Internal.NodeLog (NodeLog, Path (NodePath))
import DSL.OutEffect (Out)
import Effectful as EF
  ( Eff,
    type (:>),
  )
import PyrethrumBase
import WebDriverEffect as WE
import WebDriverSpec (DriverStatus (Ready), Selector (CSS))
import Filter (Filters(..))
import Internal.SuiteRuntime (ThreadCount(..))
import Internal.Logging qualified as L
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


lazyDemo :: IO ()
lazyDemo = runIODemo suiteLzFail
--- >>> lazyDemo
-- *** Exception: BANG !!!! driverStatusOnceHook Hook  failed !!!


-- $> lazyDemo

suiteLzFail :: Suite
suiteLzFail =
  [ Hook
      (NodePath "WebDriverDemo" "before")
      nothingBefore
      [ Hook
          (NodePath "WebDriverDemo" "beforeInner")
          pureErrorHook
          [ Fixture (NodePath "WebDriverDemo" "test") testLazy
          ]
      ]
  ]

  -- [Fixture (NodePath "WebDriverDemo" "test") testLazy]


config :: FixtureConfig
config = FxCfg "test" DeepRegression

testLazy :: Fixture DriverStatus
testLazy = Full' config pureErrorHook action_fail parseLzFail itemsLzFail

--- Hook ---

nothingBefore :: Hook Once Before () ()
nothingBefore =
  BeforeHook
    { action = \_rc -> do
        log "This is the outer hook"
    }

pureErrorHook :: Hook Once Before () DriverStatus
pureErrorHook =
  BeforeHook'
    { depends = nothingBefore,
      action' = \_rc _void -> do
        log "This is the inner hook"
        -- driver_status_fail
        -- pure Ready
        pure $ error "BANG !!!! driverStatusOnceHook Hook failed !!!"
        -- error "BANG !!!! driverStatusOnceHook Hook  failed !!!"
    }

-- driver_status_fail :: (WebUI :> es, Out NodeLog :> es) => Eff es DriverStatus
driver_status_fail :: (WebUI :> es) => Eff es DriverStatus
driver_status_fail = do 
  _status <- driverStatus
  -- fails here when driver not running status forced
  -- log $ "the driver status is: " <> txt status
  -- pure $ blowUpInGetStatus ? status $ Ready
  pure $ error "BOOM %%%% driver status failed %%%%"

-- action_fail :: (WebUI :> es, Out NodeLog :> es) => RunConfig -> DriverStatus -> Data -> Eff es AS
action_fail :: (Out NodeLog :> es) => RunConfig -> DriverStatus -> Data -> Eff es AS
action_fail _rc _hookDriverStatus itm = do
  log itm.title
  -- log $ txt hookDriverStatus
  -- status <- driver_status_fail
  -- pure $ AS {status, checkButtonText = "Checkboxes"}
  pure $ AS {status = Ready, checkButtonText = "Checkboxes"}

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
