module LazyinessSuiteDemo where

import Check
import Core (ParseException, Once, Before)
import DSL.Internal.NodeLog (NodeLog, Path (NodePath))
import DSL.OutEffect (Out)
import Effectful as EF
  ( Eff,
    type (:>),
  )
import PyrethrumBase hiding (Hook)
import PyrethrumBase qualified as PB
import WebDriverEffect as WE
import WebDriverSpec (DriverStatus (Ready), Selector (CSS))
import Filter (Filters(..))
import Internal.SuiteRuntime (ThreadCount(..))
import Internal.Logging qualified as L
import DSL.Logging (log)
import UnliftIO (catchAny)
import Data.Text.IO qualified as TIO
import Data.Text (pack)
import BasePrelude (throw)
import GHC.Records (HasField)


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
 - fix log flushing
 - fix error handling
-}


lazyDemo :: IO ()
lazyDemo = runIODemo suiteLzFail
--- >>> lazyDemo
-- *** Exception: BANG !!!! pureErrorHook Hook failed !!!


-- $> lazyDemo

suiteLzFail :: Suite
suiteLzFail =
  [ PB.Hook
      (NodePath "WebDriverDemo" "before")
      nothingBefore
      [ PB.Hook
          (NodePath "WebDriverDemo" "beforeInner")
          pureErrorHook
          [ Fixture (NodePath "WebDriverDemo" "test") testLazy
          ]
      ]
  ]


config :: FixtureConfig
config = FxCfg "test" DeepRegression

testLazy :: Fixture DriverStatus
testLazy = Full' config pureErrorHook action' parse items

--- Hook ---

nothingBefore :: PB.Hook Once Before () ()
nothingBefore =
  BeforeHook
    { action = \_rc -> do
        log "This is the outer hook"
    }

pureErrorHook :: PB.Hook Once Before () DriverStatus
pureErrorHook =
  BeforeHook'
    { depends = nothingBefore,
      action' = \_rc _void -> do
        log "This is the inner hook"
        -- driver_status_fail
        -- pure Ready
        pure $ error "BANG !!!! pureErrorHook Hook failed !!!"
        -- error "BANG !!!! pureErrorHook Hook failed !!!"
    }


-- action_fail :: (WebUI :> es, Out NodeLog :> es) => RunConfig -> DriverStatus -> Data -> Eff es AS
action' :: (Out NodeLog :> es) => RunConfig -> DriverStatus -> Data -> Eff es AS
action' _rc _hookDriverStatus itm = do
  log itm.title
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


-- ################### Simplified ##################

log_ :: Text -> IO ()
log_ = TIO.putStrLn
data Hook a = Hook {
  description :: Text,
  action :: () -> IO a
  }

data Test a = Test {
  description :: Text,
  action :: a -> IO ()
}

runner :: (HasField "action" n (t -> IO b), HasField "description" n Text) => n -> t -> IO b
runner node input = do
  log_ $ "Running " <> node.description
  catchAny (node.action input) $ \e -> do
    log_ $ "Caught exception trying to run " <> node.description <>  "\nYou better fix this:\n"  <> pack (displayException e)
    >> throw e

runTheTest :: Hook a -> Test a -> IO ()
runTheTest hook test = 
  log_ "" >>
  log_ "##########################################" >>
  runner hook () >>= runner test 

-- ################### Test ##################

workingExample :: IO ()
workingExample = runTheTest workingHook workingTest
-- $ > workingExample

workingHook :: Hook Text
workingHook = Hook "working hook" $ \_ -> do
  log_ "working hook action"
  pure "working hook Output"

workingTest :: Test Text
workingTest = Test "working test" $ \hIn -> do
  log_ $ "working test action with hook input: " <> hIn
  log_ "working test action"
  pure ()

-- ################### Failing Test ##################

failExample :: IO ()
failExample = runTheTest failingHook workingTest
-- $ > failExample

failingHook :: Hook Text
failingHook = Hook "failing hook" $ \_ -> error "BANG !!!! failingHook Hook failed !!!"

-- ################### Bombing Test ##################

bombExample :: IO ()
bombExample = runTheTest bombingHook workingTest
-- >>> bombExample
-- *** Exception: BANG !!!! bombingHook Hook failed !!!

bombingHook :: Hook Text
bombingHook = Hook "bombing hook" $ \_ -> pure $ error "BANG !!!! bombingHook Hook failed !!!"
