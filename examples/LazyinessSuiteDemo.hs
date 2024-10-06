module LazyinessSuiteDemo where

import BasePrelude (throw)
import Check
import Core (Before, Each, Once, ParseException)
import DSL.Internal.NodeLog (NodeLog, Path (NodePath))
import DSL.Logging (log)
import DSL.OutEffect (Out)
import Data.Text (pack)
import Data.Text.IO qualified as TIO
import Effectful as EF
  ( Eff,
    type (:>),
  )
import Filter (Filters (..))
import GHC.Records (HasField)
import Internal.Logging qualified as L
import Internal.SuiteRuntime (ThreadCount (..))
import PyrethrumBase hiding (Hook)
import PyrethrumBase qualified as PB
import UnliftIO (catchAny)
import WebDriverSpec (DriverStatus (Ready), Selector (CSS))

-- ################### Effectful Demo ##################

_theInternet :: Text
_theInternet = "https://the-internet.herokuapp.com/"

_checkBoxesLinkCss :: Selector
_checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

runDemo :: SuiteRunner -> Suite -> IO ()
runDemo runner' suite' = do
  (logControls, _logLst) <- L.testLogActions True
  runner' suite' Unfiltered defaultRunConfig (ThreadCount 1) logControls

-- start geckodriver first: geckodriver &
runIODemo :: Suite -> IO ()
runIODemo = runDemo ioRunner

-- ############### Test Case With Lazy Errors ###################

onceHkDeferredDemo :: IO ()
onceHkDeferredDemo = runIODemo onceHkDeferredSuite

-- $ > onceHkDeferredDemo

onceHkDeferredSuite :: Suite
onceHkDeferredSuite =
  [ PB.Hook
      (NodePath "WebDriverDemo" "before")
      nothingBefore
      [ PB.Hook
          (NodePath "WebDriverDemo" "beforeInner")
          pureErrorHook
          [ Fixture (NodePath "WebDriverDemo" "test") fxLogMessage
          ]
      ]
  ]

eachHkDeferredSuiteDemo :: IO ()
eachHkDeferredSuiteDemo = runIODemo eachHkDeferredSuite

-- $ > eachHkDeferredSuiteDemo

eachHkDeferredSuite :: Suite
eachHkDeferredSuite =
  [ PB.Hook
      (NodePath "WebDriverDemo" "before once")
      nothingBefore
      [ PB.Hook
          (NodePath "WebDriverDemo" "before inner once")
          pureErrorHook
          [ PB.Hook
              (NodePath "WebDriverDemo" "each hook")
              eachHook
              [Fixture (NodePath "WebDriverDemo" "test") fxLogMessage]
          ]
      ]
  ]

eachHkFailDemo :: IO ()
eachHkFailDemo = runIODemo eachHkFail

-- $> eachHkFailDemo
-- runs effect but does not throw error because test does not use hook input
-- TODO  - parameterise action with bool to use not use hook input
eachHkFail :: Suite
eachHkFail =
  [ PB.Hook
      (NodePath "WebDriverDemo" "before once")
      nothingBefore
      [ PB.Hook
          (NodePath "WebDriverDemo" "each hook Gunna Blow")
          eachFailureHook
          [Fixture (NodePath "WebDriverDemo" "test") fxLogMessage]
      ]
  ]

config :: FixtureConfig
config = FxCfg "test" DeepRegression

fxLogMessage :: Fixture DriverStatus
fxLogMessage = Full' config pureErrorHook action' parse items

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
      action' = pureError "This is the inner hook" "BANG !!!! pureErrorHook Hook failed !!!"
    }

eachHook :: PB.Hook Each Before DriverStatus DriverStatus
eachHook =
  BeforeHook'
    { depends = pureErrorHook,
      action' = \_rc ds -> do
        log "This is each hook"
        pure ds
    }


eachFailureHook :: PB.Hook Each Before () DriverStatus
eachFailureHook =
  BeforeHook'
    { depends = nothingBefore,
      action' =  \_rc _ds -> do 
        log "This is each hook" 
        pure $ error "BANG !!!! eachFailureHook Hook failed !!!"
    }

pureError :: (Out NodeLog :> es) => Text -> Text -> rc -> i -> Eff es b
pureError logMsg errMsg _rc _i = do
  log logMsg
  pure $ error errMsg

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

data Hook a = Hook
  { description :: Text,
    action :: () -> IO a
  }

data Test a = Test
  { description :: Text,
    action :: a -> IO ()
  }

runner :: (HasField "action" n (t -> IO b), HasField "description" n Text) => n -> t -> IO b
runner node input = do
  log_ $ "Running " <> node.description
  catchAny (node.action input) $ \e ->
    do
      log_ $ "Caught exception trying to run " <> node.description <> "\nYou better fix this:\n" <> pack (displayException e)
      >> throw e

runTheTest :: Hook a -> Test a -> IO ()
runTheTest hook test =
  log_ ""
    >> log_ "##########################################"
    >> runner hook ()
    >>= runner test

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
