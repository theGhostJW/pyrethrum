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
import WebDriverSpec (DriverStatus (Ready))
import PyrethrumExtras (txt)

-- ################### Effectful Demo ##################

runDemo :: SuiteRunner -> Suite -> IO ()
runDemo runner' suite' = do
  (logControls, _logLst) <- L.testLogActions True
  runner' suite' Unfiltered defaultRunConfig (ThreadCount 1) logControls

-- start geckodriver first: geckodriver &
runIODemo :: Suite -> IO ()
runIODemo = runDemo ioRunner

-- ############### Test Cases With Lazy Errors ###################

onceHkDeferredDemo :: IO ()
onceHkDeferredDemo = runIODemo onceHkDeferredSuite

-- $> onceHkDeferredDemo
-- creates a test initialisation failure in test when once hook output is cached


onceHkDeferredSuite :: Suite
onceHkDeferredSuite =
  [ PB.Hook
      (NodePath "WebDriverDemo" "before")
      nothingOnceBefore
      [ PB.Hook
          (NodePath "WebDriverDemo" "beforeInner")
          pureOnceErrorHook
          [ Fixture (NodePath "WebDriverDemo" "fixture") $ fxLogMessage False
          ]
      ]
  ]
------------------------------------------------------

-- $> threadHkInitFailSuiteDemo
-- creates an initialisation failure in each hook when thread hook output is cached

threadHkInitFailSuiteDemo :: IO ()
threadHkInitFailSuiteDemo = runIODemo threadHkInitFailSuite

threadHkInitFailSuite :: Suite
threadHkInitFailSuite =
  [ PB.Hook
      (NodePath "WebDriverDemo" "before once")
      nothingOnceBefore
      [ PB.Hook
          (NodePath "WebDriverDemo" "before inner thread")
          pureThreadErrorHook
          [ PB.Hook
              (NodePath "WebDriverDemo" "each hook")
              eachHook
              [Fixture (NodePath "WebDriverDemo" "fixture") $ fxLogMessage False]
          ]
      ]
  ]

------------------------------------------------------

-- $> eachHkInitFailSuiteDemo
-- creates an initialisation failure in each hook when once hook output is cached

eachHkInitFailSuiteDemo :: IO ()
eachHkInitFailSuiteDemo = runIODemo eachHkInitFailSuite

eachHkInitFailSuite :: Suite
eachHkInitFailSuite =
  [ PB.Hook
      (NodePath "WebDriverDemo" "before once")
      nothingOnceBefore
      [ PB.Hook
          (NodePath "WebDriverDemo" "before inner once")
          pureOnceErrorHook
          [ PB.Hook
              (NodePath "WebDriverDemo" "each hook")
              eachHook
              [Fixture (NodePath "WebDriverDemo" "fixture") $ fxLogMessage False]
          ]
      ]
  ]

------------------------------------------------------

eachHkFailDemoNoRead :: IO ()
eachHkFailDemoNoRead = runIODemo $ eachHkFail False

-- $> eachHkFailDemoNoRead
-- runs effect but does not throw error because test does not use hook input

------------------------------------------------------

eachHkFailDemoRead :: IO ()
eachHkFailDemoRead = runIODemo $ eachHkFail True

-- $> eachHkFailDemoRead
-- runs effect and throws error because test uses hook output
-- fails in the hook, not initialisation, because there is no caching of 
-- the hook output in each hooks

eachHkFail :: Bool -> Suite
eachHkFail readStatus =
  [ PB.Hook
      (NodePath "WebDriverDemo" "before once")
      nothingOnceBefore
      [ PB.Hook
          (NodePath "WebDriverDemo" "each hook Gunna Blow")
          eachFailureHook
          [Fixture (NodePath "WebDriverDemo" "fixture") $ fxLogMessage readStatus]
      ]
  ]

------------------------------------------------------
------------------------------------------------------

config :: FixtureConfig
config = FxCfg "test" DeepRegression

fxLogMessage :: Bool -> Fixture DriverStatus
fxLogMessage readStatus = Full' config pureOnceErrorHook (action' readStatus) parse items

--- Hook ---

nothingOnceBefore :: PB.Hook Once Before () ()
nothingOnceBefore =
  BeforeHook
    { action = \_rc -> do
        log "This is the outer hook"
    }

pureOnceErrorHook :: PB.Hook Once Before () DriverStatus
pureOnceErrorHook =
  BeforeHook'
    { depends = nothingOnceBefore,
      action' = pureError "This is the inner hook" "BANG !!!! pureErrorHook Hook failed !!!"
    }

pureThreadErrorHook :: PB.Hook Once Before () DriverStatus
pureThreadErrorHook =
  BeforeHook'
    { depends = nothingOnceBefore,
      action' = pureError "This is the Thread hook" "BANG !!!! pureThreadErrorHook Hook failed !!!"
    }

eachHook :: PB.Hook Each Before DriverStatus DriverStatus
eachHook =
  BeforeHook'
    { depends = pureOnceErrorHook,
      action' = \_rc ds -> do
        log "This is each hook"
        pure ds
    }


eachFailureHook :: PB.Hook Each Before () DriverStatus
eachFailureHook =
  BeforeHook'
    { depends = nothingOnceBefore,
      action' =  \_rc _ds -> do 
        log "This is each hook" 
        pure $ error "BANG !!!! eachFailureHook Hook failed !!!"
    }

pureError :: (Out NodeLog :> es) => Text -> Text -> rc -> i -> Eff es b
pureError logMsg errMsg _rc _i = do
  log logMsg
  pure $ error errMsg

action' :: (Out NodeLog :> es) => Bool -> RunConfig -> DriverStatus -> Data -> Eff es AS
action' readStatus _rc hookDriverStatus itm = do
  when readStatus $
    log $ "Reading status: " <> txt hookDriverStatus

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

-- ################### Simplified Laziness (IO no test framework or effect system) ##################

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
