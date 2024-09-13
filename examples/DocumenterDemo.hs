module DocumenterDemo where

import DSL.Internal.NodeEvent (NodeEvent (User), UserLog (Log))
import DSL.Out (Out, out)
import Effectful as EF
  ( Eff,
    IOE,
    runEff,
    type (:>),
  )
import PyrethrumBase
import PyrethrumExtras (txt, Abs, File, relfile, (?), toS)
import WebDriverSpec (Selector (CSS))
import Filter (Filters(..))
import Internal.SuiteRuntime (ThreadCount(..))
import Internal.Logging qualified as L
import DSL.FileSystemEffect
import Path as P (Path, reldir, toFilePath) 
import Data.Text (isInfixOf)
import FileSystemDocDemo (FSOut)
import qualified DSL.FileSystemDocInterpreter as FDoc

{-
demo the following:
  - single test suite with minimal selenium interpreter
  - read a value from "the internet"
  - navigate between pages
  - read a second value
  - validator on value
  - expect issue with laziness (if not why not)
      - solve
  - user steps
  - run with documenter
  - introduce action that uses value read from the internet
    - should blow up documenter
    - fix with doc* functions
  - TODO: Haddock docs for steps
    - effectful supports generating template haskell without type signature
    - manually add type signature and haddock
-}

-- ################### Effectful Demo ##################

_theInternet :: Text
_theInternet = "https://the-internet.herokuapp.com/"

_checkBoxesLinkCss :: Selector
_checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

runDemo :: SuiteRunner -> Suite -> IO ()
runDemo runner suite = do 
  (logControls, _logQ) <- L.testLogControls True
  runner suite Unfiltered defaultRunConfig (ThreadCount 1) logControls

-- ############### Test Case ###################

-- TODO: repeated code - refactor
logShow :: (HasLog es, Show a) => a -> Eff es ()
logShow = out . User . Log . txt

log :: (HasLog es) => Text -> Eff es ()
log = out . User . Log


-- copied from FileSystemDocDemo.hs

getPaths :: (Out NodeEvent :> es, FileSystem :> es) => Eff es [P.Path Abs File]
getPaths =
  do
    log "Getting paths"
    s <- findFilesWith isDeleteMe [[reldir|chris|]] [relfile|foo.txt|]
    r <- test' s
    log $ r ? "yes" $ "no"
    pure s
  where
    test' :: [P.Path Abs File] -> Eff es Bool
    test' _ignored = pure True
    isDeleteMe :: P.Path Abs File -> Eff es Bool
    isDeleteMe = pure . isInfixOf "deleteMe" . toS . P.toFilePath

-- #### 1. This has the behaviour we are after with a simple local interpreter

fsDemoAp :: forall es. (FSOut es) => Eff es ()
fsDemoAp = do
  paths <- getPaths
  log . txt $ length paths
  chk paths
  where
    chk :: [P.Path Abs File] -> Eff es ()
    chk _ = log "This is a check"

fsDocDemoSimple :: IO ()
fsDocDemoSimple = 
  --  docInterpreter fsDemoAp
  docRun fsDemoAp
  where 
    docRun :: Eff '[FileSystem, Out NodeEvent, IOE] a -> IO a
    docRun = runEff . runDocOut . FDoc.runFileSystem
-- >>> fsDocDemoSimple

{- TODO: make documenter work with this (copy of Webdriver demo)

webDriverSuite :: Suite
webDriverSuite =
  [Fixture (NodePath "WebDriverDemo" "test") test]

runWebDriverDemo :: SuiteRunner -> IO ()
runWebDriverDemo runner = do 
  (logControls, _logQ) <- L.testLogControls True
  runner webDriverSuite Unfiltered defaultRunConfig (ThreadCount 1) logControls
  
test :: Fixture ()
test = Full config action parse items

config :: FixtureConfig
config = FxCfg "test" DeepRegression

driver_status :: (WebUI :> es, Out NodeEvent :> es) => Eff es DriverStatus
driver_status = do 
  status <- driverStatus "NA"
  log $ "the driver status is: " <> txt status
  pure status

runIODemo :: IO ()
runIODemo = runDemo ioRunner
-- >>> runIODemo


action :: (WebUI :> es, Out NodeEvent :> es, FileSystem :> es) => RunConfig -> Data -> Eff es AS
action _rc i = do
  log i.title
  status <- driver_status
  log $ "the driver status is: " <> txt status
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

-}
