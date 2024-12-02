module DocumenterDemo where

import Check
import Core (Before, Once, ParseException)
import DSL.FileSystemEffect
import DSL.Internal.NodeLog (NodeLog, Path (NodePath))
import DSL.Logging ( log )
import DSL.OutEffect (Out)
import Data.Text (isInfixOf)
import Effectful as EF
  ( Eff,
    type (:>),
  )
import Filter (Filters (..))
import Internal.Logging qualified as L
import Internal.SuiteRuntime (ThreadCount (..))
import Path as P (Path, reldir, toFilePath)
import PyrethrumBase
  ( DataSource (..),
    Depth (..),
    Fixture,
    FixtureConfig (..),
    Hook (..),
    Node (..),
    RunConfig (..),
    Suite,
    SuiteRunner,
    defaultRunConfig,
    docRunner, mkFull, mkFull',
  )
import PyrethrumExtras (Abs, File, relfile, toS, txt, (?))
import WebDriverEffect
  ( WebUI,
    clickElem,
    findElem,
    go,
    killSession,
    maximiseWindow,
    newSession,
    readElem,
    sleep,
  )
import WebDriverPure (seconds)
import WebDriverSpec (DriverStatus (..), Selector (CSS))

runDemo :: SuiteRunner -> Suite -> IO ()
runDemo runner suite = do
  (logControls, _logList) <- L.testLogActions True
  runner suite Unfiltered defaultRunConfig (ThreadCount 1) logControls

-- putStrLn "########## Log ##########"
-- atomically logList >>= mapM_ pPrint

docDemo :: Bool -> Bool -> Suite -> IO ()
docDemo stp chks = runDemo $ docRunner stp chks

-- ############### Test Case ###################

-- copied from FileSystemDocDemo.hs

getPaths :: (Out NodeLog :> es, FileSystem :> es) => Eff es [P.Path Abs File]
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

chkPathsThatDoesNothing :: [P.Path Abs File] -> Eff es ()
chkPathsThatDoesNothing _ = pure ()

fsDemoAp :: forall es. (Out NodeLog :> es, FileSystem :> es) => Eff es ()
fsDemoAp = do
  paths <- getPaths
  chkPathsThatDoesNothing paths

-- ################### 1. FS App with full runtime ##################

fsSuiteDemo :: IO ()
fsSuiteDemo = docDemo True True fsSuite

-- >>> fsSuiteDemo

-- TODO: fix filter log
fsSuite :: Suite
fsSuite =
  [Fixture (NodePath "FS Demo Test" "test") fstest]

fstest :: Fixture ()
fstest = mkFull config fsAction parsefs fsItems

getFailNested :: Eff es FSAS
getFailNested = pure $ error "This is a nested error !!! "

getFail :: Eff es FSAS
getFail = error "This is an error !!! "

fsAction :: (FileSystem :> es, Out NodeLog :> es) => RunConfig -> FSData -> Eff es FSAS
fsAction _rc i = do
  getFailNested
  -- getFail
  paths <- getPaths
  log i.title
  chkPathsThatDoesNothing paths
  log "Paths checked ~ not really"
  pure $ FSAS {paths}

data FSData = FSItem
  { id :: Int,
    title :: Text,
    checks :: Checks FSDS
  }
  deriving (Show, Read)

{-
TODO: make better compile error example
data FSData = FSItem
  { id :: Int,
    title :: Text,
    checks :: Checks DS
  }
  deriving (Show, Read)
-}

newtype FSAS = FSAS
  { paths :: [P.Path Abs File]
  }
  deriving (Show)

newtype FSDS = FSDS
  { paths :: [P.Path Abs File]
  }
  deriving (Show)

parsefs :: FSAS -> Either ParseException FSDS
parsefs FSAS {..} = pure $ FSDS {..}

fsItems :: RunConfig -> DataSource FSData
fsItems _rc =
  Items
    [ FSItem
        { id = 1,
          title = "test the file system",
          checks =
            chk "Paths exist" (not . null . (.paths))
        }
    ]

-- ################### WebDriver Test ##################

baseWdDemo :: Bool -> Bool -> IO ()
baseWdDemo stp chks = docDemo stp chks webDriverSuite

fullDocWebdriverDemo :: IO ()
fullDocWebdriverDemo = baseWdDemo True True

-- >>> fullDocWebdriverDemo

chksDocWebdriverDemo :: IO ()
chksDocWebdriverDemo = baseWdDemo False True

-- >>> chksDocWebdriverDemo

stepsDocWebdriverDemo :: IO ()
stepsDocWebdriverDemo = baseWdDemo True False

-- >>> stepsDocWebdriverDemo

titlesWebdriverDemo :: IO ()
titlesWebdriverDemo = baseWdDemo False False

-- >>> titlesWebdriverDemo

-- TODO:
--  - add tests
--  - play with hook data objects and laziness

webDriverSuite :: Suite
webDriverSuite =
  [ Hook
      (NodePath "WebDriverDemo" "before")
      nothingBefore
      [ Hook
          (NodePath "WebDriverDemo" "beforeInner")
          intOnceHook
          [ Fixture (NodePath "WebDriverDemo" "test") test
          ]
      ]
  ]


--- Hook ---

nothingBefore :: Hook Once Before () ()
nothingBefore =
  BeforeHook
    { action = \_rc -> do
        log "This is the outer hook"
        log "Run once before the test"
    }

intOnceHook :: Hook Once Before () Int
intOnceHook =
  BeforeHook'
    { depends = nothingBefore,
      action' = \_rc _void -> do
        log "This is the inner hook"
        log "Run once before the test"
        pure 8
    }

--- Fixture ---

test :: Fixture Int
test = mkFull' config intOnceHook action parse dataSource

config :: FixtureConfig
config = FxCfg "test" DeepRegression

-- driver_status :: (WebUI :> es) => Eff es DriverStatus
driver_status :: Eff es DriverStatus
driver_status = pure $ error "This is a lazy error !!!"
-- driver_status = driverStatus

_theInternet :: Text
_theInternet = "https://the-internet.herokuapp.com/"

_checkBoxesLinkCss :: Selector
_checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

action :: (WebUI :> es, Out NodeLog :> es) => RunConfig -> Int -> Data -> Eff es AS
action _rc hkInt i = do
  log $ "test title is: " <> i.title
  log $ "received hook int: " <> txt hkInt <> " from the hook"
  status <- driver_status
  log "GOT DRIVER STATUS"
  -- log $ "the driver status is (from root): " <> txt status
  ses <- newSession
  log "GOT SESSION"
  maximiseWindow ses
  log "WINDOE MAXIMISED"
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

dataSource :: RunConfig -> DataSource Data
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
