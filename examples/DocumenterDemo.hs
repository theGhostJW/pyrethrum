{-# LANGUAGE NoStrictData #-}

module DocumenterDemo where

import Check
import Core (ParseException)
import DSL.FileSystemDocInterpreter qualified as FDoc
import DSL.FileSystemEffect
import DSL.Internal.NodeEvent (NodeEvent (User), Path (NodePath), UserLog (Log), LogSink)
import DSL.OutEffect (Out, out)
import Data.Text (isInfixOf)
import Effectful as EF
  ( Eff,
    IOE,
    runEff,
    type (:>),
  )
import Filter (Filters (..))
import Internal.Logging qualified as L
import Internal.SuiteRuntime (ThreadCount (..))
import Text.Show.Pretty (pPrint)
import Path as P (Path, reldir, toFilePath)
import PyrethrumBase
    ( SuiteRunner,
      Suite,
      RunConfig,
      FixtureConfig(FxCfg),
      HasLog,
      Fixture(Full),
      Node(Fixture),
      DataSource(ItemList),
      Depth(DeepRegression),
      defaultRunConfig,
      docRunner )
import PyrethrumExtras (Abs, File, relfile, toS, txt, (?))
import WebDriverEffect
    ( WebUI,
      driverStatus,
      newSession,
      maximiseWindow,
      go,
      findElem,
      readElem,
      clickElem,
      sleep,
      killSession )
import WebDriverPure (seconds)
import WebDriverSpec (DriverStatus (..), Selector (CSS))
import DSL.OutInterpreter (runOut)


runDemo :: SuiteRunner -> Suite -> IO ()
runDemo runner suite = do
  (logControls, _logList) <- L.testLogControls True
  runner suite Unfiltered defaultRunConfig (ThreadCount 1) logControls
  -- putStrLn "########## Log ##########"
  -- atomically logList >>= mapM_ pPrint

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

-- ######## 1. This has the behaviour we are after with a simple local interpreter ########

chkPathsThatDoesNothing :: [P.Path Abs File] -> Eff es ()
chkPathsThatDoesNothing _ = pure ()


fsDemoAp :: forall es. (Out NodeEvent :> es, FileSystem :> es) => Eff es ()
fsDemoAp = do
  paths <- getPaths
  chkPathsThatDoesNothing paths

fsDocDemoSimple :: LogSink -> IO ()
fsDocDemoSimple sink =
  --  docInterpreter fsDemoAp
  docRun fsDemoAp
  where
    docRun :: Eff '[FileSystem, Out NodeEvent, IOE] a -> IO a
    docRun = runEff . runOut sink . FDoc.runFileSystem

-- TODO:: FIX
-- >>> fsDocDemoSimple
-- No instance for `Show (LogSink -> IO ())'
--   arising from a use of `evalPrint'
--   (maybe you haven't applied a function to enough arguments?)
-- In a stmt of an interactive GHCi command: evalPrint it_a1YYG

-- ################### 2. FS App with full runtime ##################

{-
OH THE HUMANITY !!!
1. log scrambling z:: FIXED
 1.1 - take unhandled excption out of the picture in demo - FAILED STILL SCRABLED
 1.2 - switch off filter log (execute -> executeWithoutValidation) - FAILED STILL SCRABLED
 1.3 - log outfull channel  :: FIXED with use of proper interpreter
2. exception not handled
  - reinstate exception for doc
3. laziness not working
-}

fsSuiteDemo :: IO ()
fsSuiteDemo = runDemo docRunner fsSuite

-- >>> fsSuiteDemo

-- TODO: fix filter log
fsSuite :: Suite
fsSuite =
  [Fixture (NodePath "FS Demo Test" "test") fstest]

fstest :: Fixture ()
fstest = Full config fsAction parsefs fsItems

getFail :: Eff es FSAS
getFail = pure $ error "This is an error !!! "

fsAction :: (FileSystem :> es, Out NodeEvent :> es) => RunConfig -> FSData -> Eff es FSAS
fsAction _rc i = do
  getFail
  paths <- getPaths
  log i.title
  chkPathsThatDoesNothing paths
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
  ItemList
    [ FSItem
        { id = 1,
          title = "test the file system",
          checks =
            chk "Paths exist" (not . null . (.paths))
        }
    ]


-- ################### WebDriver Test ##################

docWebDriverDemo :: IO ()
docWebDriverDemo = runDemo docRunner webDriverSuite

-- >>> docWebDriverDemo

webDriverSuite :: Suite
webDriverSuite =
  [Fixture (NodePath "WebDriverDemo" "test") test]

test :: Fixture ()
test = Full config action parse items

config :: FixtureConfig
config = FxCfg "test" DeepRegression

driver_status :: (WebUI :> es, Out NodeEvent :> es) => Eff es DriverStatus
driver_status = do
  status <- driverStatus "NA"
  log $ "the driver status is: " <> txt status
  pure status

_theInternet :: Text
_theInternet = "https://the-internet.herokuapp.com/"

_checkBoxesLinkCss :: Selector
_checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

action :: (WebUI :> es, Out NodeEvent :> es) => RunConfig -> Data -> Eff es AS
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
