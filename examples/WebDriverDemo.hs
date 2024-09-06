module WebDriverDemo where

import Check
import Core (ParseException)
import DSL.Internal.NodeEvent (NodeEvent (User), Path (NodePath), UserLog (Log))
import DSL.Out (Out, out)
import Data.Text.IO qualified as T
import Debug.Trace.Extended (uu)
import Effectful as EF
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    runEff,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( interpret,
    localSeqUnlift,
    localSeqUnliftIO,
    send,
  )
import Effectful.Error.Dynamic qualified as E
import Effectful.TH (makeEffect)
import GHC.Clock (getMonotonicTime)
import Prepare (SuitePrepParams (..))
import PyrethrumBase
import PyrethrumConfigTypes (Depth (..), RunConfig (..), TestConfig (..), defaultRunConfig)
import PyrethrumExtras (txt)
import Web.Api.WebDriver as WAPI
  ( Key (EnterKey),
    WebDriverT,
    defaultFirefoxCapabilities,
    defaultWebDriverConfig,
    execWebDriverT,
    fullscreenWindow,
    navigateTo,
    performActions,
    press,
    runIsolated_,
    typeString,
    wait,
  )
import WebDriverEffect as WE
import WebDriverIOInterpreter (runWebDriver)
import WebDriverSpec (DriverStatus (Ready), ElementRef, Selector (CSS))

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

execute :: (C.Config rc, C.Config fc) => ThreadCount -> L.LogControls (L.Event L.ExePath AE.NodeEvent) (L.Log L.ExePath AE.NodeEvent) -> C.ExeParams m rc fc -> IO ()
execute tc lc p@C.ExeParams {interpreter} = -- do
  -- let suite = mkTestRun [Fixture (NodePath "WebDriverDemo" "test") test]
  -- let runParams = SuitePrepParams {suite, interpreter, runConfig = defaultRunConfig}
  -- C.execute tc lc p runParams

suite :: Suite
suite =
  [Fixture (NodePath "WebDriverDemo" "test") test]

runParams :: SuitePrepParams Action RunConfig TestConfig
runParams =
  SuitePrepParams
    { suite = mkTestRun suite,
      interpreter = actionInterpreter,
      runConfig = defaultRunConfig
    }

-- TODO: repeated code - refactor
logShow :: (HasLog es, Show a) => a -> Eff es ()
logShow = out . User . Log . txt

log :: (HasLog es) => Text -> Eff es ()
log = out . User . Log

-- ############### Test the Lot (Record) ###################

test :: Fixture ()
test = Full {config, action, parse, items}

config :: TestConfig
config = TestConfig "test" DeepRegression

action :: (WebUI :> es) => RunConfig -> Data -> Eff es AS
action _rc i = do
  log i.title
  status <- driverStatus
  log $ "the driver status is: " <> txt status
  ses <- newSession
  maximiseWindow ses
  go ses _theInternet
  link <- findElem ses _checkBoxesLinkCss
  checkButtonText <- readElem ses link
  clickElem ses link
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
              <> chk "Checkboxes text" ((== "Checkboxes") . (.checkButtonText))
        }
    ]

-- TODO nchecks
{-

mc :: DS -> [(Text, Bool)] -> Checks DS

-}

{-
_endToEnd :: IO ()
_endToEnd = do
    status' <- status
    ses <- newDefaultFirefoxSession
    maximizeWindow ses
    navigateTo ses _theInternet
    link <- findElement ses _checkBoxesLinkCss
    cbTxt <- elementText ses link
    click ses link
    deleteSession ses
    T.putStrLn ""
    T.putStrLn $ "----- " <> "Results" <> " -----"
    T.putStrLn $ txt status'
    T.putStrLn cbTxt
    T.putStrLn ""
-}

-- ################### WebDriver Example Using webdriver-w3c Library ##################

-- https://github.com/nbloomf/webdriver-w3c/blob/master/doc/Tutorial.md
-- https://hackage.haskell.org/package/webdriver-w3c

release_the_bats :: WebDriverT IO ()
release_the_bats = do
  WAPI.fullscreenWindow
  navigateTo "https://www.google.com"
  performActions [typeString "bats"]
  performActions [press EnterKey]
  wait 5000000
  pure ()

-- >>> example1
example1 :: IO ()
example1 = do
  -- start geckodriver first
  -- geckodriver &
  execWebDriverT
    defaultWebDriverConfig
    (runIsolated_ defaultFirefoxCapabilities release_the_bats)
  pure ()

--  ################### Profiling Example ##################

-- profiling demo (from eff docs)
-- timing steps
-- inline drivers
data Profiling :: Effect where
  Profile :: String -> m a -> Profiling m a

type instance DispatchOf Profiling = Dynamic

profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
profile label action' = send (Profile label action')

runProfiling :: (IOE :> es) => Eff (Profiling : es) a -> Eff es a
runProfiling = interpret $ \env -> \case
  Profile label action' -> localSeqUnliftIO env $ \unlift -> do
    t1 <- getMonotonicTime
    r <- unlift action'
    t2 <- getMonotonicTime
    putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
    pure r

runNoProfiling :: Eff (Profiling : es) a -> Eff es a
runNoProfiling = interpret $ \env -> \case
  Profile _label action' -> localSeqUnlift env $ \unlift -> unlift action'

actionP :: forall es. (Profiling :> es, IOE :> es) => Eff es ()
actionP = profile "greet" . liftIO $ putStrLn "Hello!"

profileAction :: IO ()
profileAction = runEff . runProfiling $ actionP

-- >>> profileAction
