module WebDriverDemo where

  

import Data.Text.IO qualified as T
import Effectful as EF (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  liftIO,
  runEff,
  type (:>),
 )

import Web.Api.WebDriver
import Effectful.Reader.Dynamic
import Effectful.Dispatch.Dynamic (
  interpret
 )
import Effectful.TH (makeEffect)


{-
 demo the following:
  - single test suite with minimal selenium interpreter 
    - when web ui interaction is implemented it will probably use a custom / different library due to licence
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


theInternet = "https://the-internet.herokuapp.com/"

-- https://github.com/nbloomf/webdriver-w3c/blob/master/doc/Tutorial.md
-- https://hackage.haskell.org/package/webdriver-w3c

-- Effect

type instance DispatchOf WebUI = Dynamic

data WebUI :: Effect where
  Click :: Text -> WebUI m ()
  Go :: Text -> WebUI m ()
  Read :: Text -> WebUI m Text

makeEffect ''WebUI

-- Interpreters

-- runWebUI :: forall es a. ( IOE :> es) => Eff (WebUI : es) a -> Eff es a
-- runWebUI =
--   interpret $ \_ ->
--     EF.liftIO . \case
--       Hello name -> T.putStrLn $ "Hello " <> name
--       Goodbye name -> T.putStrLn $ "Goodbye " <> name

-- runWebUICasual :: forall es a. ( IOE :> es) =>Eff (WebUI : es) a -> Eff es a
-- runWebUICasual =
--   interpret $ \_ ->
--     EF.liftIO . \case
--       Hello name -> T.putStrLn $ "hi " <> name
--       Goodbye name -> T.putStrLn $ "bye " <> name


-- Basic
-- From webdriver tutorial
-- needs geckodriver running
-- TODO :: when fully implemented need to auto start driver
-- geckodriver &

release_the_bats :: WebDriverT IO ()
release_the_bats = do
  fullscreenWindow
  navigateTo "https://www.google.com"
  performActions [typeString "bats"]
  performActions [press EnterKey]
  wait 5000000
  pure ()


-- $> example1
example1 :: IO ()
example1 = do
  execWebDriverT defaultWebDriverConfig
    (runIsolated_ defaultFirefoxCapabilities release_the_bats)
  pure ()
