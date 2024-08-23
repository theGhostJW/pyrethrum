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
1. revisit monad transformers step by step DONE
2. relook at HttpTT
3. relook at Eff
  - THIS !!! :: https://hackage.haskell.org/package/effectful-core-2.3.0.1/docs/Effectful-Dispatch-Dynamic.html#g:4
  - esp lifting
4. try find examples

Control.Monad.Script.Http

A basic type and monad transformer transformer for describing HTTP interactions.

data HttpTT e r w s p t eff a 

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

type instance DispatchOf WebUI = Dynamic

data WebUI :: Effect where
  Click :: Text -> WebUI m ()
  Go :: Text -> WebUI m ()
  Read :: Text -> WebUI m Text

makeEffect ''WebUI

-- start geckodriver first
-- geckodriver &

release_the_bats :: WebDriverT IO ()
release_the_bats = do
  fullscreenWindow
  navigateTo "https://www.google.com"
  performActions [typeString "bats"]
  performActions [press EnterKey]
  wait 5000000
  pure ()

  
-- $ > example1

-- >>> example1
example1 :: IO ()
example1 = do
  execWebDriverT defaultWebDriverConfig
    (runIsolated_ defaultFirefoxCapabilities release_the_bats)
  pure ()

