module WebDriverIOInterpreter where

  
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
import Effectful.Reader.Dynamic
import Effectful.Dispatch.Dynamic (
  interpret
 )
import Effectful.TH (makeEffect)
import WebDriverEffect

runWebDriverIO :: forall es a. ( IOE :> es) => Eff (WebUI : es) a -> Eff es a
runWebDriverIO =
  interpret $ \_ ->
    EF.liftIO . \case
      Click name -> T.putStrLn $ "Click " <> name
      Go name -> T.putStrLn $ "Go " <> name
      Read name -> undefined -- T.putStrLn $ "Read " <> name

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

-- Effect


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
