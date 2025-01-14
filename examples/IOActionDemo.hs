module IOActionDemo where

import Effectful as EF
  ( Eff,
    IOE,
    type (:>), runEff,
  )
import IOActionDocInterpreter qualified as DI
import IOActionIOInterpreter qualified as IOI
import DSL.Internal.NodeLog (NodeLog)
import DSL.OutEffect (Out)

import IOActionEffect
  ( IOAction (..),
    ioAction,
  )

import Prelude hiding (putStrLn, getLine)
import Data.Text.IO (putStrLn, getLine)
import PyrethrumExtras.IO (pp)
import DSL.OutInterpreter (runOut)

-- IOAction effect is an escape hatch for IO effects for which there is no Eff equivalent
-- It is just IOE with a description used when running the documenter

apEventOut :: forall a es. (IOE :> es) => Eff (Out NodeLog : es) a -> Eff es a
apEventOut = runOut pp

docRun :: Eff '[IOAction, Out NodeLog, IOE] a -> IO a
docRun = runEff . apEventOut . DI.runIOAction

askName :: IOAction :> es => Eff es Text
askName = ioAction "Ask name" $ do
  putStrLn "What is your name?"
  getLine

app :: forall es. IOAction :> es => Eff es ()
app = do
  name <- askName
  ioAction "Say hello" $ putStrLn $ "Hello " <> name

-- >>> runAppDocumenter
runAppDocumenter :: IO ()
runAppDocumenter = docRun app

-- must be run in cabal repl as is interactive
runAppIO :: IO ()
runAppIO = EF.runEff $ IOI.runIOAction app
