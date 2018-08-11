
module InteractorSpike where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           System.Exit                hiding (ExitCode (ExitSuccess))
import Foundation.Extension

-- Example: Console DSL:  https://github.com/lexi-lambda/freer-simple

--------------------------------------------------------------------------------
                               -- Effect Model --
--------------------------------------------------------------------------------

data Console r where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()

putStrLn :: Member Console effs => String -> Eff effs ()
putStrLn = send . PutStrLn

getLine :: Member Console effs => Eff effs String
getLine = send GetLine

exitSuccess :: Member Console effs => Eff effs ()
exitSuccess = send ExitSuccess
