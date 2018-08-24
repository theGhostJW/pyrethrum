{-# LANGUAGE QuasiQuotes #-}

module RoughTest where


-- full run through writing file
-- change to inmemory
-- change to document
-- through exception and check for
-- make document fail eg /0 based on get
-- validation validation module ?? ~ terminal validations
-- generalis runner


import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Data.Function                 ((&))
import           Data.Functor
import           Data.List
import           Foundation.Extended           as F hiding (putStrLn)
import           Foundation.String
import           Paths_pyrethrum
import qualified Prelude
import           System.Exit                   as SysExit hiding (ExitCode (ExitSuccess))

default (String)

data FileSystem r where
  ReadFile :: Path a File -> FileSystem String
  WriteFile :: Path a File -> String -> FileSystem ()

readFile :: Member FileSystem effs => Path a File -> Eff effs String
readFile = send . ReadFile

writeFile :: Member FileSystem effs => Path a File -> String -> Eff effs ()
writeFile pth = send . WriteFile pth

fileSystemInterpreter :: FileSystem a -> IO a
fileSystemInterpreter = \case
                           ReadFile path -> F.readFile path
                           WriteFile path str -> F.writeFile path str

documentationInterpreter :: FileSystem a -> IO a
documentationInterpreter = \case
                          ReadFile path -> F.readFile path
                          WriteFile path str -> F.writeFile path str

dummy :: Path Abs File
dummy = [absfile|C:\home\chris\x.txt|]

dir = "C:\\home\\chris\\x.txt"

dummy2 = mkAbsFile "C:\\home\\chris\\x.txt"
