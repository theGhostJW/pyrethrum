{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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
import           Control.Monad.Freer.Reader
import           Data.Function                 ((&))
import           Data.Functor
import           Data.List
import           Foundation.Extended           hiding (putStrLn, readFile, writeFile)
import    qualified       Foundation.Extended  as F
import           Foundation.String
import           Paths_pyrethrum
import qualified Prelude
import           System.Exit                   as SysExit hiding (ExitCode (ExitSuccess))

default (String)

data TestItem = Item {
  pre :: String,
  post :: String,
  path :: Path Abs File
}

data RunConfig = RunConfig {
  environment :: String,
  depth :: Integer,
  path :: Path Abs File
}

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

documentationInterpreter :: FileSystem ~> Eff '[Writer [String], Reader TestItem, Reader RunConfig]
documentationInterpreter =  let
                              mockContents = "Mock File Contents"
                            in
                              \case
                                ReadFile path -> tell ["readFile: " <> show path] $> mockContents
                                WriteFile path str -> tell ["write file: " <>
                                                              show path <>
                                                              "\nContents:\n" <>
                                                              str]

data ApState = ApState {
  filePath :: Path Abs File,
  fileText :: String
}
  deriving Show

interactor :: TestItem -> RunConfig -> (Member FileSystem r) => Eff r ApState
interactor item runConfig = do
                              let fullFilePath = path (runConfig :: RunConfig)
                              writeFile fullFilePath $ pre item  <> post item
                              txt <- readFile [absfile|C:\Vids\SystemDesign\VidL.txt|]
                              pure $ ApState fullFilePath txt

sampleItem =  Item {
  pre = "I do a test",
  post = "the test runs",
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|]
}

sampleRunConfig = RunConfig {
  environment = "Test",
  depth = 44,
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|]
}

executeDocumented :: forall a. Eff '[FileSystem, Reader TestItem, Reader RunConfig] a -> (a, [String])
executeDocumented app = run $ runReader sampleRunConfig $ runReader sampleItem $ runWriter $ reinterpret documentationInterpreter app

executeFileSystem :: forall a. Eff '[FileSystem, IO] a -> IO a
executeFileSystem app = runM $ interpretM fileSystemInterpreter app

demoDocument = executeDocumented $ interactor sampleItem sampleRunConfig
demoIO = executeFileSystem $ interactor sampleItem sampleRunConfig
