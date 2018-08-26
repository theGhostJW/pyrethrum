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

import qualified Control.Monad as Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Control.Monad.Freer.Reader
import           Data.Function                 ((&))
import           Data.Functor
import           Data.List
import           Foundation.Extended           hiding (putStrLn, readFile, writeFile, fail)
import    qualified       Foundation.Extended  as F
import           Foundation.String
import           Paths_pyrethrum
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Either.Exit (orDie)
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

fileSystemIOInterpreter :: forall effs a. LastMember IO effs => Eff (FileSystem ': effs) a -> Eff effs a
fileSystemIOInterpreter = interpretM $ \case
                               ReadFile path -> F.readFile path
                               WriteFile path str -> F.writeFile path str

fileSystemDocInterpreter :: Member (Writer [String]) effs => FileSystem ~> Eff effs
fileSystemDocInterpreter =  let
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

data AppEnsure r where
  Ensure :: Bool -> String -> AppEnsure ()
  Fail :: String -> AppEnsure ()

ensure :: Member AppEnsure effs => Bool -> String -> Eff effs ()
ensure condition message = send $ Ensure condition message

fail :: Member AppEnsure effs =>  String -> Eff effs ()
fail = send . Fail

errorDocInterpreter :: Member (Writer [String]) effs => AppEnsure ~> Eff effs
errorDocInterpreter = \case
                        Ensure condition errMsg -> tell [condition ? "Ensure Check Passed" $
                          "Ensure Check Failed ~ " <>  errMsg]
                        Fail errMsg -> tell ["Failure ~ " <>  errMsg]


-- errorIOInterpreter :: Member IO effs => AppEnsure ~> Eff effs
-- errorIOInterpreter = \case
--                         Ensure condition errMsg -> pure $ Monad.unless condition error errMsg
--                         Fail errMsg -> pure $ error errMsg



errorIOInterpreter :: forall effs a. LastMember IO effs => Eff (AppEnsure ': effs) a -> Eff effs a
errorIOInterpreter = interpretM $ \case
                                      Ensure condition errMsg -> Monad.unless condition $ Monad.fail $ toList errMsg
                                      Fail errMsg -> Monad.fail $ toList errMsg


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

newtype AppError = FileError String

--
-- renderAppError :: AppError -> Text
-- renderAppError (AppFileError e)      = renderFileError e
-- renderAppError (AppLineError e)      = renderLineError e
-- renderAppError (AppNormaliseError e) = renderNormaliseError e

interactor :: Members '[AppEnsure, FileSystem] effs => TestItem -> RunConfig -> Eff effs ApState
interactor item runConfig = do
                              let fullFilePath = path (runConfig :: RunConfig)
                              writeFile fullFilePath $ pre item  <> post item
                              -- fail "random error ~ its a glitch"
                              txt <- readFile [absfile|C:\Vids\SystemDesign\Wrong.txt|]
                              --txt <- readFile fullFilePath
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

r = RunConfig
null = Nothing

dEnv = "Test"
dDepth = 44 :: Integer
dPath = [absfile|C:\Vids\SystemDesign\VidList.txt|]

sampleRunConfig1 = [
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|]
  ]

executeDocumented :: forall a. Eff '[FileSystem, AppEnsure, Writer [String]] a -> (a, [String])
executeDocumented app = run $ runWriter
                            $ interpret errorDocInterpreter
                            $ interpret fileSystemDocInterpreter
                            app

executeInIO :: forall a. Eff '[FileSystem, AppEnsure, IO] a -> IO a
executeInIO app = runM $ errorIOInterpreter $ fileSystemIOInterpreter app

demoDocument = executeDocumented $ interactor sampleItem sampleRunConfig
demoExecuteInIO = executeInIO $ interactor sampleItem sampleRunConfig
