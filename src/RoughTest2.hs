
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module RoughTest2 where


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
import Ensure
import           Control.Monad.Trans.Either.Exit (orDie)
import qualified Prelude
import           System.Exit                   as SysExit hiding (ExitCode (ExitSuccess))
import           System.IO                       (FilePath, IO,
                                                  IOMode (ReadMode, WriteMode),
                                                  withFile)
import           System.IO.Error                 (isAlreadyInUseError,
                                                  isDoesNotExistError,
                                                  isPermissionError)
import Control.Exception

default (String)

{- File System Lang -}

data FileSystem r where
  ReadFile :: Path a File -> FileSystem StrictReadResult
  WriteFile :: Path a File -> String -> FileSystem ()

readFile :: Member FileSystem effs => Path a File -> Eff effs StrictReadResult
readFile = send . ReadFile

writeFile :: Member FileSystem effs => Path a File -> String -> Eff effs ()
writeFile pth = send . WriteFile pth

{- File System IO Interpreter -}

fileSystemIOInterpreter :: forall effs a. LastMember IO effs => Eff (FileSystem ': effs) a -> Eff effs a
fileSystemIOInterpreter = interpretM $ \case
                               ReadFile path -> F.readFileUTF8 path
                               WriteFile path str -> F.writeFileUTF8 path str

{- Application (Interactor) -}

data ApState = ApState {
  filePath :: Path Abs File,
  fileText :: StrictReadResult
}
  deriving Show

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

interactor :: Members '[FileSystem] effs => TestItem -> RunConfig -> Eff effs ApState
interactor item runConfig = do
                              let fullFilePath = path (runConfig :: RunConfig)
                              writeFile fullFilePath $ pre item  <> " ~ " <> post item <> " !!"
                              txt <- readFile [absfile|C:\Vids\SystemDesign\Wrong.txt|]
                              pure $ ApState fullFilePath txt

{- Application IO Interpreter -}

executeInIO :: Eff '[FileSystem, Ensure String, Error String, IO] a -> IO (Either String a)
executeInIO app = runM $ runError
                       $ ensureInterpreter
                       $ fileSystemIOInterpreter
                       app

{- Demo Execution -}

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

-- Demos
demoExecuteInIO = executeInIO $ interactor sampleItem sampleRunConfig

fileSystemDocInterpreter :: Member (Writer [String]) effs => FileSystem ~> Eff effs
fileSystemDocInterpreter =  let
                              mockContents = "Mock File Contents"
                            in
                              \case
                                ReadFile path -> tell ["readFile: " <> show path] $> Right mockContents
                                WriteFile path str -> tell ["write file: " <>
                                                              show path <>
                                                              "\nContents:\n" <>
                                                              str]

executeDocumented :: forall a. Eff '[FileSystem, Ensure String, Error String, Writer [String]] a -> (Either String a, [String])
executeDocumented app = run $ runWriter
                            $ runError
                            $ ensureInterpreter
                            $ interpret fileSystemDocInterpreter
                            app

-- Demos
demoDocument =  executeDocumented $ interactor sampleItem sampleRunConfig

--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

r = RunConfig
null = Nothing

sampleTestItems = [
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                      r "Test"   55   [absfile|C:\Vids\SystemDesign\VidList.txt|]
  ]


  --- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  --- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  --- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data FileErrorType
  = AlreadyInUse
  | DoesNotExist
  | PermissionError

data FileError
  = ReadFileError FilePath FileErrorType
  | WriteFileError FilePath FileErrorType

newtype UserError
  = UserError String

selectFileError :: IOException -> IO FileErrorType
selectFileError e | isAlreadyInUseError e = return AlreadyInUse
                  | isDoesNotExistError e = return DoesNotExist
                  | isPermissionError e   = return PermissionError
                  | otherwise             = throwIO e
