
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

import Runner
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
import FileSystem
import AppError
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

{- Application (Interactor) -}

data ApState = ApState {
  itemId :: Int,
  filePath :: Path Abs File,
  fileText :: StrictReadResult
}
  deriving Show

data TestItem = Item {
  id :: Int,
  pre :: String,
  post :: String,
  path :: Path Abs File
}

data RunConfig = RunConfig {
  environment :: String,
  depth :: Integer,
  path :: Path Abs File
}

interactor :: Members '[Ensure, FileSystem] effs => RunConfig -> TestItem -> Eff effs ApState
interactor runConfig item = do
                              let fullFilePath = path (item :: TestItem)
                              writeFile fullFilePath $ pre item  <> " ~ " <> post item <> " !!"
                              ensure True "Blahh"
                              txt <- readFile fullFilePath
                              pure $ ApState (RoughTest.id item) fullFilePath txt

{- Application IO Interpreter -}

executeInIO :: Eff '[FileSystem, Ensure, Error AppError, IO] a -> IO (Either AppError a)
executeInIO app = runM $ runError
                       $ ensureInterpreter
                       $ fileSystemIOInterpreter
                       app

{- Demo Execution -}

sampleItem =  Item {
  id = 500,
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
demoExecuteInIO = executeInIO $ interactor sampleRunConfig sampleItem
demoIOAll = Prelude.sequenceA $ runTest sampleRunConfig interactor sampleTestItems executeInIO

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

executeDocumented :: forall a. Eff '[FileSystem, Ensure, Error AppError, Writer [String]] a -> (Either AppError a, [String])
executeDocumented app = run $ runWriter
                            $ runError
                            $ ensureInterpreter
                            $ interpret fileSystemDocInterpreter
                            app

-- Demos
demoDocument = executeDocumented $ interactor sampleRunConfig sampleItem
demoDocumentedAll = runTest sampleRunConfig interactor sampleTestItems executeDocumented


--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i = Item

sampleTestItems = [
                    i 100 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                    i 110 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                    i 120 "Pre"  "Post"   [absfile|R:\Vids\SystemDesign\Wrong.txt|],
                    i 130 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                    i 140 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                    i 150 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|]
                  ];



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
