
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
-- import Control.Exception
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
import           System.IO                       (FilePath, IO,
                                                  IOMode (ReadMode, WriteMode),
                                                  withFile)
import           System.IO.Error                 (isAlreadyInUseError,
                                                  isDoesNotExistError,
                                                  isPermissionError)
import Control.Exception

default (String)

data FileSystem r where
  ReadFile :: Path a File -> FileSystem StrictReadResult
  WriteFile :: Path a File -> String -> FileSystem ()

readFile :: Member FileSystem effs => Path a File -> Eff effs StrictReadResult
readFile = send . ReadFile

writeFile :: Member FileSystem effs => Path a File -> String -> Eff effs ()
writeFile pth = send . WriteFile pth

-- throw native error here
fileSystemIOInterpreter :: forall effs a. (Member (Error AppError) effs, LastMember IO effs) => Eff (FileSystem ': effs) a -> Eff effs a
fileSystemIOInterpreter = interpretM $ \case
                               ReadFile path -> F.readFileUTF8 path
                               WriteFile path str -> F.writeFileUTF8 path str


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

data AppEnsure r where
  Ensure :: Bool -> String -> AppEnsure ()
  FailEn :: String -> AppEnsure ()

ensure :: Member AppEnsure effs => Bool -> String -> Eff effs ()
ensure condition message = send $ Ensure condition message

failEn :: Member AppEnsure effs =>  String -> Eff effs ()
failEn = send . FailEn

errorDocInterpreter :: Member (Writer [String]) effs => AppEnsure ~> Eff effs
errorDocInterpreter = \case
                    Ensure condition errMsg -> tell [condition ? "Ensure Check Passed" $
                      "Ensure Check Failed ~ " <>  errMsg]
                    FailEn errMsg -> tell ["Failure ~ " <>  errMsg]


errorIOInterpreter :: forall effs a. LastMember IO effs => Eff (AppEnsure ': effs) a -> Eff effs a
errorIOInterpreter = interpretM $ \case
                                  Ensure condition errMsg -> Monad.unless condition $ Monad.fail $ toList errMsg
                                  FailEn errMsg -> Monad.fail $ toList errMsg
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

newtype AppError =
  GeneralError String
 deriving Show


interactor :: Members '[AppEnsure, FileSystem, Error AppError] effs => TestItem -> RunConfig -> Eff effs ApState
interactor item runConfig = do
                              let fullFilePath = path (runConfig :: RunConfig)
                              writeFile fullFilePath $ pre item  <> " ~ " <> post item <> " !!"
                              -- failEn "random error ~ its a glitch"
                              -- throwError $ GeneralError "Blahh"
                              --txt <- readFile [absfile|C:\Vids\SystemDesign\Wrong.txt|]
                              txt <- readFile fullFilePath
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

executeDocumented :: forall a. Eff '[FileSystem, AppEnsure, Writer [String], Error AppError] a -> Either AppError (a, [String])
executeDocumented app = run $ runError
                            $ runWriter
                            $ interpret errorDocInterpreter
                            $ interpret fileSystemDocInterpreter
                            app

executeInIO :: forall a. Eff '[FileSystem, AppEnsure, Error AppError, IO] a -> IO (Either AppError a)
executeInIO app = runM $ runError
                       $ errorIOInterpreter
                       $ fileSystemIOInterpreter
                       app

-- Demos
demoExecuteInIO = executeInIO $ interactor sampleItem sampleRunConfig
demoDocument = executeDocumented $ interactor sampleItem sampleRunConfig


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

-- readFileEth :: Path a File -> EitherT FileError IO String
-- readFileEth filePath =
--   let
--     errorHandler e = Left . ReadFileError (toFilePath filePath) <$> selectFileError e
--   in
--     newEitherT $ catch (Right <$> F.readFile filePath) errorHandler
--
-- writeFileEth :: Path a File -> String -> EitherT FileError IO ()
-- writeFileEth filePath contents =
--   let
--     errorHandler e = Left . WriteFileError (toFilePath filePath) <$> selectFileError e
--   in
--     newEitherT $ catch (F.writeFile filePath contents >> pure (Right ())) errorHandler

-- data AppError = FileError (Path Abs File) String |
--                 InteractorError String

-- renderAppError :: AppError -> String
-- renderAppError (FileError pth msg) = "Problem with file: " <> toStr (toFilePath pth) <> " " <> msg
-- renderAppError (AppLineError e)      = renderLineError e
-- renderAppError (AppNormaliseError e) = renderNormaliseError e
