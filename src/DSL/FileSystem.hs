module DSL.FileSystem where

import           Foundation.Extended
import           Data.Functor
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           Control.Exception

default (String)

{- File System Lang -}

data FileSystem r where
  ReadFile :: Path a File -> FileSystem StrictReadResult
  WriteFile :: Path a File -> String -> FileSystem ()

data FileSystemError =
    ReadFileError IOException |
    WriteFileError IOException
    deriving (Show, Eq)

readFile :: Member FileSystem effs => Path a File -> Eff effs StrictReadResult
readFile = send . ReadFile

writeFile :: Member FileSystem effs => Path a File -> String -> Eff effs ()
writeFile pth = send . WriteFile pth

{- File System IO Interpreter -}

fileSystemIOInterpreter :: forall effs a. (Members '[Error FileSystemError, IO] effs) => Eff (FileSystem ': effs) a -> Eff effs a
fileSystemIOInterpreter =
                          let
                            handleException action handler = do
                                                               r <- send (try action)
                                                               case r of
                                                                 Left (e :: IOException) -> throwError (handler e)
                                                                 Right f -> pure f
                           in
                            interpret $ \case
                                          ReadFile path -> handleException (readFileUTF8 path) ReadFileError
                                          WriteFile path str -> handleException (writeFileUTF8 path str) WriteFileError



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
