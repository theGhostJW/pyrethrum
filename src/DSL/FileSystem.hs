module DSL.FileSystem where

import Common
import           Pyrelude as P
import           Data.Functor
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           Control.Exception as E
import           DSL.Logger
import           DSL.LogProtocol

{- File System Lang -}

data FileSystem r where
  ReadFile :: Path a File -> FileSystem Text
  WriteFile :: Path a File -> Text -> FileSystem ()

readFile :: Member FileSystem effs => Path a File -> Eff effs Text
readFile = send . ReadFile

writeFile :: Member FileSystem effs => Path a File -> Text -> Eff effs ()
writeFile pth = send . WriteFile pth

{- File System IO Interpreter -}

fileSystemIOInterpreter :: Members '[Error FileSystemError, IO] effs => Eff (FileSystem ': effs) a -> Eff effs a
fileSystemIOInterpreter =
                          let
                            handleException action handler = do
                                                               r <- send (E.try action)
                                                               case r of
                                                                 Left (e :: IOException) -> throwError (handler e)
                                                                 Right f -> pure f
                           in
                            interpret $ \case
                                          ReadFile path -> handleException (P.readFile $ toFilePath path) ReadFileError
                                          WriteFile path str -> handleException (P.writeFile (toFilePath path) str) WriteFileError



fileSystemDocInterpreter :: Member Logger effs => Eff (FileSystem ': effs) a -> Eff effs a
fileSystemDocInterpreter = interpret $
                                      let
                                        mockContents :: Text
                                        mockContents = "Mock File Contents"
                                      in
                                        \case
                                          ReadFile path ->
                                            logItem (DocAction $ ActionInfo ("readFile: " <> txt path) ) $> mockContents

                                          WriteFile path str -> logItem . DocAction $ ActionInfoM 
                                                                                        ("write file: " <> txt path)
                                                                                        $ "contents:\n" <> str
