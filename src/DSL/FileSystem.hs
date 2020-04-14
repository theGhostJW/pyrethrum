module DSL.FileSystem where

import Common
import           Pyrelude as P
import           Pyrelude.IO as PO
import           Control.Exception as E
import           DSL.Logger
import           DSL.LogProtocol
import Polysemy
import Polysemy.Error as PE

{- File System Lang -}

data FileSystem m r where
  ReadFile :: Path a File -> FileSystem m Text
  WriteFile :: Path a File -> Text -> FileSystem m ()

makeSem ''FileSystem

-- {- File System IO Interpreter -}

fileSystemIOInterpreter :: forall effs a. Members '[Error AppError, Embed IO] effs => Sem (FileSystem ': effs) a -> Sem effs a
fileSystemIOInterpreter =
  let
    handleException :: forall b. IO b  -> (IOException -> AppError) -> Sem effs b
    handleException action handler = do
                                        r <- embed (E.try action)
                                        case r of
                                          Left (e :: IOException) -> PE.throw (handler e)
                                          Right f -> pure f
  in
    interpret $ \case
                  ReadFile path -> handleException (PO.readFile $ toFilePath path) (FileSystemError ReadFileError)
                  WriteFile path str -> handleException (PO.writeFile (toFilePath path) str) (FileSystemError WriteFileError)

fileSystemDocInterpreter :: Member Logger effs => Sem (FileSystem ': effs) a -> Sem effs a
fileSystemDocInterpreter = interpret $
                                      let
                                        mockContents :: Text
                                        mockContents = "Mock File Contents"
                                      in
                                        \case
                                          ReadFile path ->
                                            logDocAction ("readFile: " <> txt path) $> mockContents

                                          WriteFile path str -> logItem . IterationLog . Doc . DocAction $ ActionInfo' 
                                                                                        ("write file: " <> txt path)
                                                                                        $ "contents:\n" <> str
