module DSL.FileSystem where

import qualified Data.Aeson as A
import Common
import           Prelude as P
import           PyrethrumExtras.IO as PO
import           Control.Exception as E
import           DSL.Logger
import           DSL.LogProtocol
import Polysemy
import Polysemy.Error as PE
import Path
import PyrethrumExtras

{- File System Lang -}

data FileSystem m r where
  ReadFile :: Path a File -> FileSystem m Text
  WriteFile :: Path a File -> Text -> FileSystem m ()

makeSem ''FileSystem

-- {- File System IO Interpreter -}

fileSystemIOInterpreter :: forall a e effs. Members '[Error (FrameworkError e), Embed IO] effs => Sem (FileSystem ': effs) a -> Sem effs a
fileSystemIOInterpreter =
  let
    handleException :: forall b. IO b  -> (IOException -> FrameworkError e) -> Sem effs b
    handleException action handler = do
                                        r <- embed (E.try action)
                                        case r of
                                          Left (e :: IOException) -> PE.throw (handler e)
                                          Right f -> pure f
  in
    interpret $ \case
                  ReadFile path -> handleException (PO.readFile $ toFilePath path) (FileSystemError ReadFileError)
                  WriteFile path str -> handleException (PO.writeFile (toFilePath path) str) (FileSystemError WriteFileError)

fileSystemDocInterpreter :: forall a e effs. (Show e, A.ToJSON e, Member (Logger e) effs) => Sem (FileSystem ': effs) a -> Sem effs a
fileSystemDocInterpreter = interpret $
                                      let
                                        mockContents :: Text
                                        mockContents = "Mock File Contents"
                                      in
                                        \case
                                          ReadFile path ->
                                            logAction ("readFile: " <> txt path) $> mockContents

                                          WriteFile path str -> logItem  . IOAction' $ DetailedInfo 
                                                                                        ("write file: " <> txt path)
                                                                                        $ "contents:\n" <> str
