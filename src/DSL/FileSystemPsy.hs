module DSL.FileSystemPsy where

import Common
import Control.Exception as E
import DSL.LogProtocol (LogProtocolBase (IOAction'))
import DSL.LoggerPsy (Logger, logAction, logItem)
import qualified Data.Aeson as A
import Path (File, Path, toFilePath)
import Polysemy (
  Embed,
  Member,
  Members,
  Sem,
  embed,
  interpret,
  makeSem,
 )
import Polysemy.Error as PE (Error, throw)
import PyrethrumExtras (txt)
import PyrethrumExtras.IO as PO
import Prelude as P
import Data.Text.IO as TIO (putStrLn, writeFile, readFile)

{- File System Lang -}

data FileSystem m r where
  ReadFile :: Path a File -> FileSystem m Text
  WriteFile :: Path a File -> Text -> FileSystem m ()

makeSem ''FileSystem

-- {- File System IO Interpreter -}

fileSystemIOInterpreter :: forall a e effs. (Members '[Error (FrameworkError e), Embed IO] effs) => Sem (FileSystem ': effs) a -> Sem effs a
fileSystemIOInterpreter =
  let
    handleException :: forall b. IO b -> (IOException -> FrameworkError e) -> Sem effs b
    handleException action handler =
      embed (E.try action)
        >>= \case
          Left (e :: IOException) -> PE.throw (handler e)
          Right f -> pure f
   in
    interpret $ \case
      ReadFile path -> handleException (TIO.readFile $ toFilePath path) (FileSystemError ReadFileError)
      WriteFile path str -> handleException (TIO.writeFile (toFilePath path) str) (FileSystemError WriteFileError)

fileSystemDocInterpreter :: forall a e effs. (Show e, A.ToJSON e, Member (Logger e) effs) => Sem (FileSystem ': effs) a -> Sem effs a
fileSystemDocInterpreter =
  interpret $
    let
      mockContents :: Text
      mockContents = "Mock File Contents"
     in
      \case
        ReadFile path ->
          logAction ("readFile: " <> txt path) $> mockContents
        WriteFile path str ->
          logItem . IOAction'
            $ DetailedInfo
              ("write file: " <> txt path)
            $ "contents:\n" <> str

