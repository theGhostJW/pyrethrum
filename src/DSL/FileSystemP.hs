module DSL.FileSystemP where

import Common
import           Pyrelude as P
import           Pyrelude.IO as PO
import           Data.Functor
-- import           Control.Monad.Freer
-- import           Control.Monad.Freer.Error
-- import           Control.Monad.Freer.Writer
import           Control.Exception as E
import           DSL.Logger
import           DSL.LogProtocol

{- File System Lang -}

data FileSystem m r where
  ReadFile :: Path a File -> FileSystem m Text
  WriteFile :: Path a File -> Text -> FileSystem m ()

-- readFile :: Member FileSystem effs => Path a File -> Eff effs Text
-- readFile = send . ReadFile

-- writeFile :: Member FileSystem effs => Path a File -> Text -> Eff effs ()
-- writeFile pth = send . WriteFile pth

-- {- File System IO Interpreter -}

-- fileSystemIOInterpreter :: Members '[Error FileSystemError, IO] effs => Eff (FileSystem ': effs) a -> Eff effs a
-- fileSystemIOInterpreter =
--                           let
--                             handleException action handler = do
--                                                                r <- send (E.try action)
--                                                                case r of
--                                                                  Left (e :: IOException) -> throwError (handler e)
--                                                                  Right f -> pure f
--                            in
--                             interpret $ \case
--                                           ReadFile path -> handleException (PO.readFile $ toFilePath path) ReadFileError
--                                           WriteFile path str -> handleException (PO.writeFile (toFilePath path) str) WriteFileError

-- fileSystemDocInterpreter :: Member Logger effs => Eff (FileSystem ': effs) a -> Eff effs a
-- fileSystemDocInterpreter = interpret $
--                                       let
--                                         mockContents :: Text
--                                         mockContents = "Mock File Contents"
--                                       in
--                                         \case
--                                           ReadFile path ->
--                                             logItem (IterationLog . Doc . DocAction $ ActionInfo ("readFile: " <> txt path) ) $> mockContents

--                                           WriteFile path str -> logItem . IterationLog . Doc . DocAction $ ActionInfoM 
--                                                                                         ("write file: " <> txt path)
--                                                                                         $ "contents:\n" <> str
