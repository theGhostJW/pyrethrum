
module FileSystem.Internal where

import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Exception
import AppErrors

{- File System Lang -}

data FileSystem r where
  ReadFile :: Path a File -> FileSystem StrictReadResult
  WriteFile :: Path a File -> String -> FileSystem ()

readFile :: Member FileSystem effs => Path a File -> Eff effs StrictReadResult
readFile = send . ReadFile

writeFile :: Member FileSystem effs => Path a File -> String -> Eff effs ()
writeFile pth = send . WriteFile pth

{- File System IO Interpreter -}

-- fileSystemIOInterpreter :: forall effs a. LastMember IO effs => Eff (FileSystem ': effs) a -> Eff effs a
-- fileSystemIOInterpreter = interpretM $ \case
--                                ReadFile path -> readFileUTF8 path
--                                WriteFile path str -> writeFileUTF8 path str

fileSystemIOInterpreter :: forall effs a. (Members '[Error AppError, IO] effs) => Eff (FileSystem ': effs) a -> Eff effs a
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


-- fileSystemIOInterpreter1 :: forall effs a. forall m e. (Members '[Error (m e), IO] effs) => (IOException -> m e) -> Eff (FileSystem ': effs) a -> Eff effs a
-- fileSystemIOInterpreter1 handler eff = interpret $ case eff of
--                                                    ReadFile path -> do
--                                                                     r <- send (try (readFileUTF8 path))
--                                                                     case r of
--                                                                       Left (e :: IOException) -> throwError (handler e)
--                                                                       Right f -> pure f

-- hoistEitherT :: (forall b. m b -> n b) -> EitherT x m a -> EitherT x n a
