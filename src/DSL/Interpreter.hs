
module DSL.Interpreter where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DSL.FileSystem
import           DSL.Ensure
import           Foundation.Extended
import Data.Either.Combinators
import TestItem
import Check

type InteractorFileSystem runConfig item r = forall v effs. (Members '[Ensure, FileSystem] effs, TestItem item v) => runConfig -> item -> Eff effs r

data AppError =
              AppFileSystemError FileSystemError |
              AppEnsureError EnsureError |
              NotImplemented String |
              GenericError String |
              IOError IOException
              deriving (Show, Eq)

unifyFSEnsureError :: Either EnsureError (Either FileSystemError v) -> Either AppError v
unifyFSEnsureError = \case
                       Right ee -> case ee of
                                       Right v -> Right v
                                       Left l -> Left $ AppFileSystemError l
                       Left enFail -> Left $ AppEnsureError enFail

executeFileSystemInIO :: Eff '[FileSystem, Ensure, Error FileSystemError, Error EnsureError, IO] a -> IO (Either AppError a)
executeFileSystemInIO app = unifyFSEnsureError <$> runM
                                  (
                                    runError
                                    $ runError
                                    $ ensureInterpreter
                                    $ fileSystemIOInterpreter
                                    app
                                  )

executeFileSystemDocument :: Eff '[FileSystem, Ensure, Error EnsureError, Writer [String]] a -> (Either AppError a, [String])
executeFileSystemDocument app =  let (val, log) = run
                                                      $ runWriter
                                                      $ runError
                                                      $ ensureInterpreter
                                                      $ interpret fileSystemDocInterpreter
                                                      app
                                  in
                                   (mapLeft AppEnsureError val, log)
