
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module ErrorsExceptions where


-- full run through writing file
-- change to inmemory
-- change to document
-- through exception and check for
-- make document fail eg /0 based on get
-- validation validation module ?? ~ terminal validations
-- generalis runner

import           Control.Exception
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Either.Exit (orDie)
import           Data.Function                   ((&))
import           Data.Functor
import           Data.List
import           Foundation.Extended             hiding (fail, putStrLn,
                                                  readFile, writeFile)
import qualified Foundation.Extended             as F
import           Foundation.String
import           Paths_pyrethrum
import qualified Prelude
import           System.Exit                     as SysExit hiding (ExitCode (ExitSuccess))
import           System.IO                       (FilePath, IO,
                                                  IOMode (ReadMode, WriteMode),
                                                  withFile)
import           System.IO.Error                 (isAlreadyInUseError,
                                                  isDoesNotExistError,
                                                  isPermissionError)


default (String)

testExceptionTakesPriority :: Int -> Int -> Either Int Int
testExceptionTakesPriority x y = run $ runError (go x y)
  where
    go a b = (+) <$> pure a <*> throwError b

demoTestExceptionTakesPriority = testExceptionTakesPriority 55 100

-- Exceptions and state.
incr :: Member (State Int) r => Eff r ()
incr = do
        i :: Int <- get
        put $ i + 1

tes1 :: (Members '[State Int, Error String] r) => Eff r b
tes1 = incr >> throwError ("exc" :: String)

-- Order does not matter

demoTes1 :: Either String (Int, Int)
demoTes1 = run $ runError $ runState (2 :: Int) tes1
-- >> Left "exc"

demoTer1 :: (Either String Int, Int)
demoTer1 = run $ runState (2 :: Int) $ runError tes1
-- >> (Left "exc", 3)

teCatch :: Member (Error String) r => Eff r a -> Eff r String
teCatch m = (m >> pure "done") `catchError` \e -> pure (e :: String)

demoteCatch :: (Either String String, Int)
demoteCatch = run $ runState (2 :: Int)
                  $ runError
                  $ teCatch tes1

demoteCatch2 :: Either String (String, Int)
demoteCatch2 = run $ runError
                   $ runState (2 :: Int)
                   $ teCatch tes1
-- >> Right ("exc", 3)

newtype TooBig = TooBig Int
  deriving (Eq, Show)

valCheckerApp :: Member (Error TooBig) r => Eff r Int -> Eff r Int
valCheckerApp m = do
                    val <- m
                    when (val > 5) $
                      throwError (TooBig val)
                    pure val

-- | Specialization to tell the type of the exception.
runErrBig :: Eff (Error TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runError

ex2rr :: Either TooBig Int
ex2rr = run $ runReader (5 :: Int)
            $ runErrBig
            $ valCheckerApp ask
-- >> Right 5

ex2rr1 :: Either TooBig Int
ex2rr1 = run $ runReader (7 :: Int)
             $ runErrBig
             $ valCheckerApp ask
-- >> Left (TooBig 7)

-- | Different order of handlers (layers).
ex2rr2 :: Either TooBig Int
ex2rr2 = run $ runErrBig
             $ runReader (7 :: Int)
             $ valCheckerApp ask
-- >> Left (TooBig 7)

-- catchError :: forall e effs a. Member (Error e) effs => Eff effs a -> (e -> Eff effs a) -> Eff effs a
catcher2 :: Member (Error TooBig) r => Eff r Int -> Eff r Int
catcher2 effs = catchError effs (\(TooBig n) -> pure n)

-- does not throw
catcher2Demo = run $ runErrBig
                   $ runReader (7 :: Int)
                   $ catcher2
                   $ valCheckerApp ask

-- >> Right 7

handeler :: Eff (Error TooBig ': effs) Int -> Eff effs Int
handeler effs = handleError effs (\(TooBig n) -> pure n)

-- does not throw
handlerDemo = run $ runErrBig
                   $ runReader (7 :: Int)
                   $ handeler
                   $ valCheckerApp ask

-- >> Right 7

-- catchError :: forall e effs a. Member (Error e) effs => Eff effs a -> (e -> Eff effs a) -> Eff effs a
exagerator :: Member (Error TooBig) r => Eff r Int -> Eff r Int
exagerator effs = catchError effs (\(TooBig n) -> throwError $ TooBig $ n * 10)

-- does not throw
exageratorDemo = run $ runErrBig
                   $ runReader (7 :: Int)
                   $ exagerator
                   $ valCheckerApp ask

-- >> Left (TooBig 70)

--- With IO --
{- File System Lang -}

data FileSystem r where
  ReadFile :: Path a File -> FileSystem StrictReadResult

data AppError = AppError String |
                   IOAppError IOException
                   deriving Show

readFile :: Members '[FileSystem, Error AppError] effs => Path a File -> Eff effs StrictReadResult
readFile path = let
                  pthStr = toStr $ toFilePath path
                in
                  F.elem 'x' pthStr
                        ? throwError (AppError "No Xs allowed in file name")
                        $ send $ ReadFile path

{- File System IO Interpreter -}

runAppError :: Eff (Error AppError ': r) a -> Eff r (Either AppError a)
runAppError = runError

{-
  interpretM takes an interpreter in IO (its first argument has type eff ~> m with m ~ IO here), so that doesn't allow you to throw AppErrors via
  the Members '[Error AppError] effs constraint.

  Instead you can use interpret, with full access to effs. That would roughly look like:
-}
fileSystemIOInterpreter :: forall effs a. (Members '[Error AppError] effs, LastMember IO effs) => Eff (FileSystem ': effs) a -> Eff effs a
fileSystemIOInterpreter = interpret $ \case
                                          ReadFile path -> do
                                                             r <- sendM (try (F.readFileUTF8 path))
                                                             case r of
                                                               Left (e :: IOException) -> throwError (IOAppError e)
                                                               Right f -> pure f

--fileSystemIOInterpreter effs = throwError $ AppError "BLahh"

application :: Members '[FileSystem, Error AppError] effs => Path a File -> Eff effs StrictReadResult
application = readFile

ioApp :: Path a File -> IO (Either AppError StrictReadResult)
ioApp path = runM
              $ runAppError
              $ fileSystemIOInterpreter
              $ application path

demoPassApp = ioApp [absfile|C:\Vids\SystemDesign\VidList.md|]
demoFailApp = ioApp [absfile|C:\Vids\SystemDesign\VidList.txt|]
demoFailIOApp = ioApp [absfile|C:\Vids\SystemDesign\MissingFile.md|]
