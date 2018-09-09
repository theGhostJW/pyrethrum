
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

import Runner
import qualified Control.Monad as Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Control.Monad.Freer.Reader
import           Data.Function                 ((&))
import           Data.Functor
import           Data.List
import           Foundation.Extended           hiding (putStrLn, readFile, writeFile, fail, Item)
import    qualified       Foundation.Extended  as F
import           Foundation.String
import           Paths_pyrethrum
import           Control.Monad.Trans.Either
import Ensure
import FileSystem
import AppError
import Data.Map
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

{- Application (Interactor) -}

data ApState = ApState {
  itemId :: Int,
  filePath :: Path Abs File,
  fileText :: StrictReadResult
}
  deriving Show

data Item = Item {
                    iid :: Int,
                    pre :: String,
                    post :: String,
                    path :: Path Abs File
                  } deriving Show

--type ValState = ApState

newtype ValState = ValState {
                    iidPlus10 :: Int
                  } deriving Show

data RunConfig = RunConfig {
  environment :: String,
  depth :: Integer,
  path :: Path Abs File
}

interactor :: Members '[Ensure, FileSystem] effs => RunConfig -> Item -> Eff effs ApState
interactor runConfig item = do
                              let fullFilePath = path (item :: Item)
                              writeFile fullFilePath $ pre item  <> " ~ " <> post item <> " !!"
                              ensure True "Blahh"
                              txt <- readFile fullFilePath
                              pure $ ApState (iid item) fullFilePath txt


prepState :: RunConfig -> ApState -> ValState
prepState r a = ValState $ 10 * itemId a

{- Application IO Interpreter -}

sampleItem =  Item {
  iid = 500,
  pre = "I do a test",
  post = "the test runs",
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|]
}

sampleRunConfig = RunConfig {
  environment = "Test",
  depth = 44,
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|]
}


executeInIO :: (a -> v) -> Eff '[FileSystem, Ensure, Error AppError, IO] a -> IO (Either AppError v)
executeInIO func app = runM $ runError
                            $ ensureInterpreter
                            $ fileSystemIOInterpreter
                            $ func <$> app

-- Demos
replShow d = Prelude.sequenceA $ Prelude.sequenceA <$> d

demoExecuteInIO :: IO (Either AppError ValState)
demoExecuteInIO = executeInIO (prepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

returnValState apState valState = valState

demoIOAll :: Either FilterError [IO (Either AppError ValState)]
demoIOAll = runTest returnValState sampleRunConfig prepState interactor sampleTestItems executeInIO All

demoIOAllRepl :: IO (Either FilterError [Either AppError ValState])
demoIOAllRepl = replShow demoIOAll


demoIOFull :: Either FilterError [IO (Either AppError (TestInfo ApState ValState))]
demoIOFull = runTest TestInfo sampleRunConfig prepState interactor sampleTestItems executeInIO All

demoIOFullRepl :: IO (Either FilterError [Either AppError (TestInfo ApState ValState)])
demoIOFullRepl = replShow demoIOFull

dummyPrepState r a = a

demoExecuteInIONoVal :: IO (Either AppError ApState)
demoExecuteInIONoVal = executeInIO (dummyPrepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

returnApState apState valState = apState

demoIOAllNoVal:: Either FilterError [IO (Either AppError ApState)]
demoIOAllNoVal = runTest returnApState sampleRunConfig prepState interactor sampleTestItems executeInIO All

demoIOAllNoValRepl ::  IO (Either FilterError [Either AppError ApState])
demoIOAllNoValRepl = replShow demoIOAllNoVal

-- demoIOAllValidate = runTest prepState sampleRunConfig interactor sampleTestItems executeInIO All

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

executeDocumented :: (a -> b) -> Eff '[FileSystem, Ensure, Error AppError, Writer [String]] a -> (Either AppError b, [String])
executeDocumented func app = run
                              $ runWriter
                              $ runError
                              $ ensureInterpreter
                              $ interpret fileSystemDocInterpreter
                              $ func <$> app

-- Demos
demoDocument :: (Either AppError ValState, [String])
demoDocument = executeDocumented (prepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

demoDocumentedAll :: Either FilterError [(Either AppError ValState, [String])]
demoDocumentedAll = runTest returnValState sampleRunConfig prepState interactor sampleTestItems executeDocumented  All

demoDocumentNoVal :: (Either AppError ApState, [String])
demoDocumentNoVal = executeDocumented (dummyPrepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

demoDocumentedAllNoVal :: Either FilterError [(Either AppError ApState, [String])]
demoDocumentedAllNoVal = runTest returnApState sampleRunConfig prepState interactor sampleTestItems executeDocumented All

instance TestItem Item where
  identifier = iid
  whenClause = pre
  thenClause = post


--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i = Item
sampleTestItems = [
                    i 100 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                    i 110 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                    i 120 "Pre"  "Post"   [absfile|R:\Vids\SystemDesign\Wrong.txt|],
                    i 130 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                    i 140 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|],
                    i 150 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|]
                  ];
