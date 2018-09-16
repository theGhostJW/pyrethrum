
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module RoughTest where

import           Check
import           Control.Exception
import qualified Control.Monad                   as Monad
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
import           Data.Map
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           Foundation.Extended             hiding (Item, fail, putStrLn,
                                                  readFile, writeFile)
import qualified Foundation.Extended             as F
import           Foundation.List.DList
import           Foundation.String
import           Paths_pyrethrum
import qualified Prelude
import           Runner
import           System.Exit                     as SysExit hiding (ExitCode (ExitSuccess))
import           System.IO                       (FilePath, IO,
                                                  IOMode (ReadMode, WriteMode),
                                                  withFile)
import           System.IO.Error                 (isAlreadyInUseError,
                                                  isDoesNotExistError,
                                                  isPermissionError)
import           TestItem

default (String)

{- Application (Interactor) -}

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  fileText :: StrictReadResult
}
  deriving Show

data Item = Item {
                    iid    :: Int,
                    pre    :: String,
                    post   :: String,
                    path   :: Path Abs File,
                    checks :: DList (Check ValState)
                  } deriving Show

--type ValState = ApState

newtype ValState = ValState {
                    iidPlus10 :: Int
                  } deriving Show

data RunConfig = RunConfig {
  environment :: String,
  depth       :: Integer,
  path        :: Path Abs File
} deriving Show

interactor :: InteractorFileSystem RunConfig Item ApState
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
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|],
  checks = mempty
}

sampleRunConfig = RunConfig {
  environment = "Test",
  depth = 44,
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|]
}

-- Demos
replShow d = Prelude.sequenceA $ Prelude.sequenceA <$> d

demoExecuteFileSystemInIO :: IO (Either AppError ApState)
demoExecuteFileSystemInIO = executeFileSystemInIO (interactor sampleRunConfig sampleItem)

returnValState apState valState = valState

This Does NOT Work Because We don't Know the Shape tof the Interactor Result ROLL BACK !!!
--emoIOAll :: Either FilterError [IO (Either AppError (Item, ApState, ValState, ResultList))]
demoIOAll = runTest prepState sampleRunConfig interactor executeFileSystemInIO sampleTestItems All
{-
-- runTest prepState runConfig interactor interpreter items filtr

demoIOAllRepl :: IO (Either FilterError [Either AppError ValState])
demoIOAllRepl = replShow demoIOAll

--demoIOFull :: Either FilterError [IO (Either AppError (TestInfo ApState ValState))]
demoIOFull = runTest sampleRunConfig prepState interactor sampleTestItems executeFileSystemInIO All

--demoIOFullRepl :: IO (Either FilterError [Either AppError (TestInfo ApState ValState)])
demoIOFullRepl = replShow demoIOFull

dummyPrepState r a = a

demoExecuteFileSystemInIONoVal :: IO (Either AppError ApState)
demoExecuteFileSystemInIONoVal = executeFileSystemInIO (dummyPrepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

returnApState apState valState = apState

demoIOAllNoVal:: Either FilterError [IO (Either AppError ApState)]
demoIOAllNoVal = runTest returnApState sampleRunConfig prepState interactor sampleTestItems executeFileSystemInIO All

demoIOAllNoValRepl ::  IO (Either FilterError [Either AppError ApState])
demoIOAllNoValRepl = replShow demoIOAllNoVal

-- demoIOAllValidate = runTest prepState sampleRunConfig interactor sampleTestItems executeFileSystemInIO All

-- Demos
demoDocument :: (Either AppError ValState, [String])
demoDocument = executeFileSystemDocument (prepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

demoDocumentedAll :: Either FilterError [(Either AppError ValState, [String])]
demoDocumentedAll = runTest returnValState sampleRunConfig prepState interactor sampleTestItems executeFileSystemDocument  All

demoDocumentNoVal :: (Either AppError ApState, [String])
demoDocumentNoVal = executeFileSystemDocument (dummyPrepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

demoDocumentedAllNoVal :: Either FilterError [(Either AppError ApState, [String])]
demoDocumentedAllNoVal = runTest returnApState sampleRunConfig prepState interactor sampleTestItems executeFileSystemDocument All

-}

instance TestItem Item ValState where
  identifier = iid
  whenClause = pre
  thenClause = post
  validation = checks


--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i = Item

sampleTestItems :: [Item]
sampleTestItems = [
                    i 100 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
                    i 110 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
                    i 120 "Pre"  "Post" [absfile|R:\Vids\SystemDesign\Wrong.txt|]   mempty,
                    i 130 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
                    i 140 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
                    i 150 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty
                  ];
