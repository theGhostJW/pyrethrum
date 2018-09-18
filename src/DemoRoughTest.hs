
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module DemoRoughTest where

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
                    checks :: CheckList ValState
                  } deriving Show

--type ValState = ApState

newtype ValState = ValState {
                    iidPlus10 :: Int
                  } deriving Show

data RunConfig = RunConfig {
  environment :: String,
  depth       :: Integer,
  path        :: Path Abs File
}

interactor :: InteractorFileSystem RunConfig Item ApState
interactor runConfig item = do
                              let fullFilePath = path (item :: Item)
                              writeFile fullFilePath $ pre item  <> " ~ " <> post item <> " !!"
                              ensure True "Blahh"
                              txt <- readFile fullFilePath
                              pure $ ApState (iid item) fullFilePath txt

prepState :: ApState -> ValState
prepState a = ValState $ 10 * itemId a

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

runElements :: forall effs. (Members '[Ensure, FileSystem] effs) => TestRunElements RunConfig Item (Eff effs ApState) ApState ValState
runElements = TestRunElements {
  testInteractor = interactor,
  testPrepState = prepState,
  testItems = items
}


--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test Items %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i = Item

items = [
          i 100 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] $
                                                                            chk "iid is small" (\vs -> iidPlus10 vs < 200 ) <>
                                                                            chk "iid is big"  (\vs -> iidPlus10 vs > 500) ,
          i 110 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 120 "Pre"  "Post"   [absfile|R:\Vids\SystemDesign\Wrong.txt|]   mempty,
          i 130 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 140 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 150 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty
  ]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instance TestItem Item ValState where
  identifier = iid
  whenClause = pre
  thenClause = post
  checkList = checks
