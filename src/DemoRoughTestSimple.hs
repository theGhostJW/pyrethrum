
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE QuasiQuotes #-}

module DemoRoughTestSimple where

import           Check
import DSL.FileSystem
import DSL.Ensure
import TestItem
import Runner
import           Control.Monad.Freer
import           DSL.Interpreter
import           Foundation.Extended             hiding (Item, fail, putStrLn,
                                                  readFile, writeFile)

type Effects effs = EFFFileSystem effs

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  fileText :: StrictReadResult
} deriving Show

newtype ValState = ValState {
                    iidPlus10 :: Int
                  } deriving Show

data RunConfig = RunConfig {
  environment :: String,
  depth       :: Integer,
  path        :: Path Abs File
}

interactor :: Effects effs => (TestItem Item ValState) => RunConfig -> Item -> Eff effs ApState
interactor runConfig item = do
                              let fullFilePath = path (item :: Item)
                              writeFile fullFilePath $ pre item  <> " ~ " <> post item <> " !!"
                              ensure True "Blahh"
                              txt <- readFile fullFilePath
                              pure $ ApState (iid item) fullFilePath txt

prepState :: ApState -> ValState
prepState a = ValState $ 10 * itemId a

--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test Items %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Item = Item {
                    iid    :: Int,
                    pre    :: String,
                    post   :: String,
                    path   :: Path Abs File,
                    checks :: CheckList ValState
                  } deriving Show

i = Item

items = [
          i 100 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] $
                                                                            chk "iid is small" (\ValState{..} -> iidPlus10 < 200 ) <>
                                                                            chk "iid is big"   (\vs -> iidPlus10 vs > 500),
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

runElements :: Effects effs => TestRunElements RunConfig Item (Eff effs ApState) ApState ValState
runElements = TestRunElements {
  testInteractor = interactor,
  testPrepState = prepState,
  testItems = items
}
