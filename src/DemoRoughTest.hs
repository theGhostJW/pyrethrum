
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE QuasiQuotes #-}

module DemoRoughTest where

import           Check
import           TestConfig
import           Control.Monad.Freer
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import qualified Prelude as P
import           Foundation.Extended             hiding (Item, fail, putStrLn,
                                                  readFile, writeFile)
import           Runner


type Effects effs = EFFFileSystem effs

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  fileText :: StrictReadResult
} deriving Show

newtype ValState = V {
                    iidPlus10 :: Int
                  } deriving Show

data RunConfig = RunConfig {
  environment :: String,
  depth       :: Integer,
  path        :: Path Abs File
}

interactor :: Effects effs => (ItemClass TestItem ValState) => RunConfig -> TestItem -> Eff effs ApState
interactor runConfig item = do
                              let fullFilePath = path (item :: TestItem)
                              writeFile fullFilePath $ pre item  <> " ~ " <> post item <> " !!"
                              ensure "Blahh" $ P.even $ iid item
                              txt <- readFile fullFilePath
                              pure $ ApState (iid item) fullFilePath txt

prepState :: ApState -> ValState
prepState ApState{..} = V $ 10 * itemId

--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test Items %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data TestItem = TestItem {
                    iid    :: Int,
                    pre    :: String,
                    post   :: String,
                    path   :: Path Abs File,
                    checks :: CheckList ValState
                  } deriving Show

i = TestItem

items = [
          i 100 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] $
                                                                            chk "iid is small" (\V{..} -> iidPlus10 < 200 ) <>
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

instance ItemClass TestItem ValState where
  identifier = iid
  whenClause = pre
  thenClause = post
  checkList = checks

runElements :: Effects effs => TestRunElements RunConfig TestItem (Eff effs ApState) ApState ValState
runElements = TestRunElements {
  testInteractor = interactor,
  testPrepState = prepState,
  testItems = items
}
