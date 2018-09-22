
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE QuasiQuotes #-}

module DemoRoughTestSimple where

import           Check
import           TestConfig
import DSL.Ensure
import Runner
import           Control.Monad.Freer
import           DSL.Interpreter
import           Foundation.Extended             hiding (fail, putStrLn,
                                                  readFile, writeFile)
import qualified Prelude as P

type Effects effs = EFFEnsureOnly effs

data ApState = ApState {
  itemId :: Int,
  simpleMessage :: String
} deriving Show

type ValState = ApState

data RunConfig = RunConfig {
  environment :: String,
  depth       :: Integer,
  path        :: Path Abs File
}

interactor :: Effects effs => (ItemClass TestItem ValState) => RunConfig -> TestItem -> Eff effs ApState
interactor runConfig TestItem{..} = do
                                  ensure "Only even iids expected" $ P.even iid
                                  pure $ ApState iid "Success"

prepState :: ApState -> ValState
prepState = id

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
                                                                            chk "iid is small" (\ApState{..} -> itemId < 200 ) <>
                                                                            chk "iid is big"   (\vs -> itemId vs > 500),
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
