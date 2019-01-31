
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}

module DemoProject.Test.Simple where

import           Check
import  DemoProject.Config as C
import DSL.Ensure
import Runner as R
import           Control.Monad.Freer
import           DSL.Interpreter
import           Foundation.Extended hiding (Item)
import qualified Prelude as P
import Data.Aeson.TH
import OrphanedInstances
import TestAndRunConfig

type Effects effs = EFFEnsureLog effs

config :: TestConfig
config = C.testConfig {
  header = "This Simple Test Only Uses Ensure Effects"
}

showItems :: IO ()
showItems = showAndLogItems items

endpoint :: (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
endpoint = ep runConfig (IID 123)

data ApState = ApState {
  itemId :: Int,
  simpleMessage :: String
} deriving Show

type ValState = ApState

interactor :: forall effs. Effects effs => (ItemClass Item ValState) => RunConfig -> Item -> Eff effs ApState
interactor _rc TestItem{..} = do
                              ensure "Only even iids expected" $ P.even iid
                              pure $ ApState iid "Success"

prepState :: ApState -> Ensurable ValState
prepState = pure . id

--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test Items %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Item = TestItem {
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
          i 123 "Pre"  "Post"   [absfile|R:\Vids\SystemDesign\Wrong.txt|]   mempty,
          i 130 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 140 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 150 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty
  ]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nameOfModule :: TestModule
nameOfModule = mkTestModule ''ApState

ep :: RunConfig -> ItemFilter Item -> (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
ep rc iFltr = testEndpoint nameOfModule rc (filterredItemIds iFltr items)


test :: forall effs. Effects effs => Test Item effs ApState ValState
test = GenericTest {
              configuration = config {address = nameOfModule},
              components = TestComponents {
                                testItems = items,
                                testInteractor = interactor,
                                testPrepState = prepState
                            }
            }

$(deriveToJSON defaultOptions ''Item)

instance ItemClass Item ValState where
  identifier = iid
  whenClause = pre
  thenClause = post
  checkList = checks
