
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
import           Pyrelude
import qualified Prelude as P
import Data.Aeson.TH
import OrphanedInstances
import RunElementClasses
import DemoProject.Test.TestFilePaths
import GHC.Generics

type Effects effs = EFFEnsureLog effs

config :: TestConfig
config = C.testConfig {
  header = "This Simple Test Only Uses Ensure Effects"
}

showItems :: IO ()
showItems = showAndLogItems $ items runConfig

endpoint :: (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
endpoint = ep runConfig (IID 123)

data ApState = ApState {
  itemId :: Int,
  simpleMessage :: Text
} deriving Show

type DState = ApState

interactor :: forall effs. Effects effs => (ItemClass Item DState) => RunConfig -> Item -> Eff effs ApState
interactor _rc TestItem{..} = do
                              ensure "Only even iids expected" $ P.even iid
                              pure $ ApState iid "Success"

prepState :: ApState -> Ensurable DState
prepState = pure . id

--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test Items %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Item = TestItem {
                      iid    :: Int,
                      pre    :: Text,
                      post   :: Text,
                      path   :: Path Abs File,
                      checks :: CheckDList DState
                    } deriving (Show, Generic)

i = TestItem

items :: RunConfig -> [Item]
items rc = [
              i 100 "Pre"  "Post" validFile $
                                            chk "iid is small" (\ApState{..} -> itemId < 200 ) <>
                                            chk "iid is big"   (\ds -> itemId ds > 500),
              i 110 "Pre"  "Post" validFile mempty,
              i 123 "Pre"  "Post" invalidFile2 mempty,
              i 130 "Pre"  "Post" validFile mempty,
              i 140 "Pre"  "Post" validFile mempty,
              i 150 "Pre"  "Post" validFile mempty
      ]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nameOfModule :: TestModule
nameOfModule = mkTestModule ''ApState

ep :: RunConfig -> ItemFilter Item -> (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
ep rc iFltr = testEndpoint nameOfModule rc (filterredItemIds iFltr $ items runConfig)


test :: forall effs. Effects effs => Test Item effs ApState DState
test = GenericTest {
              configuration = config {address = nameOfModule},
              components = TestComponents {
                                testItems = items,
                                testInteractor = interactor,
                                testPrepState = prepState
                            }
            }

instance ItemClass Item DState where
  identifier = iid
  whenClause = pre
  thenClause = post
  checkList = checks
$(deriveToJSON defaultOptions ''Item)
$(deriveToJSON defaultOptions ''ApState)
