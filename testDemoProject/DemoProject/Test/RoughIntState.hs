
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- should not need this: https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module DemoProject.Test.RoughIntState where

import           DSL.Logger
import           Check
import           DemoProject.Config as C
import           Polysemy
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.ArbitraryIO
import           Pyrelude
import Runner as R 
import Data.Aeson.TH
import OrphanedInstances()
import DemoProject.Test.TestFilePaths

type Effects effs = Members '[SuiteLogger, Ensure, ArbitraryIO, FileSystem] effs

config :: TestConfig
config = C.testConfig {
  header = "This is a Rough Test",
  countries = allCountries
 }

showItems :: IO ()
showItems = showAndLogItems $ items runConfig

endpoint :: (forall m1 m a. TestPlan m1 m a FullIOMembers) -> Sem FullIOMembers ()
endpoint = ep runConfig $ IID 120

type ApState = Int
type DState = Int

interactor :: forall effs. Effects effs => (ItemClass Item DState) => RunConfig -> Item -> Sem effs ApState
interactor RunConfig{..} Item{..} = pure 5

prepState :: EnsureEffs effs => Item -> ApState -> Sem effs DState
prepState _  _ = pure 6

--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test Items %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Item = Item {
                    iid    :: Int,
                    pre    :: Text,
                    post   :: Text,
                    path   :: Path Abs File,
                    checks :: CheckDList DState
                  } deriving (Show, Generic)


passAlwaysChk = chk "pass every time" $ const True

-- should be :: RunConfig -> [Item]
-- later optional hedgehog
items :: RunConfig -> [Item]
items rc = [ Item 110 "Whene Statement"  "Then Statement" validFile passAlwaysChk]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


nameOfModule :: TestModule
nameOfModule = mkTestModule ''ApState


ep :: RunConfig -> ItemFilter Item -> (forall m1 m a. TestPlan m1 m a FullIOMembers) -> Sem FullIOMembers ()
ep rc iFltr = testEndpoint nameOfModule rc (filterredItemIds iFltr $ items rc)

test :: forall effs. Effects effs => Test Item ApState DState effs
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