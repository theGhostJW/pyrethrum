
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- should not need this: https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module DemoProject.Test.RoughDisabled where

import           DSL.Logger
import           Check
import           DemoProject.Config as C
import Text.Show.Pretty as PP
import           Control.Monad.Freer
import           Control.Monad
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.ArbitraryIO
import qualified Prelude as P
import qualified Pyrelude.IO as PIO
import           Pyrelude
import Runner as R 
import Type.Reflection
import Data.Aeson.TH
import GHC.Generics
import qualified System.Environment as E
import OrphanedInstances
import RunElementClasses
import DemoProject.Test.TestFilePaths
import GHC.Stack

type Effects effs = Members '[Logger, Ensure, ArbitraryIO, FileSystem] effs

config :: TestConfig
config = C.testConfig {
  header = "This is a Rough Disabled Test",
  countries = allCountries,
  active = False
 }

showItems :: IO ()
showItems = showAndLogItems $ items runConfig

endpoint :: (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
endpoint = ep runConfig $ IID 120

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  exePath :: Text,
  fileText :: Text
} deriving Show

interactor :: forall effs. Effects effs => (ItemClass Item DState) => RunConfig -> Item -> Eff effs ApState
interactor RunConfig{..} Item{..} = 
    pure $ ApState {
      itemId  = iid,
      filePath = path,
      exePath = "NOT IMPLEMENTED",
      fileText = "Not Used"
    }

newtype DState = V {
                    iidx10 :: Int
                  } deriving Show


prepState :: ApState -> Item -> Ensurable DState
prepState ApState{..} itm = do
                              ensure  "I do not like 110 in prepstate" (itemId /= 110)
                              pure $ V $ 10 * itemId

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

-- should be :: RunConfig -> [Item]
-- later optional hedgehog
items :: RunConfig ->  [Item]
items rc = [Item 120 "Pre" "Post" invalidFile2 mempty]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
$(deriveToJSON defaultOptions ''DState)
$(deriveToJSON defaultOptions ''ApState)
