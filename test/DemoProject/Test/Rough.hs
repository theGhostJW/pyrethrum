
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- should not need this: https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module DemoProject.Test.Rough where

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
import           Foundation.Extended             hiding (readFile, writeFile, Item)
import Runner as R 
import Type.Reflection
import Data.Aeson.TH
import GHC.Generics
import qualified Data.Serialize as S
import qualified System.Environment as E
import OrphanedInstances
import RunElementClasses
import DemoProject.Test.TestFilePaths
import GHC.Stack

type Effects effs = Members '[Logger, Ensure, ArbitraryIO, FileSystem] effs

config :: TestConfig
config = C.testConfig {
  header = "This is a Rough Test",
  countries = allCountries
 }

jw = showItems

showItems :: IO ()
showItems = showAndLogItems items

endpoint :: (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
endpoint = ep runConfig $ IID 140

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  exePath :: String,
  fileText :: StrictReadResult
} deriving Show


-- https://www.parsonsmatt.org/2017/07/29/using_ghc_callstacks.html
-- error has calstack by default maybe catch exception and rethrow as error 
-- to get callstack
putStrLnWithCallStack :: String -> IO ()
putStrLnWithCallStack msg = do
  putStrLn msg
  putStrLn $ toS (prettyCallStack callStack)

interactor :: forall effs. Effects effs => (ItemClass Item DState) => RunConfig -> Item -> Eff effs ApState
interactor RunConfig{..} Item{..} = do
                                      writeFile path $ pre  <> " ~ " <> post <> " !!"
                                      ensure "Blahh" $ P.even iid
                                      log "Hi"
                                      logWarning "a warning"
                                      arbitraryIO "This is an arbitrary Put Line" () (putStrLn "Hello from random action")
                                      txt <- readFile path

                                      when (iid == 140)
                                        $ void $ arbitraryIO "This is an arbitrary THING THAT WILL BLOW UP" (Right "tHIS WILL BLOW UP") (readFileUTF8 invalidFile)

                                      when (iid == 130) $
                                        do 
                                          log' "Hi there" "a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf" 
                                          logWarning' "Hi there warning" "a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf" 
                                          logWarning' "Hi there warning 2" "a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf" 

                                      when (iid == 110) $
                                        do 
                                          log "SHould Crash" 
                                          log $ toS (prettyCallStack callStack)
                                          arbitraryIO "Debug Stack" () (putStrLnWithCallStack "Hello with stack")
                                          -- error "BANG !!!"

                                      pure $ ApState  {
                                        itemId  = iid,
                                        filePath = path,
                                        exePath = "NOT IMPLEMENTED",
                                        fileText = txt
                                      }


newtype DState = V {
                    iidx10 :: Int
                  } deriving Show


prepState :: ApState -> Ensurable DState
prepState ApState{..} = do
                          ensure  "I do not like 110 in prepstate" (itemId /= 110)
                          pure $ V $ 10 * itemId

--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test Items %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Item = Item {
                    iid    :: Int,
                    pre    :: String,
                    post   :: String,
                    path   :: Path Abs File,
                    checks :: CheckList DState
                  } deriving (Show, Generic)

i = Item

-- should be :: RunConfig -> [Item]
-- later optional hedgehog
items :: [Item]
items = [
          i 100 "Pre"  "Post" validFile $
                              gateFirst $ chk "iid x 10 is small" (\V{..} -> iidx10 < 200 ) <>
                              expectFailure "this bug was introduced in an earlier version and will be fixed eventually" (chk "iid x 10 is big" (\V{..} -> iidx10 > 500)),
          i 110 "Pre"  "Post" validFile mempty,
          i 120 "Pre"  "Post" invalidFile2 mempty,
          i 130 "Pre"  "Post" validFile mempty,
          i 140 "Pre"  "Post" validFile mempty,
          i 150 "Pre"  "Post" validFileWithSpace mempty
  ]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


nameOfModule :: TestModule
nameOfModule = mkTestModule ''ApState


ep :: RunConfig -> ItemFilter Item -> (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
ep rc iFltr = testEndpoint nameOfModule rc (filterredItemIds iFltr items)

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
