
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- should not need this: https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module DemoProject.Test.Rough where

import           DSL.Logger
import           Check
import           DemoProject.Config as C
import Text.Show.Pretty as PP
import           Polysemy
import           Control.Monad
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.ArbitraryIO
import qualified Prelude as P
import qualified Pyrelude.IO as PIO
import           Pyrelude
import RunnerP as R 
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
  header = "This is a Rough Test",
  countries = allCountries
 }

jw = endpoint

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


-- error has calstack by default maybe catch exception and rethrow as error 
-- to get callstack
putStrLnWithCallStack :: Text -> IO ()
putStrLnWithCallStack msg = do
  PIO.putStrLn msg
  PIO.putStrLn $ toS (prettyCallStack callStack)

interactor :: forall effs. Effects effs => (ItemClass Item DState) => RunConfig -> Item -> Sem effs ApState
interactor RunConfig{..} Item{..} = 
  do
    writeFile path $ pre  <> " ~ " <> post <> " !!"
    ensure "Blahh" $ P.even iid
    log "Hi"

    arbitraryIO "This is an arbitrary Put Line" () (PIO.putStrLn "Hello from random action")
    tx <- readFile path

    when (iid == 140)
      $ void $ arbitraryIO "This is an arbitrary THING THAT WILL BLOW UP" "tHIS WILL BLOW UP" (PIO.readFile $ toFilePath invalidFile)

    when (iid == 130) $
      do 
        logWarning "a warning"
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
      fileText = tx
    }

newtype DState = V {
                    iidx10 :: Int
                  } deriving Show

prepState :: Item -> ApState -> Ensurable DState
prepState _i ApState{..} = do
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

i = Item

passAlwaysChk = chk' "pass every time" (const "this is additoinal info \nblahh\nblahh\nblahh") $ const True

-- should be :: RunConfig -> [Item]
-- later optional hedgehog
items :: RunConfig -> [Item]
items rc = [
          i 100 "Pre"  "Post" validFile $
                                gate 
                                . expectFailure "this bug was introduced in an earlier version and will be fixed eventually" 
                                $ chk' "iid x 10 is small" (\V{..} -> "the iid x 10 (" <> txt iidx10 <> ") is expected to be less than 200") ((200 >) . iidx10) 
                                <> chk' "iid x 10 is big"  (\V{..} -> "the iid x 10 (" <> txt iidx10 <> ") is expected to be greater than 500") ((500 <) . iidx10),
          i 110 "Pre"  "Post" validFile passAlwaysChk ,
          i 120 "Pre"  "Post" invalidFile2 passAlwaysChk,
          i 130 "Pre"  "Post" validFile passAlwaysChk,
          i 140 "Pre"  "Post" validFile passAlwaysChk,
          i 150 "Pre"  "Post" validFileWithSpace mempty,
          i 160 "Pre"  "Post" validFile passAlwaysChk ,
          i 170 "Pre"  "Post" validFile passAlwaysChk,
          i 180 "Pre"  "Post" validFile passAlwaysChk,
          i 190 "Pre"  "Post" validFile passAlwaysChk
  ]

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
