{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module MemberReflection where

import Pyrelude
import qualified Pyrelude as P
import qualified Pyrelude.IO as PIO
import Control.Monad.Freer
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.ArbitraryIO
import           DSL.Logger
import           Runner
import           Data.Aeson.TH
import           RunElementClasses
import           Data.Set as S
import Type.Reflection as R
import Check
import GHC.Generics
import Control.Monad

{-# ANN module "HLint: ignore" #-}


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Example Config %%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum)
data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)

data RunConfig = RunConfig {
  runTitle    :: Text,
  environment :: Environment,
  country     :: Country,
  depth       :: Depth
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Environment)
$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''Depth)
$(deriveJSON defaultOptions ''RunConfig)

instance Titled RunConfig where
  title = runTitle

instance RunConfigClass RunConfig

allEnvironments :: Set Environment
allEnvironments = S.fromList enumList

allNonProdEnvironments :: Set Environment
allNonProdEnvironments = S.fromList [TST, UAT, PreProd]

allCountries = S.fromList [AU, NZ]
auOnly = S.singleton AU
nzOnly = S.singleton NZ

data TestConfig = TestConfig {
  header       :: Text,
  address      :: TestModule,
  environments :: Set Environment,
  countries    :: Set Country,
  minDepth     :: Depth,
  active       :: Bool
}  deriving (Eq, Show)

$(deriveJSON defaultOptions ''TestConfig)

type Test = GenericTest TestConfig RunConfig
type TestResult = GenericResult TestConfig

instance Titled TestConfig where
  title = header

instance TestConfigClass TestConfig where
  moduleAddress = address

testConfig :: TestConfig
testConfig = TestConfig {
  header    = "Configuration Error ~ No Title Assigned",
  address = TestModule "Configuration Error ~ No Address Assigned",
  environments = allNonProdEnvironments,
  countries    = auOnly,
  minDepth     = DeepRegression,
  active       = True
}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Example Test Case %%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type Effects effs = Members '[Logger, Ensure, ArbitraryIO, FileSystem] effs

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  exePath :: Text,
  fileText :: Text
} deriving Show

newtype DState = V {
                    iidx10 :: Int
                  } deriving Show

interactor :: forall effs. Effects effs => RunConfig -> Item -> Eff effs ApState
interactor RunConfig{..} Item{..} = do
                                      writeFile path $ pre  <> " ~ " <> post <> " !!"
                                      ensure "Blahh" $ P.even iid
                                      log "Hi"
                                      arbitraryIO "This is an arbitrary Put Line" () (PIO.putStrLn "Hello from random action")
                                      tx <- readFile path

                                      when (iid == 140)
                                        $ void $ arbitraryIO "This is an arbitrary THING THAT WILL BLOW UP" "tHIS WILL BLOW UP" (PIO.readFile $ toFilePath invalidFile)

                                      pure $ ApState  {
                                        itemId  = iid,
                                        filePath = path,
                                        exePath = "NOT IMPLEMENTED",
                                        fileText = tx
                                      }

prepState :: ApState -> Ensurable DState
prepState ApState{..} = do
                          ensure  "I do not like 110 in prepstate" (itemId /= 110)
                          pure $ V $ 10 * itemId

i = Item

invalidFile :: Path Abs File  
invalidFile = 
#ifdef mingw32_HOST_OS 
  [absfile|C:\Vids\SystemDesign\Blahhh.txt|] -- windows
#else 
  [absfile|/mnt/c/vids/systemdesign/blahhh.txt|] -- linux
#endif

invalidFile2 :: Path Abs File  
invalidFile2 = 
#ifdef mingw32_HOST_OS 
  [absfile|R:\Vids\SystemDesign\Wrong.txt|] -- windows
#else 
  [absfile|/mnt/r/Vids/SystemDesign/Wrong.txt|] -- linux
#endif

validFile :: Path Abs File  
validFile = 
#ifdef mingw32_HOST_OS 
  [absfile|C:\Vids\SystemDesign\VidList.txt|] -- windows
#else 
  [absfile|/mnt/c/Vids/SystemDesign/VidList.txt|] -- linux
#endif

validFileWithSpace :: Path Abs File  
validFileWithSpace = 
#ifdef mingw32_HOST_OS 
  [absfile|C:\Vids\SystemDesign\Vid List.txt|] -- windows
#else 
  [absfile|/mnt/c/Vids/SystemDesign/Vid List.txt|] -- linux
#endif

items :: [Item]
items = [
          i 100 "Pre"  "Post" validFile $
                              chk "iid x 10 is small" (\V{..} -> iidx10 < 200 ) <>
                              chk "iid x 10 is big"   (\V{..} -> iidx10 > 500),
          i 110 "Pre"  "Post" validFile mempty,
          i 120 "Pre"  "Post" invalidFile2 mempty,
          i 130 "Pre"  "Post" validFile mempty,
          i 140 "Pre"  "Post" validFile mempty,
          i 150 "Pre"  "Post" validFileWithSpace mempty
  ]
  
nameOfModule :: TestModule
nameOfModule = mkTestModule ''ApState

test :: forall effs. Effects effs => Test Item effs ApState DState
test = GenericTest {
              configuration = MemberReflection.testConfig {address = nameOfModule},
              components = TestComponents {
                                testItems = items,
                                testInteractor = interactor,
                                testPrepState = prepState
                            }
            }

data Item = Item {
  iid    :: Int,
  pre    :: Text,
  post   :: Text,
  path   :: Path Abs File,
  checks :: CheckDList DState
} deriving (Show, Generic)


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Reflection %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- https://stackoverflow.com/a/53272316/5589037

newtype WithEffects_ es0 es1 a = WithEffects { unWithEffects :: Members es0 es1 => a }
--
type EFileSystem2 = '[Logger, Ensure, ArbitraryIO, FileSystem]
type WithEffects = WithEffects_ EFileSystem2
--
test2 :: forall effs. WithEffects effs (Test Item effs ApState DState)
test2 = WithEffects test
--
effsRepTest :: Typeable es0 => WithEffects_ es0 es1 a -> R.TypeRep es0
effsRepTest _ = R.typeRep
--
showEffsTest :: Typeable es0 => WithEffects_ es0 es1 a -> Text
showEffsTest = txt . effsRepTest
--
demo :: Text
demo = showEffsTest test2

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class ShowTypes (es :: [* -> *]) where
  showTypes :: [Text]

instance ShowTypes '[] where
  showTypes = []

instance (Typeable e, ShowTypes es) => ShowTypes (e ': es) where
   showTypes = txt (R.typeRep @e) : showTypes @es

showEffs2 :: forall es0 es1 a. ShowTypes es0 => WithEffects_ es0 es1 a -> [Text]
showEffs2 _ = showTypes @es0

demo2 :: [Text]
demo2 = showEffs2 test2
