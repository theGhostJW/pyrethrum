
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- should not need this: https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}

module DemoRoughTest where

import           DSL.Logger
import           Control.Monad.Freer.Error
import           Check
import DemoConfig
import           TestAndRunConfig
import           Control.Monad.Freer
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import Data.Either
import qualified Prelude as P
import           Foundation.Extended             hiding (readFile, writeFile, Item)
import           Runner as R
import Type.Reflection

type Effects effs = EFFFileSystem effs

config :: TestConfig
config = testConfig {
  header = "This is a Rough Test",
  countries = allCountries
 }

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  fileText :: StrictReadResult
} deriving Show

interactor :: forall effs. Effects effs => (ItemClass Item ValState) => RunConfig -> Item -> Eff effs ApState
interactor RunConfig{..} Item{..} = do
                                      writeFile path $ pre  <> " ~ " <> post <> " !!"
                                      ensure "Blahh" $ P.even iid
                                      txt <- readFile path
                                      pure $ ApState iid path txt

newtype ValState = V {
                    iidx10 :: Int
                  } deriving Show

-- change to Ensure eff
prepState :: ApState -> Ensurable ValState
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
                    checks :: CheckList ValState
                  } deriving Show

i = Item

-- should be :: RunConfig -> [Item]
-- later optional hedgehog
items :: [Item]
items = [
          i 100 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] $
                                chk "iid x 10 is small" (\V{..} -> iidx10 < 200 ) <>
                                chk "iid x 10 is big"   (\V{..} -> iidx10 > 500),
          i 110 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 120 "Pre"  "Post"   [absfile|R:\Vids\SystemDesign\Wrong.txt|]   mempty,
          i 130 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 140 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 150 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty
  ]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :: forall effs. Effects effs => Test Item effs ApState ValState
test = GenericTest {
              configuration = config {address = moduleOf ''ApState},
              components = TestComponents {
                                testItems = items,
                                testInteractor = interactor,
                                testPrepState = prepState
                            }
            }

instance ItemClass Item ValState where
  identifier = iid
  whenClause = pre
  thenClause = post
  checkList = checks

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Reflection %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- https://stackoverflow.com/questions/53272036/freer-simple-how-can-i-generate-a-list-of-effect-members-at-runtime/53272316#53272316
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype WithEffects_ es0 es1 a = WithEffects { unWithEffects :: Members es0 es1 => a }
--
type EFileSystem2 = '[Logger, Ensure, FileSystem]
type WithEffects = WithEffects_ EFileSystem2
--
test2 :: forall effs. WithEffects effs (Test Item effs ApState ValState)
test2 = WithEffects test
--
effsRepTest :: Typeable es0 => WithEffects_ es0 es1 a -> TypeRep es0
effsRepTest _ = typeRep
--
showEffsTest :: Typeable es0 => WithEffects_ es0 es1 a -> String
showEffsTest = show . effsRepTest
--
demo :: String
demo = showEffsTest test2

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class ShowTypes (es :: [* -> *]) where
  showTypes :: [String]

instance ShowTypes '[] where
  showTypes = []

instance (Typeable e, ShowTypes es) => ShowTypes (e ': es) where
  showTypes = show (typeRep @e) : showTypes @es

showEffs2 :: forall es0 es1 a. ShowTypes es0 => WithEffects_ es0 es1 a -> [String]
showEffs2 _ = showTypes @es0

demo2 :: [String]
demo2 = showEffs2 test2

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Approach 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- 1. interpret AppSate ~ will probably need in IO   ✔
--    ?? FAIL with type signature ~ Ambigous type variable
-- 1.1 Call multiple tests  ✔
-- 1.2 constructor ✔
-- 2. call multiple items from test list ✔
-- 3. inject separate logger ✔
-- 4. log ✔
-- 5. reinstate testInfo - including left ✔
-- 6. Generalise ✔
-- 7. ensure on prepstate ✔
-- 8. another testinfo constructor for failed prepstate ✔
-- 9. test filter ✔
-- 10. group - rollover - go home is home
-- 10.01 - structure demo group
-- 10.02 - fitering groups without prerun
-- 10.03 - execution
-- 10.10 - io prerun HOF for error handling ~ test with / without
-- 10.11 - abstract ??
-- 11. test case end point
-- 11. ~ try merging into typeclass
-- 12. logging to file
--    12.01 add hoc and results logging to the same file
--    12.02 include test path in log
-- 13. Log Formatting and Report Generation
-- 13.1 ~ generalised log type / log protocol
-- 13.2 ~ serialisation format
-- 14.0 - hedgehog / randomiser
-- 15.0 ~ report generation
