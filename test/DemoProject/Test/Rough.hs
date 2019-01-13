
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- should not need this: https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}

module DemoProject.Test.Rough where

import           DSL.Logger
import           Check
import DemoProject.Config
import           Control.Monad.Freer
import           Control.Monad
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.ArbitraryIO
import qualified Prelude as P
import           Foundation.Extended             hiding (readFile, writeFile, Item)
import           Foundation.String
import           Runner as R
import Type.Reflection
import Data.Aeson.TH
import GHC.Generics
import qualified Data.Serialize as S
import qualified System.Environment as E

type Effects effs = Members '[Logger, Ensure, ArbitraryIO, FileSystem] effs

-- PROBLEMS
--  - HIE not compiling for ghc 8.6.3
--  - HIE not working on windows for GHC 8.6.2

--  - Intero not installing for 6.3 due to ghci-issues package index not availible
--    -- 8.0.1, 8.0.2, 8.2.1, 8.2.2, 8.4.1, 8.4.2, 8.4.3, 8.6.1
--  - clone intero change stack and intalled
--  - installed stack-run with modifications found on github (custom stack.yaml added to intero directory)
--  - neither haskelly or Haskello eunning in vs code (loading (endlessly) / no type information available)

--  - rollback to ghc 8.4.2 rebuild pyrethru and hie - to as per issue 9725bc0bfcd1b0f112e5e7299d482168fbe1efb3
--   HIE build fails
--      -  src\Haskell\Ide\Engine\Plugin\GhcMod.hs:179:32: error:
--      ot in scope: GM.getTypecheckedModuleGhc'
--      either `GhcMod', `GhcMod.DynFlags', `GhcMod.Error', `GhcMod.Gap',
--             `GhcMod.ModuleLoader', `GhcMod.Monad', `GhcMod.SrcUtils',
--             `GhcMod.Types' nor `GhcMod.Utils' exports getTypecheckedModuleGhc'.
--
--      79 |                               (GM.getTypecheckedModuleGhc' (myLogger rfm) fp)
--
-- Update buildlatest.ps to point to yaml.8.4.2
--  build using script + add one dependency
-- works !!!!!! -- build agasin when HIE issues fixed

config :: TestConfig
config = testConfig {
  header = "This is a Rough Test",
  countries = allCountries
 }

jw = endpoint

endpoint :: (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
endpoint = ep runConfig $ IID 140

data ApState = ApState {
  itemId   :: Int,
  filePath :: Path Abs File,
  exePath :: String,
  fileText :: StrictReadResult
} deriving Show

interactor :: forall effs. Effects effs => (ItemClass Item ValState) => RunConfig -> Item -> Eff effs ApState
interactor RunConfig{..} Item{..} = do
                                      writeFile path $ pre  <> " ~ " <> post <> " !!"
                                      ensure "Blahh" $ P.even iid
                                      log "Hi"
                                      arbitraryIO "This is an arbitrary Put Line" () (putStrLn "Hello from random action")
                                      txt <- readFile path

                                      when (iid == 140)
                                        $ void $ arbitraryIO "This is an arbitrary THING THAT WILL BLOW UP" (Right "tHIS WILL BLOW UP") (readFileUTF8 [absfile|C:\Vids\SystemDesign\Blahhh.txt|])

                                      pure $ ApState  {
                                        itemId  = iid,
                                        filePath = path,
                                        exePath = "NOT IMPLEMENTED",
                                        fileText = txt
                                      }


newtype ValState = V {
                    iidx10 :: Int
                  } deriving Show


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
          i 100 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] $
                              chk "iid x 10 is small" (\V{..} -> iidx10 < 200 ) <>
                              chk "iid x 10 is big"   (\V{..} -> iidx10 > 500),
          i 110 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 120 "Pre"  "Post" [absfile|R:\Vids\SystemDesign\Wrong.txt|]   mempty,
          i 130 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 140 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 150 "Pre"  "Post" [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty
  ]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nameOfModule :: String
nameOfModule = moduleOf ''ApState

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

instance ItemClass Item ValState where
  identifier = iid
  whenClause = pre
  thenClause = post
  checkList = checks

-- $(deriveJSON defaultOptions ''Item)
-- $(deriveJSON defaultOptions ''StrictReadError')
-- $(deriveToJSON defaultOptions ''ApStateDeleteMe)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Reflection %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- https://stackoverflow.com/questions/53272036/freer-simple-how-can-i-generate-a-list-of-effect-members-at-runtime/53272316#53272316
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype WithEffects_ es0 es1 a = WithEffects { unWithEffects :: Members es0 es1 => a }
--
type EFileSystem2 = '[Logger, Ensure, ArbitraryIO, FileSystem]
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
-- 10. group - rollover - go home is home ✔
-- 10.01 - structure demo group ✔
-- 10.02 - fitering groups without prerun ✔
-- 10.03 - execution ✔
-- 10.04 - testing ✔
-- 10.10 - io prerun HOF for excption handling ~ test with / without ✔
-- 10.11 - abstract ✔
-- 11. test case end point ✔
-- 12. logging to file
--    12.01 add hoc and results logging to the same file
--    12.02 include test path in log  ✔
-- 13. Log Formatting and Report Generation
-- 13.1 ~ generalised log type / log protocol  ✔
-- 13.2 ~ serialisation format
    -- log to file
    -- log to both
    -- log summary - data types -> print

-- 14.0 - hedgehog / randomiser
-- 15.0 ~ report generation
