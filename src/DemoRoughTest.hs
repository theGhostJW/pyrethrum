
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
import qualified Prelude as P
import           Foundation.Extended             hiding (readFile, writeFile, Item)
import           Runner

type Effects effs = EFFFileSystem effs

config :: TestConfig
config = testConfig { header = "This is a Rough Test" }

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

prepState :: ApState -> ValState
prepState ApState{..} = V $ 10 * itemId

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

items = [
          i 100 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] $
                                chk "iid is small" (\V{..} -> iidx10 < 200 ) <>
                                chk "iid is big"   (\V{..} -> iidx10 > 500),
          i 110 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 120 "Pre"  "Post"   [absfile|R:\Vids\SystemDesign\Wrong.txt|]   mempty,
          i 130 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 140 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
          i 150 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty
  ]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute :: forall effs. Effects effs => (Test Item (Eff effs ApState) ApState ValState -> IO ()) -> IO ()
execute f = f test
--
-- exDocAll :: IO ()
-- exDocAll = runAllDoc test
--
-- exAll :: IO ()
-- exAll = runAllFull test

exAll :: IO ()
exAll = execute runAllFull


-- exDocAll' :: (ItemClass i vs, Show i, Show as, Show vs) => (Test i (Eff '[FileSystem, Logger, Ensure, Error FileSystemError, Error EnsureError, IO] as) as vs -> IO ()) -> IO ()
-- exDocAll' f = f test

-- data Test1
--   -- | @Fold @ @ step @ @ initial @ @ extract@
--   --  = forall x. Fold (x -> a -> x) x (x -> b
--   = forall i vs as. (ItemClass i vs, Show i, Show as, Show vs) => Test1 ((Test i (Eff '[FileSystem, Logger, Ensure, Error FileSystemError, Error EnsureError, IO] as) as vs -> IO ()) -> IO ())


test :: forall effs. Effects effs => Test Item (Eff effs ApState) ApState ValState
test = GenericTest {
              address = moduleOf ''ApState,
              configuration = config,
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
  -- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Approach 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  -- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interactorEffs :: Eff effs ()
interactorEffs = undefined
