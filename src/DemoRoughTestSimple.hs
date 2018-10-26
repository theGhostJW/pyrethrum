
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}

module DemoRoughTestSimple where

import           Check
import DSL.Logger
import DemoConfig
import           TestAndRunConfig
import DSL.Ensure
import Runner
import           Control.Monad.Freer
import           DSL.Interpreter
import           Foundation.Extended hiding (Item)
import           Control.Monad.Freer.Error
import           DSL.FileSystem
import qualified Prelude as P

type Effects effs = EFFEnsureOnly effs

config :: TestConfig
config = testConfig { header = "This Simple Test Only Uses Ensure Effects" }

data ApState = ApState {
  itemId :: Int,
  simpleMessage :: String
} deriving Show

type ValState = ApState

interactor :: forall effs. Effects effs => (ItemClass Item ValState) => RunConfig -> Item -> Eff effs ApState
interactor rc TestItem{..} = do
                              ensure "Only even iids expected" $ P.even iid
                              pure $ ApState iid "Success"

prepState :: ApState -> ValState
prepState = id

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


execute :: Effects effs => (Test Item (Eff effs ApState) ApState ValState -> IO ()) -> IO ()
execute f = f test

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

runApState :: (Functor f1, Functor f2, Effects effs) =>
     (Item -> ApState -> ValState -> b)
     -> RunConfig
     -> (Eff effs ApState -> f1 (f2 ApState))
     -> Item
     -> f1 (f2 b)
runApState agg rc intrprt itm = let
                                   runVals as = agg itm as $ prepState as
                                in
                                   (runVals <$>) <$> intrprt (interactor rc itm)

runAllItems :: (Functor f1, Functor f2, Effects effs) =>
     (Item -> f2 b -> b)                   -- recover from either
     -> (Item -> ApState -> ValState -> b)
     -> RunConfig
     -> (Eff effs ApState -> f1 (f2 ApState))
     -> [f1 b]
runAllItems frmEth agg rc intrprt = (\itm -> frmEth itm <$> runApState agg rc intrprt itm) <$> items

runLogAllItems :: (Monad m, Effects effs) =>
                   (TestInfo Item as vs -> m b)                             -- logger
                   -> (Item -> ApState -> ValState -> TestInfo Item as vs)  -- rslt constructor
                   -> RunConfig                                             -- runConfig
                   -> (Eff effs ApState -> m (Either AppError ApState))     -- interpreter
                   -> [m b]
runLogAllItems logger agg rc intrprt = (logger =<<) <$> runAllItems recoverTestInfo agg rc intrprt
