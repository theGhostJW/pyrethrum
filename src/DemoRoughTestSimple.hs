
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

-- 1. interpret AppSate ~ will probably need in IO
--    ?? FAIL ~ Ambigous type variable but works with no type signature
-- 2. call injecting ap state
-- 3. call multiple from test list
-- 4. inject separate logger
-- 5. constructor and log
-- >> Profit

-- runApState :: RunConfig -> (forall effs. Effects effs => Eff effs ApState -> IO (Either AppError ApState)) -> IO (Either AppError ApState)
-- runApState :: RunConfig -> (forall effs. Effects effs => Eff effs ApState -> IO (Either AppError ApState)) -> IO (Either AppError ApState)
runApState rc intrprt = let
                          itm = P.head items
                        in
                          intrprt $ interactor rc itm

runApStatePrint intrprt = runApState runConfig intrprt >>= P.print

demoAspSimp = runApStatePrint executeFileSystemInIOCopy


  -- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  -- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Approach 2 - Fail %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  -- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interactorEffs :: forall effs. Effects effs =>
                                  RunConfig ->
                                  (forall as vs i. ItemClass i vs => i -> as -> vs -> TestInfo i as vs) ->
                                  Eff effs (IO ())
interactorEffs rc agf = do
                         let
                            runitem :: Item -> Eff effs (IO ())
                            runitem itm = do
                                            as <- interactor rc itm
                                            log $ agf itm as $ prepState as
                                            pure $ pure ()
                         mergeIO <$> P.traverse runitem items

mergeIO :: [IO ()] -> IO ()
mergeIO = foldl' (>>) (pure ())
