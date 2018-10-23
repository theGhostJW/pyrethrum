
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
          -- i 100 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] $
          --                       chk "iid is small" (\V{..} -> iidx10 < 200 ) <>
          --                       chk "iid is big"   (\V{..} -> iidx10 > 500),
          -- i 110 "Pre"  "Post"   [absfile|C:\Vids\SystemDesign\VidList.txt|] mempty,
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

-- 1. interpret AppSate ~ will probably need in IO   ✔
--    ?? FAIL with type signatur ~ Ambigous type variable
-- 1.1 Call multiple tests  ✔
-- 1.2 constructor ✔
-- 2. call multiple items from test list
-- 3. inject separate logger
-- 4. log
-- 5. Generalise
-- 6. ensure on prepstate
-- >>

-- (i -> as -> vs -> ag)
-- runApState :: RunConfig -> (forall effs. Effects effs => Eff effs ApState -> IO (Either AppError ApState)) -> IO (Either AppError ApState)
runApState agg rc intrprt = let
                               itm = P.head items
                               runVals as = agg itm as $ prepState as
                            in
                               (runVals <$>) <$> intrprt (interactor rc itm)

runApStatePrint agg intrprt = runApState agg runConfig intrprt >>= P.print
-- --
demoAsp :: IO ()
demoAsp = runApStatePrint testInfoFull executeFileSystemInIOCopy


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Approach 2- FAIL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interactorEffs :: forall effs. Effects effs =>
                                RunConfig ->
                                (forall as vs i. ItemClass i vs => i -> as -> vs -> TestInfo i as vs) ->
                                Eff effs [IO ()]
interactorEffs rc agf = do
                         let
                            runitem :: Item -> Eff effs (IO ())
                            runitem itm = do
                                          as <- interactor rc itm
                                          log $ agf itm as $ prepState as
                                          pure $ pure ()
                         P.traverse runitem items

--- Single item maybe this will be usefull ??
interactorEffsSingleItem :: forall effs. Effects effs =>
                                RunConfig ->
                                (forall as vs i. ItemClass i vs => i -> as -> vs -> TestInfo i as vs) ->
                                Item ->
                                Eff effs (Either AppError (TestInfo Item ApState ValState))
interactorEffsSingleItem rc agf itm = do
                                        as <- interactor rc itm
                                        let rslt = agf itm as $ prepState as
                                        log rslt
                                        pure $ pure rslt

--- may be usufull ??
interactList :: forall effs. Effects effs => RunConfig ->
            (forall as vs i. ItemClass i vs => i -> as -> vs -> TestInfo i as vs) ->
            [Eff effs (IO ())]
interactList rc agf = let
                        rsltLst :: [Eff effs (Either AppError (TestInfo Item ApState ValState))]
                        rsltLst = interactorEffsSingleItem rc agf <$> items

                      in
                        (P.print <$>) <$> rsltLst
