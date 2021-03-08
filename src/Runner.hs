{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Runner (
  RunParams(..)
  , module RB
  , module ItemFilter
  , module C
) where

import Common as C
    ( dList,
      indentText,
      DetailedInfo(..),
      FileSystemErrorType(..),
      FilterErrorType(..),
      FrameworkError(..),
      OutputDListText,
      PreTestStage(..) )
import DSL.Interpreter ( ApEffs )
import DSL.Logger ( logItem, Logger )
import DSL.LogProtocol as LP
import DSL.CurrentTime ( utcOffset )
import Polysemy
import Polysemy.Error as PE ( Error, catch, throw )
import ItemFilter  (ItemFilter (..), filterredItemIds)
import qualified Data.Set as S
import RunElementClasses as C
import Data.Aeson as A
import TestFilter
    ( acceptFilter,
      filterGroups,
      filterTestCfg,
      FilterList,
      TestFilter(..), acceptAnyFilter )
import RunnerBase as RB
    ( doNothing,
      Ensurable,
      GenericResult(..),
      HookLocation(..),
      ItemRunner,
      PreRun(..),
      RunElement(..),
      Test(..),
      TestPlanBase )
import Prelude

logBoundry :: forall e effs. (Show e, A.ToJSON e, Member (Logger e) effs) => BoundaryEvent -> Sem effs ()
logBoundry = logItem . BoundaryLog

runTestItems :: forall i as ds tc rc e effs. (ToJSON e, Show e, TestConfigClass tc, ItemClass i ds, Member (Logger e) effs) =>
      Maybe (S.Set Int)                                                    -- target Ids
      -> [i]   
      -> rc                                                            -- items
      -> Test e tc rc i as ds effs
      -> ItemRunner e as ds i tc rc effs
      -> [Sem effs ()]
runTestItems iIds items rc test@Test{ config = tc } itemRunner =
  let
    startTest :: Sem effs ()
    startTest = logBoundry . StartTest $ mkDisplayInfo tc

    endTest :: Sem effs ()
    endTest = logBoundry . EndTest $ moduleAddress tc

    filteredItems :: [i]
    filteredItems = filter inTargIds items

    applyRunner :: i -> Sem effs ()
    applyRunner i =  
      let
        iid :: ItemId
        iid = ItemId (moduleAddress tc) (identifier i)
      in
        do
          logBoundry . StartIteration iid (WhenClause $ whenClause i) (ThenClause $ thenClause i) $ toJSON i
          itemRunner rc test i
          logBoundry $ EndIteration iid

    inTargIds :: i -> Bool
    inTargIds i = maybe True (S.member (identifier i)) iIds

  in
    case filteredItems of
      [] -> []
      [x] -> [startTest *> applyRunner x *> endTest]
      x : xs -> (startTest *> applyRunner x)
                : (applyRunner <$> Prelude.init xs)
                <> [applyRunner (Prelude.last xs) *> endTest]

runTest ::  forall i rc as ds tc e effs. (ItemClass i ds, TestConfigClass tc, ToJSON e, Show e, Member (Logger e) effs) =>
                   RunParams e rc tc effs                -- Run Params
                   -> Test e tc rc i as ds effs          -- Test Case
                   -> [Sem effs ()]                      -- [TestIterations]
runTest RunParams {filters, rc, itemIds, itemRunner}  test@Test {config = tc, items} =
    if acceptFilter (filterTestCfg filters rc tc)
    then runTestItems itemIds (items rc) rc test itemRunner
    else []

logLPError ::  forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => FrameworkError e -> Sem effs ()
logLPError = logItem . logRun . LP.Error

data RunParams e rc tc effs = RunParams {
  plan :: forall mo mi a. TestPlanBase e tc rc mo mi a effs,
  filters :: FilterList rc tc,
  itemIds :: Maybe (S.Set Int),   
  itemRunner :: forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => ItemRunner e as ds i tc rc effs,
  rc :: rc
}

-- TODO - Error handling especially outside tests 
exeElm :: RunElement [] (Sem effs) () effs -> Sem effs ()
exeElm = \case 
            Tests tests' -> sequence_ . sequence_ tests'

            Hook location' hook' subTests' -> 
              case location' of 
                BeforeEach -> exeElm $ sequence_ hook' <$> subTests'
                BeforeAll -> sequence_ hook' $ exeElm subTests'
                AfterAll  -> sequence_ exeElm subTests' hook'
                AfterEach -> exeElm $ \t -> sequence_ t hook' <$> subTests'
                
            Group title' subTests' -> 
                do
                  logBoundry . StartGroup $ GroupTitle title'
                  exeElm subTests'
                  logBoundry $ EndGroup title'