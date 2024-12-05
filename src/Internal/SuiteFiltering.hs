module Internal.SuiteFiltering (
  FilteredSuite (..),
  filterSuite
) where

import Core qualified as C
import Filter
import PyrethrumExtras ((?))
import Prelude hiding (All, atomically, id, newEmptyTMVarIO, newTVarIO, readMVar)
import qualified CoreTypeFamilies as C

{-
todo :: define defect properties with sum type type and typeclass which returns defect info

-}

data FilteredSuite a = MkFilteredSuite
  { suite :: [a],
    filterResults :: [FilterResult Text]
  }

filterFixture :: forall m rc fc hi. (C.Config fc) => Filters rc fc -> rc -> C.Fixture m rc fc hi -> FilterResult Text
filterFixture fltrs rc fx =
  fr
    { rejection =
        fr.rejection
          <|> ( C.fixtureEmpty rc fx
                  ? Just "Empty Fixture - the fixture either has no test data or all test data has been filtered out"
                  $ Nothing
              )
    }
  where
    fr :: FilterResult Text
    fr = (.title) <$> applyFilters fltrs rc (C.getConfig fx)

filterSuite :: forall m rc fc i. (C.Config fc) => Filters rc fc -> rc -> [C.Node m rc fc i] -> FilteredSuite (C.Node m rc fc i)
filterSuite fltrs rc suite' =
  MkFilteredSuite {suite, filterResults}
  where
    (suite, filterResults) = filterSuiteTuple fltrs rc suite'

filterSuiteTuple :: forall m rc fc i. (C.Config fc) => Filters rc fc -> rc -> [C.Node m rc fc i] -> ([C.Node m rc fc i], [FilterResult Text])
filterSuiteTuple fltrs rc suite =
  (reverse fNodes, fRslts)
  where
    (fNodes, fRslts) = foldl' filterNode ([], []) suite

    filterSuite' :: forall i'. [C.Node m rc fc i'] -> ([C.Node m rc fc i'], [FilterResult Text])
    filterSuite' = filterSuiteTuple fltrs rc

    filterNode :: forall hi. ([C.Node m rc fc hi], [FilterResult Text]) -> C.Node m rc fc hi -> ([C.Node m rc fc hi], [FilterResult Text])
    filterNode (accNodes, fltrInfo) = \case
      C.Hook {..} ->
        (null sn ? accNodes $ C.Hook {subNodes = sn, ..} : accNodes, fr)
        where
          (sn, fr) = filterSuite' subNodes
      fx@C.Fixture {fixture} ->
        (accepted fr ? fx : accNodes $ accNodes, fr : fltrInfo)
        where
          fr = filterFixture fltrs rc fixture
