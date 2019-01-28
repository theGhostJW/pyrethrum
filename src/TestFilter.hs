module TestFilter where 

import           Foundation.Extended
import Data.Aeson.TH
import OrphanedInstances
import Data.Functor.Identity
import RunnerBase

type TestAddress = String


data FilterRejection tc = FilterRejection {
                 reason :: String,
                 cfg    :: tc
                 } deriving (Eq, Show)

$(deriveJSON defaultOptions ''FilterRejection)

data TestFilter rc tc = TestFilter {
  title :: String,
  predicate :: rc -> tc -> Bool
}

type TestFilters rc tc = [TestFilter rc tc]

filterTestCfg :: forall rc tc. TestFilters rc tc -> rc -> tc -> Either (FilterRejection tc) tc
filterTestCfg fltrs rc tc =
  let
    applyFilter :: TestFilter rc tc -> Either (FilterRejection tc) tc
    applyFilter fltr = predicate fltr rc tc ?
                                        Right tc $
                                        Left $ FilterRejection (title fltr) tc
  in
    fromMaybe (pure tc) $ find isLeft $ applyFilter <$> fltrs

filterTest :: forall i as vs tc rc effs.  TestFilters rc tc -> rc -> GenericTest tc rc i effs as vs -> Identity (Either (FilterRejection tc) tc)
filterTest fltrs rc t = Identity $ filterTestCfg fltrs rc $ (configuration :: (GenericTest tc rc i effs as vs -> tc)) t

filterGroups :: forall tc rc effs.
              (
                (forall i as vs. (Show i, Show as, Show vs) =>
                      GenericTest tc rc i effs as vs -> Identity (Either (FilterRejection tc) tc)) -> [TestGroup Identity (Either (FilterRejection tc)) tc effs]
              )
              -> TestFilters rc tc
              -> rc
              -> [[Either (FilterRejection tc) tc]]
filterGroups groupLst fltrs rc =
    let
      testFilter :: GenericTest tc rc i effs as vs -> Identity (Either (FilterRejection tc) tc)
      testFilter = filterTest fltrs rc
    in
      (runIdentity <$>) <$> (tests <$> groupLst testFilter)

filterLog :: forall tc. [[Either (FilterRejection tc) tc]] -> [Either (FilterRejection tc) tc]
filterLog = mconcat

filterGroupFlags :: forall tc. [[Either (FilterRejection tc) tc]] -> [Bool]
filterGroupFlags grpFltrRslts = any isRight <$> grpFltrRslts
