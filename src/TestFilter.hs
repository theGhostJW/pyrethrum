module TestFilter where 

import           Prelude as P
import RunnerBase as RB
import RunElementClasses as C
import qualified Data.List as L 
import Control.Monad.Identity
import Data.Text
import Data.Maybe

acceptFilter :: TestFilterResult -> Bool
acceptFilter = isNothing . reasonForRejection

rejectFilter :: TestFilterResult -> Bool
rejectFilter = isJust . reasonForRejection

mkTestFilterResult :: TestConfigClass tc => tc -> Maybe Text -> TestFilterResult
mkTestFilterResult tc rejection = TestFilterResult {
                                testInfo = mkDisplayInfo tc,
                                reasonForRejection = rejection
                              }

data TestFilter rc tc = TestFilter {
  title :: Text,
  predicate :: rc -> tc -> Bool
}

type FilterList rc tc = [TestFilter rc tc]

filterTestCfg :: forall rc tc. TestConfigClass tc => FilterList rc tc -> rc -> tc -> TestFilterResult
filterTestCfg fltrs rc tc = undefined
    

filterTest :: forall i as ds tc rc e effs. TestConfigClass tc => FilterList rc tc -> rc -> Test e tc rc i as ds effs -> Identity (Identity TestFilterResult)
filterTest fltrs rc Test{ config = tc } = Identity . Identity $ filterTestCfg fltrs rc tc

filterGroups :: forall tc rc e effs. TestConfigClass tc =>
              (
                (forall i as ds. (Show i, Show as, Show ds) =>
                      Test e tc rc i as ds effs -> Identity (Identity TestFilterResult)) -> [RunElement Identity Identity TestFilterResult effs]
              )
              -> FilterList rc tc
              -> rc
              -> [[TestFilterResult]]
filterGroups groupLst fltrs rc =
    let
      testFilter :: Test e tc rc i as ds effs -> Identity (Identity TestFilterResult)
      testFilter = filterTest fltrs rc
    in
      (runIdentity . runIdentity <$>) <$> (tests <$> groupLst testFilter)

acceptAnyFilter :: [TestFilterResult] -> Bool
acceptAnyFilter = P.any acceptFilter 