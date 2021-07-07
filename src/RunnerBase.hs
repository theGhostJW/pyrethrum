

module RunnerBase (
  ItemRunner,
  Suite,
  LRB.SuiteItem(..),
  Test(..),
  GenericResult(..),
  concatTests,
  groupAddresses,
  groupName
  )
 where

import Common (FilterErrorType, FrameworkError, HookCardinality(..))
import Pyrelude
import Polysemy
import Polysemy.Error
import RunElementClasses
import Data.Aeson
import Internal.RunnerBaseLazy as LRB

type ItemRunner e as ds i hi tc rc effs = 
    rc -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs ()

type Suite e tc rc effs a = 
   (forall hi i as ds. (Show i, Show as, Show ds) => hi -> Test e tc rc hi i as ds effs -> a) -> SuiteItem () effs [a]
    --  (forall hi i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, Show as, Show ds, Show i, ToJSON i) => Test e tc rc hi i as ds effs -> a) -> SuiteItem () effs [a]

data GenericResult tc rslt = TestResult {
  configuration :: tc,
  results :: Either FilterErrorType [rslt]
} deriving Show


{- 
TODO 
 SuiteItem Update
  DONE - Add Hook     
  DONE - Add Suite Tests
    * Thread Hook Output to Subelements (GADT)
      * get compiling
      * utilise input - ie change test runner
    * Update Tests
    * Explicit Hook connstructors
    * Update Tests
    * Query static data - items / checks / Config / Known Defects on Checks / Effects esp Hook effects how to query
  Update Tests
  Concurrency
  Update Tests
  Known Defect on interact / parse 
  Exception error handling
  Update Tests
  sample for actions that return values in the context of documentation
  Update log interpretor 
  Runner that extracts test items - eg to report known errors
  Update Tests
  Update Demo
-}


concatTests :: SuiteItem hi effs t -> [t]
concatTests = uu
  -- let 
  --   concat' ts = mconcat $ concatTests <$> ts
  -- in
  --   \case
  --     (Tests f) -> [f]
  --     (BeforeHook _ _ _ ts) -> concat' ts
  --     (AfterHook _ _ _ ts) -> concat' ts
  --     (Group _ ts) -> concat' ts


groupName :: SuiteItem hi effs a -> Maybe Text
groupName = \case 
              Tests _ -> Nothing
              BeforeHook {} -> Nothing
              AfterHook {} -> Nothing
              Group t _ -> Just t

groupAddresses' :: [Text] -> Text -> SuiteItem hi effs a -> [Text]
groupAddresses' accum root el = uu
  -- let
  --   delim = "."

  --   appendDelim :: Text -> Text -> Text
  --   appendDelim p s = p <> (null p || null s ? empty $ delim) <> s

  --   childAddresses :: [SuiteItem o effs a] -> [Text]
  --   childAddresses  se = mconcat $ groupAddresses' accum root <$> se
  -- in
  --   case el of
  --     Tests _ -> accum
      
  --     BeforeHook _ _ _ subElems -> childAddresses subElems
      
  --     AfterHook _ _ _ subElems -> childAddresses subElems

  --     Group t subElems -> 
  --       let 
  --         address = appendDelim root t 
  --       in 
  --         address : mconcat (groupAddresses' accum address <$> subElems)
        

groupAddresses :: SuiteItem hi effs a -> [Text]
groupAddresses = groupAddresses' [] "" 

-- data up to here want items to have title and validations 
-- and config

data Test e tc rc hi i as ds effs = Test {
  config :: tc,
  items :: rc -> [i],
  interactor :: rc -> hi -> i -> Sem effs as,
  parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
}