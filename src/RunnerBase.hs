module RunnerBase
  ( queryElm,
    ItemRunner,
    TestSuite,
    LRB.SuiteItem (..),
    Test (..),
    GenericResult (..),
    concatTests,
    groupAddresses,
    groupName,
  )
where

import Common (FilterErrorType, FrameworkError, HookCardinality (..))
import Data.Aeson
import Data.Stack
import Internal.RunnerBaseLazy as LRB
import Polysemy
import Polysemy.Error
import Pyrelude
import RunElementClasses

type ItemRunner e as ds i hi tc rc effs =
  rc -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs ()

type TestSuite e tc rc effs a =
  (forall hi i as ds. (Show i, Show as, Show ds) => hi -> Test e tc rc hi i as ds effs -> a) -> SuiteItem () effs [a]

--  (forall hi i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, Show as, Show ds, Show i, ToJSON i) => Test e tc rc hi i as ds effs -> a) -> SuiteItem () effs [a]

data GenericResult tc rslt = TestResult
  { configuration :: tc,
    results :: Either FilterErrorType [rslt]
  }  deriving (Show)

data AddressedElm a = AddressedElm
  { address :: Stack Text,
    element :: a
  }

queryElm' :: forall hi effs a. (a -> Text) -> Stack Text -> SuiteItem hi effs [a] -> [AddressedElm a]
queryElm' getTitle address =
  let 
      badCall f = f $ error "Bad param - this param should never be called"
      newStack ttl = stackPush address ttl
   in \case
        Tests {tests} -> (\t -> AddressedElm (newStack (getTitle t)) t) <$> tests
        BeforeHook {title = t, bhElms} -> bhElms >>= queryElm' getTitle (newStack t) . badCall
        AfterHook {title = t,  ahElms} -> ahElms >>= queryElm' getTitle (newStack t) . badCall
        Group {title = t, gElms} -> gElms >>= queryElm' getTitle (newStack t) 


querySuite :: forall hi effs a. (a -> Text) -> Suite hi effs [a] -> [AddressedElm a]
querySuite gt s = queryElm' gt stackNew (root s)
   

-- queryElm :: forall hi effs a. SuiteItem hi effs [a] -> [a]
-- queryElm =
--   let hkElms = (queryElm . (\f -> f $ error "Bad param - this param should never be accessed") =<<)
--    in \case
--         Tests {tests} -> tests
--         BeforeHook {bhElms} -> hkElms bhElms
--         AfterHook {ahElms} -> hkElms ahElms
--         Group {gElms} -> queryElm =<< gElms

{-
TODO
 SuiteItem Update
  DONE - Add Hook
  DONE - Add TestSuite Tests
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

data Test e tc rc hi i as ds effs = Test
  { config :: tc,
    items :: rc -> [i],
    interactor :: rc -> hi -> i -> Sem effs as,
    parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
  }
