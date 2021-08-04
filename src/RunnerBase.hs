module RunnerBase
  ( AddressedElm (..),
    ItemRunner,
    TestSuite,
    LRB.SuiteItem (..),
    LRB.IsRoot,
    LRB.NotRoot,
    Test (..),
    GenericResult (..),
    concatTests,
    groupAddresses,
    groupName,
    querySuite,
  )
where

import Common (FilterErrorType, FrameworkError, HookCardinality (..))
import Data.Aeson
import Data.Aeson.TH
import Internal.RunnerBaseLazy as LRB
import Polysemy
import Polysemy.Error
import Pyrelude
  ( Applicative ((<*>)),
    Bool (..),
    Category ((.)),
    Either,
    Eq (..),
    Listy (null),
    Maybe (..),
    Monad ((>>=)),
    Ord (..),
    Ordering (..),
    Show,
    Text,
    error,
    fromJust,
    fromMaybe,
    isNothing,
    not,
    otherwise,
    uu,
    ($),
    (&&),
    (<$>),
    (?),
    (||),
  )
import RunElementClasses

type ItemRunner e as ds i hi tc rc effs =
  rc -> ModuleDomain -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs ()

type TestSuite e tc rc effs a =
  (forall hi i as ds. (Show i, Show as, Show ds) => ModuleDomain -> hi -> Test e tc rc hi i as ds effs -> a) -> SuiteItem IsRoot () effs [a]

--  (forall hi i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, Show as, Show ds, Show i, ToJSON i) => Test e tc rc hi i as ds effs -> a) -> SuiteItem () effs [a]

data GenericResult tc rslt = TestResult
  { configuration :: tc,
    results :: Either FilterErrorType [rslt]
  }
  deriving (Show)

queryElm' :: forall r hi effs a. (a -> Text) -> ModuleDomain -> SuiteItem r hi effs [a] -> [AddressedElm a]
queryElm' getItemTitle domain =
  let badCall f = f $ error "Bad param - this param should never be called"
  in \case
        Group {title = t, gElms} -> gElms >>= queryElm' getItemTitle (addLayer domain t)
        Tests {tests} -> (\t -> AddressedElm (elementDomain domain (getItemTitle t)) t) <$> tests
        -- beforeHook, afterHook and root do not contribute to the domain
        BeforeHook {title = t, bhElms} -> bhElms >>= queryElm' getItemTitle domain . badCall
        AfterHook {title = t, ahElms} -> ahElms >>= queryElm' getItemTitle domain . badCall
        Root {rootElms} -> rootElms >>= queryElm' getItemTitle domain

querySuite :: forall hi effs a. (a -> Text) -> SuiteItem IsRoot hi effs [a] -> [AddressedElm a]
querySuite getItemTitle = queryElm' getItemTitle rootDomain

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

concatTests :: SuiteItem r hi effs t -> [t]
concatTests = uu

-- let
--   concat' ts = mconcat $ concatTests <$> ts
-- in
--   \case
--     (Tests f) -> [f]
--     (BeforeHook _ _ _ ts) -> concat' ts
--     (AfterHook _ _ _ ts) -> concat' ts
--     (Group _ ts) -> concat' ts

groupName :: SuiteItem r hi effs a -> Maybe Text
groupName = \case
  Tests _ -> Nothing
  BeforeHook {} -> Nothing
  AfterHook {} -> Nothing
  Root {} -> Nothing
  Group t _ -> Just t

groupAddresses' :: [Text] -> Text -> SuiteItem r hi effs a -> [Text]
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

groupAddresses :: SuiteItem r hi effs a -> [Text]
groupAddresses = groupAddresses' [] ""

-- data up to here want items to have title and validations
-- and config

data Test e tc rc hi i as ds effs = Test
  { config :: tc,
    items :: rc -> [i],
    interactor :: rc -> hi -> i -> Sem effs as,
    parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
  }
