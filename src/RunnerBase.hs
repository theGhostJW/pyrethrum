module RunnerBase
  ( AddressedElm (..),
    ItemRunner,
    TestSuite,
    RBL.SuiteItem (..),
    RBL.One,
    RBL.Many,
    Test (..),
    GenericResult (..),
    queryElm,
    querySuite,
  )
where

import Common (FilterErrorType, FrameworkError)
import Data.Aeson hiding (One)
import Data.Aeson.TH
import Internal.RunnerBaseLazy as RBL
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
    Show (show),
    Text,
    error,
    fromJust,
    fromMaybe,
    isNothing,
    not,
    otherwise,
    toS,
    uu,
    ($),
    (&&),
    (<$>),
    (<>),
    (?),
    (||), Int
  )
import RunElementClasses (Address, AddressElemType, AddressedElm (..), push, rootAddress)
import qualified RunElementClasses as RC
import GHC.Records (HasField)
import qualified Check

type ItemRunner e as ds i hi tc rc effs =
  rc -> Address -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs ()

type TestSuite e tc rc effs a =
  (forall hi i as ds. (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, HasField "checks" i (Check.Checks ds), HasField "id" i Int, HasField "title" i Text) => Address -> hi -> Test e tc rc hi i as ds effs -> a) -> SuiteItem One () () effs [a]

data GenericResult tc rslt = TestResult
  { configuration :: tc,
    results :: Either FilterErrorType [rslt]
  }
  deriving (Show)

queryElm :: forall hi ho effs c a. (a -> Text) -> Address -> SuiteItem c hi ho effs [a] -> [AddressedElm a]
queryElm getItemTitle address =
  let badCall :: forall c1 o o1. (Address -> o -> SuiteItem c1 o o1 effs [a]) -> SuiteItem c1 o o1 effs [a]
      badCall f = f address . error $ "Framework Defect - this param should never be accessed when querying for element data: " <> show address

      nextAddress :: Text -> RC.AddressElemType -> Address
      nextAddress ttl et = push ttl et address

      elmQuery :: forall hi' ho' c1. AddressElemType -> Text -> [Address -> hi' -> SuiteItem c1 hi' ho' effs [a]] -> [AddressedElm a]
      elmQuery et ttl elms = elms >>= queryElm getItemTitle (nextAddress ttl et) . badCall

      hkQuery :: forall hi' ho' c1. Text -> [Address -> hi' -> SuiteItem c1 hi' ho' effs [a]] -> [AddressedElm a]
      hkQuery = elmQuery RC.Hook
   in \case
        Root {rootElms} -> rootElms >>= queryElm getItemTitle address
        Tests {tests} -> (\i -> AddressedElm (push (getItemTitle i) RC.Test address) i) <$> tests
        Group {title = t, gElms = e} -> elmQuery RC.Group t e
        BeforeAll {title = t, bhElms = e} -> hkQuery t e
        BeforeEach {title' = t, bhElms' = e} -> hkQuery t e
        AfterAll {title = t, ahElms = e} -> hkQuery t e
        AfterEach {title' = t, ahElms' = e} -> hkQuery t e

querySuite :: forall hi ho effs a. (a -> Text) -> SuiteItem One hi ho effs [a] -> [AddressedElm a]
querySuite getItemTitle = queryElm getItemTitle rootAddress

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

-- concatTests :: SuiteItem hi ho effs t -> [t]
-- concatTests = uu

-- let
--   concat' ts = mconcat $ concatTests <$> ts
-- in
--   \case
--     (Tests f) -> [f]
--     (BeforeHook _ _ _ ts) -> concat' ts
--     (AfterHook _ _ _ ts) -> concat' ts
--     (Group _ ts) -> concat' ts

-- groupName :: SuiteItem hi ho effs a -> Maybe Text
-- groupName = \case
--   Tests _ -> Nothing
--   BeforeAll {} -> Nothing
--   BeforeEach {} -> Nothing
--   AfterAll {} -> Nothing
--   AfterEach {} -> Nothing
--   Root {} -> Nothing
--   Group t _ -> Just t

-- groupAddresses' :: [Text] -> Text -> SuiteItem hi ho effs a -> [Text]
-- groupAddresses' accum root el = uu

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

-- groupAddresses :: SuiteItem hi ho effs a -> [Text]
-- groupAddresses = groupAddresses' [] ""

-- data up to here want items to have title and validations
-- and config

data Test e tc rc hi i as ds effs = Test
  { config :: tc,
    items :: rc -> [i],
    interactor :: rc -> hi -> i -> Sem effs as,
    parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
  }
