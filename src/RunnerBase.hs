module RunnerBase
  ( AddressedElm (..),
    ItemRunner,
    TestSuite,
    RBL.SuiteItem (..),
    RBL.One,
    RBL.Many,
    RBL.Root',
    Test (..),
    GenericResult (..),
    queryElm,
    querySuite',
    querySuite,
  )
where

import qualified Check as C
import Common (FilterErrorType, FrameworkError)
import Data.Aeson hiding (One)
import Data.Aeson.TH
import GHC.Records (HasField, getField)
import Internal.RunnerBaseLazy as RBL
import Polysemy
import Polysemy.Error
import Pyrelude
  ( Applicative ((<*>)),
    Bool (..),
    Category ((.)),
    Either,
    Eq (..),
    Int,
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
    toList,
    toS,
    uu,
    ($),
    (&&),
    (<$>),
    (<>),
    (?),
    (||),
  )
import RunElementClasses (Address, AddressElemType, AddressedElm (..), push, rootAddress, Config)
import qualified RunElementClasses as RC

type ItemRunner e as ds i hi tc rc effs =
  rc -> Address -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs ()

type TestSuite e tc rc effs a =
  (forall ho hi i as ds. (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, HasField "checks" i (C.Checks ds), HasField "id" i Int, HasField "title" i Text) => Address -> hi -> (hi -> Sem effs ho) -> (ho -> Sem effs ()) -> Test e tc rc ho i as ds effs -> a) -> SuiteItem Root' () effs a

data GenericResult tc rslt = TestResult
  { configuration :: tc,
    results :: Either FilterErrorType [rslt]
  }
  deriving (Show)

queryElm :: (a -> Text) -> Address -> SuiteItem c hi effs a -> [AddressedElm a]
queryElm getTitle address = 
  let 
    hiNull= Error "hi in query should not be referenced" 
    beNull = Error "be in query should not be referenced" 
    aeNull = Error "ae in query should not be referenced"
    {- 
      --     nextAddress :: Text -> RC.AddressElemType -> Address
  --     nextAddress ttl et = push ttl et address

  --     elmQuery :: forall hi' ho' c1. AddressElemType -> Text -> [Address -> hi' -> SuiteItem c1 hi' ho' effs [a]] -> [AddressedElm a]
  --     elmQuery et ttl elms = elms >>= queryElm getItemTitle (nextAddress ttl et) . badCall

  --     hkQuery :: forall hi' ho' c1. Text -> [Address -> hi' -> SuiteItem c1 hi' ho' effs [a]] -> [AddressedElm a]
  --     hkQuery = elmQuery RC.Hook
  --  in \case
  --       Root {rootElms} -> rootElms >>= queryElm getItemTitle address
  -}

  in
  \case
    Root {rootElms} -> rootElms >>= queryElm getTitle address
    --  Address -> hi -> (hi -> Sem effs ho) -> (ho -> Sem effs ()) -> [t]
    Tests {tests} -> uu -- (\i -> AddressedElm (push (getItemTitle i) RC.Test address) i) <$> tests
    Group {title = t, gElms = e} -> uu --elmQuery RC.Group t e
    BeforeAll {title = t, bhElms = e} -> uu --hkQuery t e
    BeforeEach {title' = t, bhElms' = e} -> uu --hkQuery t e
    AfterAll {title = t, ahElms = e} -> uu --hkQuery t e
    AfterEach {title' = t, ahElms' = e} -> uu --hkQuery t e


querySuite' :: forall e tc rc effs a. 
  rc ->
  (a -> Text) ->  -- get title
  ( forall ho i as ds. -- data extractor
    (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, RC.ItemClass i ds) =>
    rc ->
    Address ->
    Test e tc rc ho i as ds effs ->
    a
  ) 
  -> TestSuite e tc rc effs a -- suiite
  -> [AddressedElm a]
querySuite' rc getTitle extractor suite = 
  let 
    fullQuery :: (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, RC.ItemClass i ds) =>
      Address ->
      hi ->
      (hi -> Sem effs ho) -> -- beforeEach
      (ho -> Sem effs ()) -> -- AfterEach
      Test e tc rc ho i as ds effs ->
      a
    fullQuery a _hi _be _ae t = extractor rc a t


    root :: SuiteItem Root' () effs a
    root = suite fullQuery
  in 
    queryElm getTitle rootAddress root


querySuite :: forall hi effs a. (a -> Text) -> SuiteItem Root' hi effs a -> [AddressedElm a]
querySuite getItemTitle = uu --queryElm getItemTitle rootAddress

  
  -- uu --queryElm getItemTitle rootAddress
-- querySuite getItemTitle = queryElm getItemTitle rootAddress

-- queryTest ::
--   (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, RC.ItemClass i ds) =>
--   Address ->
--   hi ->
--   (hi -> Sem effs ho) -> -- beforeEach
--   (ho -> Sem effs ()) -> -- AfterEach
--   Test e tc rc hi i as ds effs ->
--   a
-- queryTest = uu


-- querySuite :: (RC.Config rc, RC.Config tc) => (forall ho hi i as ds. (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, HasField "checks" i (C.Checks ds), HasField "id" i Int, HasField "title" i Text) => rc -> tc -> i -> a) -> TestSuite e tc rc effs a
-- querySuite = uu
-- forall ho hi i as ds. (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, HasField "checks" i (Check.Checks ds), HasField "id" i Int, HasField "title" i Text) => Address -> hi -> (hi -> Sem effs ho) -> (ho -> Sem effs ()) -> Test e tc rc ho i as ds effs -> a

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

data CheckInfo = CheckInfo {
  header :: Text,
  expectation :: C.ResultExpectation
}

data ItemInfo = ItemInfo {
  id :: Int,
  title :: Text,
  checks :: [CheckInfo]
}

data TestInfo tc = TestInfo {
  title :: Text,
  config :: tc,
  itemInfo :: [ItemInfo]
}

info :: forall e tc rc hi i as ds effs. (RC.ItemClass i ds, RC.Config tc) => rc -> Test e tc rc hi i as ds effs -> TestInfo tc 
info rc t = 
  let 
    ckInfo :: C.Check ds -> CheckInfo
    ckInfo c = CheckInfo (C.header c) (C.expectation c)

    iinfo :: i -> ItemInfo
    iinfo i = ItemInfo {   
      id = getField @"id" i,
      title = getField @"title" i,
      checks = ckInfo <$> (toList . C.un $ getField @"checks" i )
      }

    cfg :: tc
    cfg = (config :: Test e tc rc hi i as ds effs -> tc) t

  in
    TestInfo {
      title = getField @"title" cfg,
      config = cfg,
      itemInfo = iinfo <$> (items t) rc
    }

data Test e tc rc hi i as ds effs = Test
  { config :: tc,
    items :: rc -> [i],
    interactor :: rc -> hi -> i -> Sem effs as,
    parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
  }
