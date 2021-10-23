module RunnerBase
  ( AddressedElm (..),
    ItemRunner,
    TestSuite,
    RBL.Suite (..),
    RBL.SuiteItem (..),
    Test (..),
    TestInfo (..),
    GenericResult (..),
    queryElm,
    querySuite',
    querySuite,
    testInfo,
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
    pure,
    toList,
    toS,
    undefined,
    uu,
    ($),
    (&&),
    (<$>),
    (<>),
    (?),
    (||),
  )
import RunElementClasses (Address, AddressElemType, AddressedElm (..), Config, TestFilterResult (TestFilterResult), push, rootAddress)
import qualified RunElementClasses as RC

type ItemRunner e as ds i hi tc rc effs =
  rc -> Address -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs ()

type TestSuite e tc rc effs a =
  ( forall ho i as ds.
    (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, HasField "checks" i (C.Checks ds), HasField "id" i Int, HasField "title" i Text) =>
    Address ->
    Sem effs ho ->
    (ho -> Sem effs ()) ->
    Test e tc rc ho i as ds effs ->
    a
  ) ->
  Suite () effs a

data GenericResult tc rslt = TestResult
  { configuration :: tc,
    results :: Either FilterErrorType [rslt]
  }
  deriving (Show)

queryElm :: forall c hi effs a. (a -> Text) -> Address -> SuiteItem hi effs a -> [AddressedElm a]
queryElm title' address =
  let beUndefined = undefined

      aeUndefined ho = undefined

      tstAddress :: a -> Address
      tstAddress a = push (title' a) RC.Test address

      grpAddress' :: AddressElemType -> Text -> Address
      grpAddress' et ttl = push ttl et address

      hkQuery' :: forall hii cc. AddressElemType -> Text -> [SuiteItem hii effs a] -> [AddressedElm a]
      hkQuery' et t e = e >>= queryElm title' (grpAddress' et t)

      hkQuery :: forall hii cc. Text -> [SuiteItem hii effs a] -> [AddressedElm a]
      hkQuery = hkQuery' RC.Hook
   in \case
        Tests {tests} -> (\a -> AddressedElm (tstAddress a) a) <$> tests address beUndefined aeUndefined
        Group {title = t, gElms = e} -> hkQuery' RC.Group t e
        BeforeAll {title = t, bhElms = e} -> hkQuery t e
        BeforeEach {title' = t, bhElms' = e} -> hkQuery t e
        AfterAll {title = t, ahElms = e} -> hkQuery t e
        AfterEach {title' = t, ahElms' = e} -> hkQuery t e

querySuiteElms :: forall c hi effs a. (a -> Text) -> Address -> Suite hi effs a -> [AddressedElm a]
querySuiteElms title' address suite = un suite >>= queryElm title' address

querySuite' ::
  forall e tc rc effs a.
  RC.Config tc =>
  rc ->
  (a -> Text) -> -- get title
  ( forall ho i as ds. -- data extractor
    (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, RC.Config tc, RC.ItemClass i ds) =>
    rc ->
    Address ->
    Test e tc rc ho i as ds effs ->
    a
  ) ->
  TestSuite e tc rc effs a -> -- suiite
  [AddressedElm a]
querySuite' rc title' extractor suite =
  let fullQuery ::
        (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, RC.ItemClass i ds) =>
        Address ->
        Sem effs ho -> -- beforeEach
        (ho -> Sem effs ()) -> -- AfterEach
        Test e tc rc ho i as ds effs ->
        a
      fullQuery a _be _ae t = extractor rc a t

      root :: Suite () effs a
      root = suite fullQuery
   in querySuiteElms title' rootAddress root

querySuite :: forall rc e tc effs. Config tc => rc -> TestSuite e tc rc effs (TestInfo tc) -> [AddressedElm (TestInfo tc)]
querySuite rc suite =
  let extractor :: forall ho i as ds. RC.ItemClass i ds => rc -> Address -> Test e tc rc ho i as ds effs -> TestInfo tc
      extractor rc' _add t = testInfo rc' t

      title' :: TestInfo tc -> Text
      title' = getField @"title"
   in querySuite' rc title' extractor suite

data CheckInfo = CheckInfo
  { header :: Text,
    expectation :: C.ResultExpectation
  }
  deriving (Show)

data ItemInfo = ItemInfo
  { id :: Int,
    title :: Text,
    checks :: [CheckInfo]
  }
  deriving (Show)

data TestInfo tc = TestInfo
  { title :: Text,
    config :: tc,
    itemInfo :: [ItemInfo]
  }
  deriving (Show)

testInfo :: forall e tc rc hi i as ds effs. (RC.ItemClass i ds, RC.Config tc) => rc -> Test e tc rc hi i as ds effs -> TestInfo tc
testInfo rc t =
  let ckInfo :: C.Check ds -> CheckInfo
      ckInfo c = CheckInfo (C.header c) (C.expectation c)

      iinfo :: i -> ItemInfo
      iinfo i =
        ItemInfo
          { id = getField @"id" i,
            title = getField @"title" i,
            checks = ckInfo <$> (toList . C.un $ getField @"checks" i)
          }

      cfg :: tc
      cfg = (config :: Test e tc rc hi i as ds effs -> tc) t
   in TestInfo
        { title = getField @"title" cfg,
          config = cfg,
          itemInfo = iinfo <$> items t rc
        }

data Test e tc rc hi i as ds effs = Test
  { config :: tc,
    items :: rc -> [i],
    interactor :: rc -> hi -> i -> Sem effs as,
    parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
  }
