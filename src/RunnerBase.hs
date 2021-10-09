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
  ( pure,
    Applicative ((<*>)),
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
    undefined,
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

queryElm :: forall c hi effs a. (a -> Text) -> Address -> SuiteItem c hi effs a -> [AddressedElm a]
queryElm title' address = 
  let 
    hiUndefined :: hi
    hiUndefined = undefined

    beUndefined :: hi -> Sem effs ho
    beUndefined hi = undefined

    aeUndefined :: ho -> Sem effs ()
    aeUndefined ho = undefined

    tstAddress :: a -> Address
    tstAddress a = push (title' a) RC.Test address

    grpAddress' :: AddressElemType -> Text -> Address
    grpAddress' et ttl = push ttl et address

    hkQuery t e = e >>= queryElm title' (grpAddress' RC.Hook t) 
  in
  \case
    Root {rootElms} -> rootElms >>= queryElm title' address
    Tests {tests} -> (\a -> AddressedElm (tstAddress a) a) <$> tests address hiUndefined beUndefined aeUndefined 
    Group {title = t, gElms = e} -> e >>= queryElm title' (grpAddress' RC.Group t) 
    -- BeforeAll {title = t, bhElms = e} -> e >>= queryElm title' (grpAddress' RC.Hook t) 
    BeforeAll {title = t, bhElms = e} -> hkQuery t e
    BeforeEach {title' = t, bhElms' = e} -> e >>= queryElm title' (grpAddress' RC.Hook t) 
    AfterAll {title = t, ahElms = e} -> e >>= queryElm title' (grpAddress' RC.Hook t) 
    AfterEach {title' = t, ahElms' = e} -> e >>= queryElm title' (grpAddress' RC.Hook t) 


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

data CheckInfo = CheckInfo {
  header :: Text,
  expectation :: C.ResultExpectation
} deriving Show

data ItemInfo = ItemInfo {
  id :: Int,
  title :: Text,
  checks :: [CheckInfo]
} deriving Show

data TestInfo tc = TestInfo {
  title :: Text,
  config :: tc,
  itemInfo :: [ItemInfo]
} deriving Show

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
      itemInfo = iinfo <$> items t rc
    }

data Test e tc rc hi i as ds effs = Test
  { config :: tc,
    items :: rc -> [i],
    interactor :: rc -> hi -> i -> Sem effs as,
    parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
  }