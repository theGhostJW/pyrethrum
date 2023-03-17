module RunnerBase
  ( AddressedElm (..),
    ItemRunner,
    SuiteSource,
    RBL.TestSuite (..),
    RBL.SuiteItem (..),
    Test (..),
    TestInfo (..),
    GenericResult (..),
    queryElm,
    querySuite',
    querySuite,
    nullItems,
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
import RunElementClasses (Address, AddressElemType, AddressedElm (..), Config, TestFilterResult (TestFilterResult), push, rootAddress)
import qualified RunElementClasses as RC
import PyrethrumExtras hiding (bracket)
import Text.Extra as T (length)

type ItemRunner e as ds i hd tc rc effs =
  rc -> Address -> hd -> Test e tc rc hd i as ds effs -> i -> Sem effs ()


-- (address -> hookData -> test -> a) -> TestSuite () effs a
type SuiteSource e tc rc effs a =
  ( forall hd i as ds.
    (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, HasField "checks" i (C.Checks ds), HasField "id" i Int, HasField "title" i Text) =>
    Address ->
    hd ->
    Test e tc rc hd i as ds effs ->
    a
  ) ->
  TestSuite () effs a

data GenericResult tc rslt = TestResult
  { configuration :: tc,
    results :: Either FilterErrorType [rslt]
  }
  deriving (Show)

queryElm :: forall hi effs a. (a -> Text) -> Address -> SuiteItem hi effs a -> [AddressedElm a]
queryElm title' address =
  let hdUndefined = uu

      tstAddress :: a -> Address
      tstAddress a = push (title' a) RC.Test address

      grpAddress' :: AddressElemType -> Text -> Address
      grpAddress' et ttl = push ttl et address

      hkQuery' :: forall hii. AddressElemType -> Text -> [SuiteItem hii effs a] -> [AddressedElm a]
      hkQuery' et t e = e >>= queryElm title' (grpAddress' et t)

      hkQuery :: forall hii. Text -> [SuiteItem hii effs a] -> [AddressedElm a]
      hkQuery = hkQuery' RC.Hook
   in \case
        Tests {tests} -> (\a -> AddressedElm (tstAddress a) a) <$> tests address hdUndefined
        Group {title = t, gElms = e} -> hkQuery' RC.Group t e
        OnceHook {title = t, hkElms = e} -> hkQuery t e

querySuiteElms :: forall hi effs a. (a -> Text) -> Address -> TestSuite hi effs a -> [AddressedElm a]
querySuiteElms title' address suite = suite.un >>= queryElm title' address

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
  SuiteSource e tc rc effs a -> -- suiite
  [AddressedElm a]
querySuite' rc title' extractor suite =
  let fullQuery ::
        (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, RC.ItemClass i ds) =>
        Address ->
        hd ->
        Test e tc rc hd i as ds effs ->
        a
      fullQuery a hd = extractor rc a

      root :: TestSuite () effs a
      root = suite fullQuery
   in querySuiteElms title' rootAddress root

querySuite :: forall rc e tc effs. Config tc => rc -> SuiteSource e tc rc effs (TestInfo tc) -> [AddressedElm (TestInfo tc)]
querySuite rc suite =
  let extractor :: forall ho i as ds. RC.ItemClass i ds => rc -> Address -> Test e tc rc ho i as ds effs -> TestInfo tc
      extractor rc' _add = testInfo rc'

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
      ckInfo C.Check {header, expectation}= CheckInfo header expectation

      iinfo :: i -> ItemInfo
      iinfo i =
        ItemInfo
          { id = getField @"id" i,
            title = getField @"title" i,
            checks = ckInfo <$> toList i.checks.un
          }

      cfg :: tc
      cfg = t.config
   in TestInfo
        { title = cfg.title,
          config = cfg,
          itemInfo = iinfo <$> t.items rc
        }

data Test e tc rc hd i as ds effs = Test
  { 
    -- Depricate - See test GADIT BELOW 
    config :: tc,
    items :: rc -> [i],
    interactor :: rc -> hd -> i -> Sem effs as,
    parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
  }

data TestGADIT e tc rc hd effs where 
  TestGADT ::
   { 
     -- Replace all with this
    config :: tc,
    items :: rc -> [i],
    interactor :: rc -> hd -> i -> Sem effs as,
    parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
   } -> TestGADIT e tc rc hd effs 

nullItems :: Test e tc rc hd i as ds effs -> rc -> Bool
nullItems t rc = null $ t.items rc



{-
The following demos that before and after could be implemented  via a bracket / resource like function
and partially applied / shared. This seems to be a much simplerer approach than trying to impement these
hooks via a SuiteItem data constructors which would end up haeing pretty arbitary semantics for the end user

defer as needs to be written in terms of resource
-}

demoBeforeAll:: hd -> Sem effs Text
demoBeforeAll hd = pure "Hello"

demoAfterAll:: Text -> Sem effs ()
demoAfterAll t = pure ()

--  bracket for 
bracket :: Sem effs a   -- computation to run first ("acquire resource")
          -> (a -> Sem effs ()) -- computation to run last ("release resource")
          -> (a -> Sem effs c)  -- computation to run in-between
          -> Sem effs c
bracket = uu

hookBa :: hd -> Sem effs b
hookBa = uu

demoInteractor1 :: rc -> hd -> i -> Sem effs Int
demoInteractor1 rc hd i =
    bracket
      (demoBeforeAll hd)
      (\_ -> pure ())
      (pure . T.length {- can refer to: rc, hd, i  here-})

-- helpers 
-- withBeforeAfter 
-- withBefore
-- withAfter

