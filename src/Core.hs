{-# LANGUAGE UndecidableInstances #-}

module Core where

import DSL.Internal.ApEvent
import Data.Aeson (FromJSON, ToJSON, Value (..), parseJSON, toJSON)

import Data.Aeson.Types (ToJSON (..))
import qualified Data.DList as DL
import Effectful (Eff, Effect)
import qualified Effectful.Error.Dynamic as E
import Effectful.Internal.Effect ((:>))
import GHC.Records (HasField)
import GHC.Show (Show (..))
import qualified Internal.PreNode as PN
import PyrethrumExtras (toS, uu)
import Effectful.TH (makeEffect)

newtype CheckFailure = CheckFailure Text
  deriving (Show)

newtype ParseException = ParseException Text
  deriving (Show)

instance Exception ParseException

{-

-- polymorphic Suites or fixtures
withOnceSuite
withThreadSuite

SuiteesAction

data Suite = Suite
  {
    SuiteId:: Text,
    SuiteDescription :: Text,
    SuiteType :: SuiteType,
    Suite :: Eff '[E.Error ParseException] a
  }

  -}
-- data

type HasTitle a = HasField "title" a Text

type HasId a = HasField "id" a Int

class (HasField "title" a Text, Show a, FromJSON a, ToJSON a, Eq a) => Config a

type ItemClass i ds = (HasTitle i, HasId i, HasField "checks" i (Checks ds))

newtype Checks ds = Checks
  { un :: DL.DList (Check ds)
  }
  deriving (Show, Semigroup, Monoid, IsList)

-- TODO:: look into listLike

map :: (Check ds -> Check ds2) -> Checks ds -> Checks ds2
map f = Checks . DL.map f . (.un)

data Check ds = Check
  { header :: Text
  , rule :: ds -> Eff '[E.Error CheckFailure] ()
  }

instance Show (Check v) where
  show :: Check v -> String
  show ck@Check{header} = toS header

instance ToJSON (Check v) where
  toJSON :: Check v -> Value
  toJSON = String . toS . (.header)

-- concurrency
data Once
data Thread
data Each

-- order
data Before
data After

class BeforeTest a
class OnceParam a
class ThreadParam a
class AfterTest a

data OnceBefore

instance BeforeTest OnceBefore
instance OnceParam OnceBefore
instance ThreadParam OnceBefore

data ThreadBefore
instance BeforeTest ThreadBefore
instance ThreadParam ThreadBefore

newtype HookResult hookProps a = HookResult a

-- data Suite effs :: Effect where
--   OnceBefore :: Eff effs a -> Suite effs m (HookResult OnceBefore a)
--   OnceBefore' :: Eff effs (HookResult OnceBefore a) -> (a -> Eff effs b) -> Suite effs m (HookResult OnceBefore b)

-- Rename constraints
-- Instance
-- Around
-- Test
-- see --  HERE!!!!!!! - implementt stub esp checks
-- document lifted functions

data Suite rc tc effs :: Effect where
  BeforeOnce :: (rc -> Eff effs a) -> Suite rc tc effs m (HookResult OnceBefore a)
  BeforeOnceChild :: m (HookResult OnceBefore a) -> (rc -> a -> Eff effs b) -> Suite rc tc effs m (HookResult OnceBefore b)
  BeforeThread :: (rc -> Eff effs a) -> Suite rc tc effs m (HookResult ThreadBefore a)
  BeforeThreadChild :: ThreadParam hc => m (HookResult hc a) -> (rc -> a -> Eff effs b) -> Suite rc tc effs m (HookResult ThreadBefore b)
  -- Test :: ItemClass i ds => (rc -> i -> Eff effs as) -> (as -> Eff '[E.Error ParseException] ds) -> (rc -> [i]) -> Suite rc tc effs m ()
  Test ::  TestParams rc tc effs -> Suite rc tc effs m (TestParams rc tc effs)
  WithHook :: m (HookResult hc a) -> (a -> TestParams rc tc effs) -> Suite rc tc effs m (TestParams rc tc effs)


  OnceAfter :: Eff effs () -> Suite rc tc effs m ()
  -- TODO: error messages when hooks are wrong

data TestParams rc tc effs where
  Full ::
    { config :: tc
    , action :: rc -> i -> Eff effs as
    , parse :: as -> Eff '[E.Error ParseException] ds
    , items :: rc -> [i]
    } ->
    TestParams rc tc effs
  NoParse ::
    { config :: tc
    , action :: rc -> i -> Eff effs ds
    , items :: rc -> [i]
    } ->
    TestParams rc tc effs
  -- TODO Singleton

makeEffect ''Suite
-- type PreNodeRoot = PreNode () ()

-- data Test'' si ti ii = Test''
--   { id :: Text

--   }

-- data Fixture effs oi ti tsti where
--   Fixture ::
--     { title :: Text
--     , maxThreads :: Maybe Int
--     , onceSuite :: OnceSuite effs oi oo
--     , threadSuite :: ThreadSuite oo ti to
--     , testSuite :: TestSuite oo to tsti tsto
--     -- , tests :: NonEmpty (Test oo to tsto)
--     } ->
--     Fixture effs oi ti tsti

-- data SuiteOut o where
--   SuiteOut ::
--     { title :: Text
--     , value :: o
--     } ->
--     SuiteOut o

-- data Suite effs o where
--   Suite ::
--     { title :: Text
--     , action :: Eff effs o
--     } ->
--     Suite effs o
--   deriving (Functor)

-- runSuite :: Suite effs o -> Eff effs (SuiteOut o)
-- runSuite Suite{title, action} = SuiteOut title <$> action

-- mkSuite :: Text -> Suite effs i -> (i -> Eff effs o) -> Suite effs o
-- mkSuite title parentSuite f =
--   withSuite parentSuite f $ Suite title

-- withSuite :: Suite effs i -> (i -> Eff effs o) -> (Eff effs o -> a) -> a
-- withSuite Suite{action} transformer constructor =
--   constructor $ action >>= transformer

-- data PreNode oi ti where
--   Group ::
--     { title :: Text
--     , threadLimit :: Maybe Int
--     , onceSuite :: OnceSuite oi oo
--     , threadSuite :: ThreadSuite oo ti to
--     , subNodes :: NonEmpty (PreNode oo to)
--     } ->
--     PreNode oi ti
--   Fixtures ::
--     { title :: Text
--     , threadLimit :: Maybe Int
--     , testSuite :: TestSuite oi ti () tsto
--     , fixtures :: NonEmpty (Fixture oi ti tsto)
--     } ->
--     PreNode oi ti

-- data OnceSuite effs oi oo where
--   OnceNone :: OnceSuite effs oi oi
--   OnceBefore ::
--     { Suite :: oi -> Eff effs oo
--     } ->
--     OnceSuite effs oi oo
--   OnceAfter ::
--     { releaseOnly :: oi -> Eff effs ()
--     } ->
--     OnceSuite effs oi oi
--   OnceAround ::
--     { Suite :: oi -> Eff effs oo
--     , release :: oo -> Eff effs ()
--     } ->
--     OnceSuite effs oi oo

-- data ThreadSuite oi ti to where
--   ThreadNone :: ThreadSuite oi ti ti
--   ThreadBefore ::
--     { Suite :: oi -> ti -> Eff effs to
--     } ->
--     ThreadSuite oi ti to
--   ThreadAfter ::
--     { releaseOnly :: ti -> Eff effs ()
--     } ->
--     ThreadSuite oi ti ti
--   ThreadAround ::
--     { Suite :: oi -> ti -> Eff effs to
--     , release :: to -> Eff effs ()
--     } ->
--     ThreadSuite oi ti to

-- data TestSuite oi ti tsti tsto where
--   TestNone :: TestSuite oi ti tsti tsti
--   TestBefore ::
--     { Suite :: oi -> ti -> tsti -> Eff effs tsto
--     } ->
--     TestSuite oi ti tsti tsto
--   TestAfter ::
--     { releaseOnly :: tsti -> Eff effs ()
--     } ->
--     TestSuite oi ti tsti tsti
--   TestAround ::
--     { Suite :: oi -> ti -> tsti -> Eff effs tsto
--     , release :: tsto -> Eff effs ()
--     } ->
--     TestSuite oi ti tsti tsto

-- -- mkTest r context = Test r (items r) (interactor r) (parse r)
