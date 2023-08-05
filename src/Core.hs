{-# LANGUAGE UndecidableInstances #-}

module Core where

import DSL.Internal.ApEvent
import Data.Aeson (FromJSON, ToJSON, Value (..), parseJSON, toJSON)

import Data.Aeson.Types (ToJSON (..))
import qualified Data.DList as DL
import Effectful (Eff, Effect)
import qualified Effectful.Error.Dynamic as E
import Effectful.Internal.Effect ((:>))
import Effectful.TH (makeEffect)
import GHC.Records (HasField)
import GHC.Show (Show (..))
import qualified Internal.PreNode as PN
import PyrethrumExtras (toS, uu)

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

data Check ds
  = Check
      { header :: Text
      , rule :: ds -> Bool
      }
  | CheckMessage
      { header :: Text
      , message :: ds -> Text
      , rule :: ds -> Bool
      }

-- generate a check from a predicate
chk :: Text -> (ds -> Bool) -> Checks ds
chk hdr = Checks . pure . Check hdr

-- generate a check from a predicate with detailed message
chk' :: Text -> (ds -> Text) -> (ds -> Bool) -> Checks ds
chk' hdr msg = Checks . pure . CheckMessage hdr msg

instance Show (Check v) where
  show :: Check v -> String
  show ck = toS ck.header

instance ToJSON (Check v) where
  toJSON :: Check v -> Value
  toJSON = String . toS . (.header)

--

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

data Test
instance AfterTest Test

data OnceAfter
instance AfterTest OnceAfter

data ThreadAfter
instance AfterTest ThreadAfter

data EachBefore
instance BeforeTest EachBefore

data EachAfter
instance AfterTest EachAfter

newtype StubLoc = StubLoc Text
data Addressed a = Addressed
  { loc :: StubLoc
  , value :: a
  }

-- data Suite effs :: Effect where
--   OnceBefore :: Eff effs a -> Suite effs m (HookResult OnceBefore a)
--   OnceBefore' :: Eff effs (HookResult OnceBefore a) -> (a -> Eff effs b) -> Suite effs m (HookResult OnceBefore b)

-- TODO: error messages when hooks are wrong

data AbstractFixture rc tc effs loc a where
  -- once hooks
  OnceBefore ::
    { onceAction :: rc -> Eff effs a
    } ->
    AbstractFixture rc tc effs OnceBefore a
  OnceBefore' ::
    { onceParent :: AbstractFixture rc tc effs OnceBefore a
    , onceChildAction :: rc -> a -> Eff effs b
    } ->
    AbstractFixture rc tc effs OnceBefore b
  OnceAfter ::
    { onceBefore :: (AfterTest loc) => AbstractFixture rc tc effs loc ()
    , onceAfterAction :: rc -> Eff effs ()
    } ->
    AbstractFixture rc tc effs OnceAfter ()
  OnceResource ::
    { onceSetup :: rc -> Eff effs a
    , onceTearDown :: a -> Eff effs ()
    } ->
    AbstractFixture rc tc effs OnceBefore ()
  OnceResource' ::
    { onceResourceParent :: AbstractFixture rc tc effs OnceBefore a
    , onceChildSetup :: a -> rc -> Eff effs b
    , onceChildTearDown :: b -> Eff effs ()
    } ->
    AbstractFixture rc tc effs OnceBefore a
  -- once per thread hooks
  ThreadBefore ::
    { action :: rc -> Eff effs a
    } ->
    AbstractFixture rc tc effs ThreadBefore a
  ThreadBefore' ::
    { parent :: (ThreadParam loc, BeforeTest loc) => AbstractFixture rc tc effs loc a
    , childAction :: rc -> a -> Eff effs b
    } ->
    AbstractFixture rc tc effs ThreadBefore b
  ThreadAfter ::
    { threadBefore :: (AfterTest loc) => AbstractFixture rc tc effs loc ()
    , threadAfterAction :: rc -> Eff effs ()
    } ->
    AbstractFixture rc tc effs ThreadAfter ()
  ThreadResource ::
    { threadSetup :: rc -> Eff effs a
    , threadTearDown :: a -> Eff effs ()
    } ->
    AbstractFixture rc tc effs ThreadBefore ()
  ThreadResource' ::
    { threadResourceParent :: AbstractFixture rc tc effs OnceBefore a
    , threadChildSetup :: a -> rc -> Eff effs b
    , threadChildTearDown :: b -> Eff effs ()
    } ->
    AbstractFixture rc tc effs ThreadBefore a
  -- each hooks
  EachBefore ::
    { eachAction :: rc -> Eff effs a
    } ->
    AbstractFixture rc tc effs EachBefore a
  EachBefore' ::
    { eachParent :: (BeforeTest loc) => AbstractFixture rc tc effs loc a
    , eachChildAction :: rc -> a -> Eff effs b
    } ->
    AbstractFixture rc tc effs EachBefore b
  EachAfter ::
    { eachBefore :: (AfterTest loc) => AbstractFixture rc tc effs loc ()
    , eachAfterAction :: rc -> Eff effs ()
    } ->
    AbstractFixture rc tc effs EachAfter ()
  EachResource ::
    { eachSetup :: rc -> Eff effs a
    , eachTearDown :: a -> Eff effs ()
    } ->
    AbstractFixture rc tc effs EachBefore ()
  EachResource' ::
    { eachResourceParent :: (BeforeTest loc) => AbstractFixture rc tc effs loc a
    , eachChildSetup :: a -> rc -> Eff effs b
    , eachChildTearDown :: b -> Eff effs ()
    } ->
    AbstractFixture rc tc effs EachBefore a
  -- test
  Test :: {test :: AbstractTest rc tc effs} -> AbstractFixture rc tc effs Test ()

data AbstractTest rc tc effs where
  Full ::
    (ItemClass i ds) =>
    { config :: tc
    , action :: rc -> i -> Eff effs as
    , parse :: as -> Either ParseException ds
    , items :: rc -> [i]
    } ->
    AbstractTest rc tc effs
  Full' ::
    (ItemClass i ds) =>
    { parentHook :: (BeforeTest loc) => AbstractFixture rc tc effs loc a
    , config :: tc
    , childAction :: a -> rc -> i -> Eff effs as
    , parse :: as -> Either ParseException ds
    , items :: rc -> [i]
    } ->
    AbstractTest rc tc effs
  NoParse ::
    (ItemClass i ds) =>
    { config :: tc
    , action :: rc -> i -> Eff effs ds
    , items :: rc -> [i]
    } ->
    AbstractTest rc tc effs
  NoParse' ::
    (ItemClass i ds) =>
    { parentHook :: (BeforeTest loc) => AbstractFixture rc tc effs loc a
    , config :: tc
    , childAction :: a -> rc -> i -> Eff effs ds
    , items :: rc -> [i]
    } ->
    AbstractTest rc tc effs
  Single ::
    { config :: tc
    , singleAction :: rc -> Eff effs ds
    , checks :: Checks ds
    } ->
    AbstractTest rc tc effs
  Single' ::
    (ItemClass i ds) =>
    { parentHook :: (BeforeTest loc) => AbstractFixture rc tc effs loc a
    , config :: tc
    , childSingleAction :: a -> rc -> Eff effs ds
    , checks :: Checks ds
    } ->
    AbstractTest rc tc effs

-- try this
-- part 1
--  - do notation :: NA
-- - interpretor + writer :: NA
-- - extract loc from item (hard code for now) + fixtureType and dependency  loc
-- - stub for checks (see part 4)
-- - add missing fixtures
-- - reinstate before
-- - hook around
-- - fixture hooks
-- - instance hooks
-- - check constraints line up
-- change prenode and execution to match new structure
-- - need 2 files
--  - fixture list - plain old do for initial extraction -- hard code for now
--  - test Suite - the actual test suite with interperetor as param
--   - error as warning extension added to the top of both generated files
-- stub for run test
-- - generate suite based on prenode
-- part 2
-- reinstrate run test
-- demo running single test
-- part 3
--  - unit tests for suite runtime
-- part 4
-- - implement check for recursion check
-- - generator (hie or below)
-- remove stubLoc from type
-- see weeder :: HIE
-- start with:: https://github.com/theGhostJW/pyrethrum-extras/blob/master/src/Language/Haskell/TH/Syntax/Extended.hs
-- see also:: https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/Language-Haskell-TH-Syntax.html#t:Name
-- part 5 reinstate filtering // tree shaking

{-

module Language.Haskell.TH.Syntax.Extended (
 module S,
 moduleOf
)
 where

import           BasePrelude as B
import           Data.Text hiding (reverse, dropWhile)
import           Language.Haskell.TH.Syntax as S
import           Stringy

-- https://stackoverflow.com/a/5679470/5589037
moduleOf :: S.Name -> Text
moduleOf =
  let
    dropLastToken :: String -> String
    dropLastToken = reverse . dropWhile (== '.') . dropWhile (/= '.') . reverse
  in
    toS . dropLastToken . show

mkTestAddress :: Name -> TestAddress
mkTestAddress = TestAddress . moduleOf

nameOfModule :: TestAddress
nameOfModule = mkTestAddress ''ApState

  -}
-- - create concrete object Foo vs AbstractFoo

-- expose parent in return type in hook
-- have a root element
-- list of tests
-- copy types??
-- reddit
-- work backward to root
-- build from root

-- Rename constraints
-- Test
-- Instance
-- Around
-- see --  HERE!!!!!!! - implementt stub esp checks
-- sub effs -- how does it work
-- document lifted functions
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
