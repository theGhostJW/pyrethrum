{-# LANGUAGE UndecidableInstances #-}

module Core where

import DSL.Internal.ApEvent
import Data.Aeson (FromJSON, ToJSON, Value (..), parseJSON, toJSON)

import Data.Aeson.Types (ToJSON (..))
import qualified Data.DList as DL
import Effectful (Eff)
import qualified Effectful.Error.Dynamic as E
import Effectful.Internal.Effect ((:>))
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

-- polymorphic hooks or fixtures
withOnceHook
withThreadHook

hookesAction

data Hook = Hook
  {
    hookId:: Text,
    hookDescription :: Text,
    hookType :: HookType,
    hook :: Eff '[E.Error ParseException] a
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

data PyrethrumTest rc tc effs where
  Test ::
    { config :: tc
    , items :: rc -> [i]
    , interactor :: rc -> i -> Eff effs as
    , parse :: as -> Eff '[E.Error ParseException] ds
    } ->
    PyrethrumTest rc tc effs
  TestNoParse ::
    { config :: tc
    , items :: rc -> [i]
    , interactor :: rc -> i -> Eff effs ds
    } ->
    PyrethrumTest rc tc effs

-- type PreNodeRoot = PreNode () ()

-- data Test'' si ti ii = Test''
--   { id :: Text
--   , test :: si -> ti -> ii -> Eff effs ()
--   }

data Fixture effs oi ti tsti where
  Fixture ::
    { title :: Text
    , maxThreads :: Maybe Int
    , onceHook :: OnceHook effs oi oo
    , threadHook :: ThreadHook oo ti to
    , testHook :: TestHook oo to tsti tsto
    -- , tests :: NonEmpty (Test oo to tsto)
    } ->
    Fixture effs oi ti tsti

-- data HookOut o where
--   HookOut ::
--     { title :: Text
--     , value :: o
--     } ->
--     HookOut o

-- data Hook effs o where
--   Hook ::
--     { title :: Text
--     , action :: Eff effs o
--     } ->
--     Hook effs o
--   deriving (Functor)

-- runHook :: Hook effs o -> Eff effs (HookOut o)
-- runHook Hook{title, action} = HookOut title <$> action

-- mkHook :: Text -> Hook effs i -> (i -> Eff effs o) -> Hook effs o
-- mkHook title parentHook f =
--   withHook parentHook f $ Hook title
   
-- withHook :: Hook effs i -> (i -> Eff effs o) -> (Eff effs o -> a) -> a
-- withHook Hook{action} transformer constructor =
--   constructor $ action >>= transformer

-- data PreNode oi ti where
--   Group ::
--     { title :: Text
--     , threadLimit :: Maybe Int
--     , onceHook :: OnceHook oi oo
--     , threadHook :: ThreadHook oo ti to
--     , subNodes :: NonEmpty (PreNode oo to)
--     } ->
--     PreNode oi ti
--   Fixtures ::
--     { title :: Text
--     , threadLimit :: Maybe Int
--     , testHook :: TestHook oi ti () tsto
--     , fixtures :: NonEmpty (Fixture oi ti tsto)
--     } ->
--     PreNode oi ti

data OnceHook effs oi oo where
  OnceNone :: OnceHook effs oi oi
  OnceBefore ::
    { hook :: oi -> Eff effs oo
    } ->
    OnceHook effs oi oo
  OnceAfter ::
    { releaseOnly :: oi -> Eff effs ()
    } ->
    OnceHook effs oi oi
  OnceAround ::
    { hook :: oi -> Eff effs oo
    , release :: oo -> Eff effs ()
    } ->
    OnceHook effs oi oo

data ThreadHook oi ti to where
  ThreadNone :: ThreadHook oi ti ti
  ThreadBefore ::
    { hook :: oi -> ti -> Eff effs to
    } ->
    ThreadHook oi ti to
  ThreadAfter ::
    { releaseOnly :: ti -> Eff effs ()
    } ->
    ThreadHook oi ti ti
  ThreadAround ::
    { hook :: oi -> ti -> Eff effs to
    , release :: to -> Eff effs ()
    } ->
    ThreadHook oi ti to

data TestHook oi ti tsti tsto where
  TestNone :: TestHook oi ti tsti tsti
  TestBefore ::
    { hook :: oi -> ti -> tsti -> Eff effs tsto
    } ->
    TestHook oi ti tsti tsto
  TestAfter ::
    { releaseOnly :: tsti -> Eff effs ()
    } ->
    TestHook oi ti tsti tsti
  TestAround ::
    { hook :: oi -> ti -> tsti -> Eff effs tsto
    , release :: tsto -> Eff effs ()
    } ->
    TestHook oi ti tsti tsto

-- mkTest r context = Test r (items r) (interactor r) (parse r)
