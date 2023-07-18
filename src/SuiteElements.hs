module SuiteElements where

import DSL.Internal.ApEvent
import Effectful (Eff)
import qualified Effectful.Error.Dynamic as E
import Effectful.Internal.Effect ((:>))
import qualified Internal.PreNode as PN

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
data Test tc rc hd effs where
  Test ::
    { config :: tc
    , items :: rc -> [i]
    , interactor :: rc -> hd -> i -> Eff effs as
    , parse :: as -> Eff '[E.Error ParseException] ds
    } ->
    Test tc rc hd effs

type PreNodeRoot = PreNode () ()

data Test'' si ti ii = Test''
  { id :: Text
  , test :: si -> ti -> ii -> IO ()
  }

data Fixture oi ti tsti where
  Fixture ::
    { title :: Text
    , maxThreads :: Maybe Int
    , onceHook :: OnceHook oi oo
    , threadHook :: ThreadHook oo ti to
    , testHook :: TestHook oo to tsti tsto
    -- , tests :: NonEmpty (Test oo to tsto)
    } ->
    Fixture oi ti tsti

data PreNode oi ti where
  Group ::
    { title :: Text
    , threadLimit :: Maybe Int
    , onceHook :: OnceHook oi oo
    , threadHook :: ThreadHook oo ti to
    , subNodes :: NonEmpty (PreNode oo to)
    } ->
    PreNode oi ti
  Fixtures ::
    { title :: Text
    , threadLimit :: Maybe Int
    , testHook :: TestHook oi ti () tsto
    , fixtures :: NonEmpty (Fixture oi ti tsto)
    } ->
    PreNode oi ti

data OnceHook oi oo where
  OnceNone :: OnceHook oi oi
  OnceBefore ::
    { hook :: oi -> IO oo
    } ->
    OnceHook oi oo
  OnceAfter ::
    { releaseOnly :: oi -> IO ()
    } ->
    OnceHook oi oi
  OnceAround ::
    { hook :: oi -> IO oo
    , release :: oo -> IO ()
    } ->
    OnceHook oi oo

data ThreadHook oi ti to where
  ThreadNone :: ThreadHook oi ti ti
  ThreadBefore ::
    { hook :: oi -> ti -> IO to
    } ->
    ThreadHook oi ti to
  ThreadAfter ::
    { releaseOnly :: ti -> IO ()
    } ->
    ThreadHook oi ti ti
  ThreadAround ::
    { hook :: oi -> ti -> IO to
    , release :: to -> IO ()
    } ->
    ThreadHook oi ti to

data TestHook oi ti tsti tsto where
  TestNone :: TestHook oi ti tsti tsti
  TestBefore ::
    { hook :: oi -> ti -> tsti -> IO tsto
    } ->
    TestHook oi ti tsti tsto
  TestAfter ::
    { releaseOnly :: tsti -> IO ()
    } ->
    TestHook oi ti tsti tsti
  TestAround ::
    { hook :: oi -> ti -> tsti -> IO tsto
    , release :: tsto -> IO ()
    } ->
    TestHook oi ti tsti tsto

mkTest r context = Test r (items r) (interactor r) (parse r)
