{-# LANGUAGE UndecidableInstances #-}

module Core where

import DSL.Internal.ApEvent hiding (Check)
import Data.Aeson (FromJSON, ToJSON (..), Value (..), parseJSON, toJSON)

import qualified Data.DList as DL
import Effectful (Eff, Effect)
import qualified Effectful.Error.Dynamic as E
import Effectful.Internal.Effect ((:>))
import Effectful.TH (makeEffect)
import GHC.Records (HasField)
import GHC.Show (Show (..))
import PyrethrumExtras (toS, uu)

newtype CheckFailure = CheckFailure Text
  deriving (Show)

newtype ParseException = ParseException Text
  deriving (Show)

instance Exception ParseException

type HasTitle a = HasField "title" a Text
type HasMaxThreads a = HasField "maxThreads" a Int

type HasId a = HasField "id" a Int

class (HasTitle a, Show a, HasMaxThreads a, FromJSON a, ToJSON a, Eq a) => Config a

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

class OnceParam a
class ThreadParam a
class EachParam a

class OnceAfterParam a
class ThreadAfterParam a
class EachAfterParam a

---

data OnceBefore
instance OnceParam OnceBefore
instance ThreadParam OnceBefore
instance EachParam OnceBefore

data ThreadBefore
instance ThreadParam ThreadBefore
instance EachParam ThreadBefore

data EachBefore
instance EachParam EachBefore

---
-- after hooks simply pass data through to the suite elements that depend on them

data OnceAfter
instance OnceAfterParam OnceAfter
instance ThreadAfterParam OnceAfter
instance EachAfterParam OnceAfter

data ThreadAfter
instance OnceAfterParam ThreadAfter
instance ThreadAfterParam ThreadAfter

data EachAfter
instance EachAfterParam EachAfter

---
-- resource ooks can be params for before or after hooks

data OnceResource
instance OnceParam OnceResource
instance ThreadParam OnceResource
instance EachParam OnceResource
instance OnceAfterParam OnceResource
instance ThreadAfterParam OnceResource
instance EachAfterParam OnceResource

data ThreadResource
instance ThreadParam ThreadResource
instance EachParam ThreadResource
instance OnceAfterParam ThreadResource
instance ThreadAfterParam ThreadResource

data EachResource
instance EachParam EachResource
instance EachAfterParam EachResource

newtype StubLoc = StubLoc Text
data Addressed a = Addressed
  { loc :: StubLoc
  , value :: a
  }

-- data Suite effs :: Effect where
--   OnceBefore :: Eff effs a -> Suite effs m (HookResult OnceBefore a)
--   OnceBefore' :: Eff effs (HookResult OnceBefore a) -> (a -> Eff effs b) -> Suite effs m (HookResult OnceBefore b)

-- TODO: error messages when hooks are wrong

data Hook rc tc effs loc i o where
  -- once hooks
  OnceBefore ::
    { onceAction :: rc -> Eff effs o
    } ->
    Hook rc tc effs OnceBefore () o
  OnceBefore' ::
    -- forall rc tc effs loc i o.
    (OnceParam loc) =>
    { onceParent :: Hook rc tc effs loc pi i
    , onceAction' :: i -> rc -> Eff effs o
    } ->
    Hook rc tc effs OnceBefore i o
  OnceAfter' ::
    (OnceAfterParam loc) =>
    { onceAfterParent :: Hook rc tc effs loc pi i
    , onceAfter' :: rc -> Eff effs ()
    } ->
    Hook rc tc effs OnceAfter i i
  OnceAfter ::
    { onceAfter :: rc -> Eff effs ()
    } ->
    Hook rc tc effs OnceAfter () ()
  OnceResource ::
    { onceSetup :: rc -> Eff effs o
    , onceTearDown :: o -> Eff effs ()
    } ->
    Hook rc tc effs OnceResource () o
  OnceResource' ::
    -- forall rc tc effs loc a b.
    (OnceParam loc) =>
    { onceResourceParent :: Hook rc tc effs loc pi i
    , onceSetup' :: i -> rc -> Eff effs o
    , onceTearDown' :: o -> Eff effs ()
    } ->
    Hook rc tc effs OnceResource i o
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: rc -> Eff effs o
    } ->
    Hook rc tc effs ThreadBefore () o
  ThreadBefore' ::
    -- forall rc tc effs loc a b.
    (ThreadParam loc) =>
    { threadParent :: Hook rc tc effs loc pi i
    , threadAction' :: i -> rc -> Eff effs o
    } ->
    Hook rc tc effs ThreadBefore i o
  ThreadAfter ::
    { threadAfter :: rc -> Eff effs ()
    } ->
    Hook rc tc effs ThreadAfter () ()
  ThreadAfter' ::
    (ThreadAfterParam loc) =>
    { 
      threadAfterParent :: Hook rc tc effs loc pi i,
      threadAfter' :: rc -> Eff effs ()
    } ->
    Hook rc tc effs ThreadAfter i i
  ThreadResource ::
    { threadSetup :: rc -> Eff effs o
    , threadTearDown :: a -> Eff effs ()
    } ->
    Hook rc tc effs ThreadResource () o
  ThreadResource' ::
    -- forall rc tc effs loc a b.
    (ThreadParam loc) =>
    { threadResourceParent :: Hook rc tc effs loc pi i
    , threadSetup' :: i -> rc -> Eff effs o
    , threadTearDown' :: o -> Eff effs ()
    } ->
    Hook rc tc effs ThreadResource i o
  -- each hooks
  EachBefore ::
    { eachAction :: rc -> Eff effs o
    } ->
    Hook rc tc effs EachBefore () o
  EachBefore' ::
    -- forall rc tc effs loc a b.
    (EachParam loc) =>
    { eachParent :: Hook rc tc effs loc pi i
    , eachAction' :: i -> rc -> Eff effs o
    } ->
    Hook rc tc effs EachBefore i o
  EachAfter ::
    { eachAfter :: rc -> Eff effs ()
    } ->
    Hook rc tc effs EachAfter () ()
  EachAfter' ::
    (EachAfterParam loc) =>
    { 
      eachAfterParent :: Hook rc tc effs loc pi i,
      eachAfter' :: rc -> Eff effs ()
    } ->
    Hook rc tc effs EachAfter i i
  EachResource ::
    { eachSetup :: rc -> Eff effs o
    , eachTearDown :: o -> Eff effs ()
    } ->
    Hook rc tc effs EachResource () o
  EachResource' ::
    -- forall rc tc effs loc a b.
    (EachParam loc) =>
    { eachResourceParent :: Hook rc tc effs loc pi i
    , eachSetup' :: i -> rc -> Eff effs o
    , eachTearDown' :: o -> Eff effs ()
    } ->
    Hook rc tc effs EachResource i o

data Test rc tc effs hi where
  Full ::
    (ItemClass i ds) =>
    { config :: tc
    , action :: rc -> i -> Eff effs as
    , parse :: as -> Either ParseException ds
    , items :: rc -> [i]
    } ->
    Test rc tc effs ()
  Full' ::
    (ItemClass i ds, EachParam loc) =>
    { parentHook :: Hook rc tc effs loc pi hi
    , config' :: tc
    , childAction :: hi -> rc -> i -> Eff effs as
    , parse' :: as -> Either ParseException ds
    , items' :: rc -> [i]
    } ->
    Test rc tc effs hi
  NoParse ::
    (ItemClass i ds) =>
    { config :: tc
    , action :: rc -> i -> Eff effs ds
    , items :: rc -> [i]
    } ->
    Test rc tc effs ()
  NoParse' ::
    (ItemClass i ds, EachParam loc) =>
    { parentHook :: Hook rc tc effs loc pi hi
    , config' :: tc
    , childAction :: hi -> rc -> i -> Eff effs ds
    , items' :: rc -> [i]
    } ->
    Test rc tc effs hi
  Single ::
    { config :: tc
    , singleAction :: rc -> Eff effs ds
    , checks :: Checks ds
    } ->
    Test rc tc effs ()
  Single' ::
    (EachParam loc) =>
    { parentHook :: Hook rc tc effs loc pi hi
    , config' :: tc
    , childSingleAction :: hi -> rc -> Eff effs ds
    , checks' :: Checks ds
    } ->
    Test rc tc effs hi

data Path = Path
  { module' :: Text
  , title :: Text
  }

type Suite rc tc effs = [SuiteElement rc tc effs ()]

data SuiteElement rc tc effs i where
  Hook ::
    { path :: Path
    , hook :: Hook rc tc effs loc i o
    , subNodes :: [SuiteElement rc tc effs o]
    } ->
    SuiteElement rc tc effs i
  Test ::
    { path :: Path
    , test :: Test rc tc effs i
    } ->
    SuiteElement rc tc effs i

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