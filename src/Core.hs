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


data Once
instance OnceParam Once
instance ThreadParam Once
instance EachParam Once

data Thread
instance ThreadParam Thread
instance EachParam Thread

data Each
instance EachParam Each


data Hook rc tc effs loc i o where
  -- once hooks
  OnceBefore ::
    { onceAction :: rc -> Eff effs o
    } ->
    Hook rc tc effs Once () o
  OnceBefore' ::
    -- forall rc tc effs loc i o.
    (OnceParam loc) =>
    { onceParent :: Hook rc tc effs loc pi i
    , onceAction' :: i -> rc -> Eff effs o
    } ->
    Hook rc tc effs Once i o
  OnceAfter' ::
    (OnceParam loc) =>
    { onceAfterParent :: Hook rc tc effs loc pi i
    , onceAfter' :: rc -> Eff effs ()
    } ->
    Hook rc tc effs Once i i
  OnceAfter ::
    { onceAfter :: rc -> Eff effs ()
    } ->
    Hook rc tc effs Once () ()
  OnceAround ::
    { onceSetup :: rc -> Eff effs o
    , onceTearDown :: o -> Eff effs ()
    } ->
    Hook rc tc effs Once () o
  OnceAround' ::
    -- forall rc tc effs loc a b.
    (OnceParam loc) =>
    { onceAroundParent :: Hook rc tc effs loc pi i
    , onceSetup' :: i -> rc -> Eff effs o
    , onceTearDown' :: o -> Eff effs ()
    } ->
    Hook rc tc effs Once i o
  -- once per thread hooks
  ThreadBefore ::
    { threadAction :: rc -> Eff effs o
    } ->
    Hook rc tc effs Thread () o
  ThreadBefore' ::
    -- forall rc tc effs loc a b.
    (ThreadParam loc) =>
    { threadParent :: Hook rc tc effs loc pi i
    , threadAction' :: i -> rc -> Eff effs o
    } ->
    Hook rc tc effs Thread i o
  ThreadAfter ::
    { threadAfter :: rc -> Eff effs ()
    } ->
    Hook rc tc effs Thread () ()
  ThreadAfter' ::
    (ThreadParam loc) =>
    { 
      threadAfterParent :: Hook rc tc effs loc pi i,
      threadAfter' :: rc -> Eff effs ()
    } ->
    Hook rc tc effs Thread i i
  ThreadAround ::
    { threadSetup :: rc -> Eff effs o
    , threadTearDown :: a -> Eff effs ()
    } ->
    Hook rc tc effs Thread () o
  ThreadAround' ::
    -- forall rc tc effs loc a b.
    (ThreadParam loc) =>
    { threadAroundParent :: Hook rc tc effs loc pi i
    , threadSetup' :: i -> rc -> Eff effs o
    , threadTearDown' :: o -> Eff effs ()
    } ->
    Hook rc tc effs Thread i o
  -- each hooks
  EachBefore ::
    { eachAction :: rc -> Eff effs o
    } ->
    Hook rc tc effs Each () o
  EachBefore' ::
    -- forall rc tc effs loc a b.
    (EachParam loc) =>
    { eachParent :: Hook rc tc effs loc pi i
    , eachAction' :: i -> rc -> Eff effs o
    } ->
    Hook rc tc effs Each i o
  EachAfter ::
    { eachAfter :: rc -> Eff effs ()
    } ->
    Hook rc tc effs Each () ()
  EachAfter' ::
    (EachParam loc) =>
    { 
      eachAfterParent :: Hook rc tc effs loc pi i,
      eachAfter' :: rc -> Eff effs ()
    } ->
    Hook rc tc effs Each i i
  EachAround ::
    { eachSetup :: rc -> Eff effs o
    , eachTearDown :: o -> Eff effs ()
    } ->
    Hook rc tc effs Each () o
  EachAround' ::
    -- forall rc tc effs loc a b.
    (EachParam loc) =>
    { eachAroundParent :: Hook rc tc effs loc pi i
    , eachSetup' :: i -> rc -> Eff effs o
    , eachTearDown' :: o -> Eff effs ()
    } ->
    Hook rc tc effs Each i o



newtype StubLoc = StubLoc Text
data Addressed a = Addressed
  { loc :: StubLoc
  , value :: a
  }
  
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
    { parent :: Hook rc tc effs loc pi hi
    , config' :: tc
    , action' :: hi -> rc -> i -> Eff effs as
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
    { parent :: Hook rc tc effs loc pi hi
    , config' :: tc
    , action' :: hi -> rc -> i -> Eff effs ds
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
    { parent :: Hook rc tc effs loc pi hi
    , config' :: tc
    , singleAction' :: hi -> rc -> Eff effs ds
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