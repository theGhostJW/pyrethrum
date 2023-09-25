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

data Frequency = Once | Thread | Each deriving (Show, Eq)

class Param a where
  frequency :: Frequency

class (Param a, Param b) => ValidParent a b

data Once
data Thread
data Each

instance Param Once where
  frequency :: Frequency
  frequency = Once

instance Param Thread where
  frequency :: Frequency
  frequency = Thread

instance Param Each where
  frequency :: Frequency
  frequency = Each

instance ValidParent Once Once
instance ValidParent Once Thread
instance ValidParent Once Each

instance ValidParent Thread Thread
instance ValidParent Thread Each

instance ValidParent Each Each

data Hook rc effs loc i o where
  Before ::
    (Param loc) =>
    { action :: rc -> Eff effs o
    } ->
    Hook rc effs loc () o
  Before' ::
    (Param ploc, Param loc, ValidParent ploc loc) =>
    { parent :: Hook rc effs ploc pi i
    , action' :: rc -> i -> Eff effs o
    } ->
    Hook rc effs loc i o
  After ::
    (Param loc) =>
    { afterAction :: rc -> Eff effs ()
    } ->
    Hook rc effs loc () ()
  After' ::
    (Param ploc, Param loc, ValidParent ploc loc) =>
    { afterParent :: Hook rc effs ploc pi i
    , afterAction' :: rc -> Eff effs ()
    } ->
    Hook rc effs loc i i
  Around ::
    (Param loc) =>
    { setup :: rc -> Eff effs o
    , teardown :: rc -> o -> Eff effs ()
    } ->
    Hook rc effs loc () o
  Around' ::
    (Param ploc, Param loc, ValidParent ploc loc) =>
    { parent :: Hook rc effs ploc pi i
    , setup' :: rc -> i -> Eff effs o
    , teardown' :: rc -> o -> Eff effs ()
    } ->
    Hook rc effs loc i o


hookFrequency :: forall rc effs loc i o. Param loc => Hook rc effs loc i o -> Frequency
hookFrequency _ = frequency @loc

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
    (ItemClass i ds) =>
    { parent :: Hook rc effs loc pi hi
    , config' :: tc
    , action' :: rc -> hi -> i -> Eff effs as
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
    (ItemClass i ds) =>
    { parent :: Hook rc effs loc pi hi
    , config' :: tc
    , action' :: rc -> hi -> i -> Eff effs ds
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
    { parent :: Hook rc effs loc pi hi
    , config' :: tc
    , singleAction' :: rc -> hi -> Eff effs ds
    , checks' :: Checks ds
    } ->
    Test rc tc effs hi

data Path = Path
  { module' :: Text
  , title :: Text
  } deriving Show

type Suite rc tc effs = [SuiteElement rc tc effs ()]

data SuiteElement rc tc effs i where
  Hook ::
    (Param loc) =>
    { path :: Path
    , hook :: Hook rc effs loc i o
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