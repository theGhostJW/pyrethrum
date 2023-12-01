{-# LANGUAGE UndecidableInstances #-}

module Core where

import DSL.Internal.ApEvent hiding (Check)
import Data.Aeson (FromJSON, ToJSON (..), Value (..), parseJSON, toJSON)
import Internal.ThreadEvent ( Frequency(..) )

import qualified Data.DList as DL
import Effectful (Eff, Effect)
import Effectful.Error.Static as E (Error)
import Effectful.Internal.Effect ((:>))
import Effectful.TH (makeEffect)
import GHC.Records (HasField)
import GHC.Show (Show (..))
import PyrethrumExtras (toS, uu)
import CheckNew (Checks)

newtype CheckFailure = CheckFailure Text
  deriving (Show)

newtype ParseException = ParseException Text
  deriving (Show)

instance Exception ParseException

type HasTitle a = HasField "title" a Text

-- TODO :: YAGNI delete this
type HasMaxThreads a = HasField "maxThreads" a Int

type HasId a = HasField "id" a Int

class (HasTitle a, Show a, ToJSON a, Eq a) => Config a

class (HasTitle i, HasId i, HasField "checks" i (Checks ds), ToJSON i, ToJSON ds) => Item i ds

-- TODO: a property class with different constraints
{-

Property [Data] Checks Shrinker

where data is serialisable to and from JSON so can rerun speicific instances and add
to list

-}



-- TODO:: look into listLike

--

class Param a where
  frequency :: Frequency

class (Param a, Param b) => ValidDepends a b

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

instance ValidDepends Once Once

instance ValidDepends Once Thread
instance ValidDepends Thread Thread

instance ValidDepends Once Each
instance ValidDepends Thread Each
instance ValidDepends Each Each


data Hook rc effs loc i o where
  Before ::
    (Param loc) =>
    { action :: rc -> Eff effs o
    } ->
    Hook rc effs loc () o
  Before' ::
    (Param ploc, Param loc, ValidDepends ploc loc) =>
    { depends :: Hook rc effs ploc pi i
    , action' :: rc -> i -> Eff effs o
    } ->
    Hook rc effs loc i o
  After ::
    (Param loc) =>
    { afterAction :: rc -> Eff effs ()
    } ->
    Hook rc effs loc () ()
  After' ::
    (Param ploc, Param loc, ValidDepends ploc loc) =>
    { afterDepends :: Hook rc effs ploc pi i
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
    (Param ploc, Param loc, ValidDepends ploc loc) =>
    { depends :: Hook rc effs ploc pi i
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

data Test c rc tc effs hi where
  Full ::
    (Item i ds, ToJSON as) =>
    { config :: tc
    , action :: rc -> i -> Eff effs as
    , parse :: as -> Eff '[E.Error ParseException] ds
    , items :: rc -> c i
    } ->
    Test c rc tc effs ()
  Full' ::
    (Item i ds, ToJSON as) =>
    { depends :: Hook rc effs loc pi hi
    , config' :: tc
    , action' :: rc -> hi -> i -> Eff effs as
    , parse' :: as -> Eff '[E.Error ParseException] ds
    , items' :: rc -> c i
    } ->
    Test c rc tc effs hi
  NoParse ::
    (Item i ds) =>
    { config :: tc
    , action :: rc -> i -> Eff effs ds
    , items :: rc -> c i
    } ->
    Test c rc tc effs ()
  NoParse' ::
    (Item i ds) =>
    { depends :: Hook rc effs loc pi hi
    , config' :: tc
    , action' :: rc -> hi -> i -> Eff effs ds
    , items' :: rc -> c i
    } ->
    Test c rc tc effs hi
  Single ::
    (ToJSON ds) =>
    { config :: tc
    , singleAction :: rc -> Eff effs ds
    , checks :: Checks ds
    } ->
    Test c rc tc effs ()
  Single' ::
    (ToJSON ds) =>
    { depends :: Hook rc effs loc pi hi
    , config' :: tc
    , singleAction' :: rc -> hi -> Eff effs ds
    , checks' :: Checks ds
    } ->
    Test c rc tc effs hi

-- TODO :: RENAME
-- SuiteElement => Node
-- Test => Fixture
{-
Suite 
  - Nodes 
    - Hooks 
    - Fixtures 
      - Test
      - Test
      - Test
-}
data SuiteElement m rc tc effs hi where
  Hook ::
    (Param loc) =>
    { path :: Path
    , hook :: Hook rc effs loc hi o
    , subNodes :: m (SuiteElement m rc tc effs o)
    } ->
    SuiteElement m rc tc effs hi
  Test ::
    { path :: Path
    , test :: Test m rc tc effs hi
    } ->
    SuiteElement m rc tc effs hi

data ExeParams m rc tc effs where
  ExeParams ::
    { suite :: m (SuiteElement m rc tc effs ())
    , interpreter :: forall a. Eff effs a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    ExeParams m rc tc effs



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
todo plugin issu 

differ 
case 1: 
  hook.depends = hook1 
  hook depends hook 1 
  
case 2: 
  hook1 depends hook2
  hook = hook1 
  hook depends hook2
  


-}