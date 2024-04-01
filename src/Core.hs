{-# LANGUAGE UndecidableInstances #-}

module Core where

import Check (Checks)
import DSL.Internal.ApEvent hiding (Check)
import Data.Aeson (ToJSON (..))
import GHC.Records (HasField)
import Internal.ThreadEvent (Hz (..))

data Before
data After
data Around

class When a

instance When Before
instance When After
instance When Around

newtype CheckFailure = CheckFailure Text
  deriving (Show)

newtype ParseException
  = ParseFailure Text
  deriving (Show, Read)

instance Exception ParseException

type HasTitle a = HasField "title" a Text

type HasId a = HasField "id" a Int

class (HasTitle a, Show a, ToJSON a, Eq a) => Config a

type Item i ds = (HasTitle i, HasId i, HasField "checks" i (Checks ds), Show i, Read i, Show ds, Show i)

failParser :: Text -> Either ParseException a
failParser = Left . ParseFailure

-- add a function to serialise known properties to JSON on id, title, -> optional tricky may need to parse from string  notes, calcs
-- https://softwareengineering.stackexchange.com/a/250135https://softwareengineering.stackexchange.com/a/250135
{-

Property [Data] Checks Shrinker

where data is serialisable to and from JSON so can rerun speicific instances and add
to list

-}

{-
TODO:: idea defect reconciler
 - can pretty print (No JSON) to log so long as id/title/hash is logged and a hash of the show is logged so
    we can warn if the item has changed since it was run

 - for property based / generative tests we insist on deriving JSON and log as YAML and pprint
    on failure

 - consider removing ids from items
    ~ regular use title which must be unique or title stub (which can include test)
    ~ property based stub
      ~ generator

 - global stub function
 - tst stub function  Item -> [Item] -> [Item] ~ ignores list and creates singleton
-}

-- TODO:: look into listLike

--

class Frequency a where
  frequency :: Hz

class (Frequency a, Frequency b) => CanDependOn a b

data Once
data Thread
data Each

instance Frequency Once where
  frequency :: Hz
  frequency = Once

instance Frequency Thread where
  frequency :: Hz
  frequency = Thread

instance Frequency Each where
  frequency :: Hz
  frequency = Each

instance CanDependOn Once Once

instance CanDependOn Thread Once
instance CanDependOn Thread Thread

instance CanDependOn Each Once
instance CanDependOn Each Thread
instance CanDependOn Each Each

data Hook m rc loc i o where
  Before ::
    (Frequency loc) =>
    { action :: rc -> m o
    } ->
    Hook m rc loc () o
  Before' ::
    (Frequency ploc, Frequency loc, CanDependOn loc ploc) =>
    { depends :: Hook m rc ploc pi i
    , action' :: rc -> i -> m o
    } ->
    Hook m rc loc i o
  After ::
    (Frequency loc) =>
    { afterAction :: rc -> m ()
    } ->
    Hook m rc loc () ()
  After' ::
    (Frequency ploc, Frequency loc, CanDependOn loc ploc) =>
    { afterDepends :: Hook m rc ploc pi i
    , afterAction' :: rc -> m ()
    } ->
    Hook m rc loc i i
  Around ::
    (Frequency loc) =>
    { setup :: rc -> m o
    , teardown :: rc -> o -> m ()
    } ->
    Hook m rc loc () o
  Around' ::
    (Frequency ploc, Frequency loc, CanDependOn loc ploc) =>
    { depends :: Hook m rc ploc pi i
    , setup' :: rc -> i -> m o
    , teardown' :: rc -> o -> m ()
    } ->
    Hook m rc loc i o

hookFrequency :: forall m rc loc i o. (Frequency loc) => Hook m rc loc i o -> Hz
hookFrequency _ = frequency @loc

newtype StubLoc = StubLoc Text
data Addressed a = Addressed
  { loc :: StubLoc
  , value :: a
  }

data Fixture m c rc tc hi where
  Full ::
    (Item i ds, Show as) =>
    { config :: tc
    , action :: rc -> i -> m as
    , parse :: as -> Either ParseException ds
    , items :: rc -> c i
    } ->
    Fixture m c rc tc ()
  Full' ::
    (Item i ds, Show as) =>
    { config' :: tc
    , depends :: Hook m rc loc pi hi
    , action' :: rc -> hi -> i -> m as
    , parse' :: as -> Either ParseException ds
    , items' :: rc -> c i
    } ->
    Fixture m c rc tc hi
  Direct ::
    (Item i ds) =>
    { config :: tc
    , action :: rc -> i -> m ds
    , items :: rc -> c i
    } ->
    Fixture m c rc tc ()
  Direct' ::
    (Item i ds) =>
    { config' :: tc
    , depends :: Hook m rc loc pi hi
    , action' :: rc -> hi -> i -> m ds
    , items' :: rc -> c i
    } ->
    Fixture m c rc tc hi
  Single ::
    (Show ds) =>
    { config :: tc
    , singleAction :: rc -> m ds
    , checks :: Checks ds
    } ->
    Fixture m c rc tc ()
  Single' ::
    (Show ds) =>
    { config' :: tc
    , depends :: Hook m rc loc pi hi
    , singleAction' :: rc -> hi -> m ds
    , checks' :: Checks ds
    } ->
    Fixture m c rc tc hi

data Node m c rc tc hi where
  Hook ::
    (Frequency loc) =>
    { path :: Path
    , hook :: Hook m rc loc hi o
    , subNodes :: c (Node m c rc tc o)
    } ->
    Node m c rc tc hi
  Fixture ::
    { path :: Path
    , fixture :: Fixture m c rc tc hi
    } ->
    Node m c rc tc hi

data ExeParams m c rc tc where
  ExeParams ::
    { suite :: c (Node m c rc tc ())
    , interpreter :: forall a. m a -> IO (Either (CallStack, SomeException) a)
    , runConfig :: rc
    } ->
    ExeParams m c rc tc

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