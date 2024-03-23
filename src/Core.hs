{-# LANGUAGE UndecidableInstances #-}

module Core where

import DSL.Internal.ApEvent hiding (Check)
import Data.Aeson (ToJSON (..))
import Internal.ThreadEvent ( Hz(..) )

import Effectful (Eff)
{- TODO: 
  - consider removing Eff from Core (Eff effs => m) 
  - Effectful runner would just be parameterised (Eff effs) 
  - change parse to Either
  - have to see how this would affect dependencies between Hooks and tests
    - should not nhave to use whole effect stack across all Hooks and Fixtures
    - expect to just work => implement after examples are done
  -}
import Effectful.Error.Static as E (Error)
import GHC.Records (HasField)
import Check (Checks)


data Before
data After
data Around

class When a

instance When Before
instance When After
instance When Around

newtype CheckFailure = CheckFailure Text
  deriving (Show)

newtype ParseException = ParseException Text
  deriving (Show, Read)

instance Exception ParseException

type HasTitle a = HasField "title" a Text

type HasId a = HasField "id" a Int

class (HasTitle a, Show a, ToJSON a, Eq a) => Config a

type Item i ds = (HasTitle i, HasId i, HasField "checks" i (Checks ds), Show i, Show ds)
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

data Hook rc effs loc i o where
  Before ::
    (Frequency loc) =>
    { action :: rc -> Eff effs o
    } ->
    Hook rc effs loc () o
  Before' ::
    (Frequency ploc, Frequency loc, CanDependOn loc ploc) =>
    { depends :: Hook rc effs ploc pi i
    , action' :: rc -> i -> Eff effs o
    } ->
    Hook rc effs loc i o
  After ::
    (Frequency loc) =>
    { afterAction :: rc -> Eff effs ()
    } ->
    Hook rc effs loc () ()
  After' ::
    (Frequency ploc, Frequency loc, CanDependOn loc ploc) =>
    { afterDepends :: Hook rc effs ploc pi i
    , afterAction' :: rc -> Eff effs ()
    } ->
    Hook rc effs loc i i
  Around ::
    (Frequency loc) =>
    { setup :: rc -> Eff effs o
    , teardown :: rc -> o -> Eff effs ()
    } ->
    Hook rc effs loc () o
  Around' ::
    (Frequency ploc, Frequency loc, CanDependOn loc ploc) =>
    { depends :: Hook rc effs ploc pi i
    , setup' :: rc -> i -> Eff effs o
    , teardown' :: rc -> o -> Eff effs ()
    } ->
    Hook rc effs loc i o


hookFrequency :: forall rc effs loc i o. Frequency loc => Hook rc effs loc i o -> Hz
hookFrequency _ = frequency @loc

newtype StubLoc = StubLoc Text
data Addressed a = Addressed
  { loc :: StubLoc
  , value :: a
  }

data Fixture c rc tc effs hi where
  Full ::
    (Item i ds, Show as) =>
    { config :: tc
    , action :: rc -> i -> Eff effs as
    , parse :: as -> Eff '[E.Error ParseException] ds
    , items :: rc -> c i
    } ->
    Fixture c rc tc effs ()
  Full' ::
    (Item i ds, Show as) =>
    { depends :: Hook rc effs loc pi hi
    , config' :: tc
    , action' :: rc -> hi -> i -> Eff effs as
    , parse' :: as -> Eff '[E.Error ParseException] ds
    , items' :: rc -> c i
    } ->
    Fixture c rc tc effs hi
  NoParse ::
    (Item i ds) =>
    { config :: tc
    , action :: rc -> i -> Eff effs ds
    , items :: rc -> c i
    } ->
    Fixture c rc tc effs ()
  NoParse' ::
    (Item i ds) =>
    { depends :: Hook rc effs loc pi hi
    , config' :: tc
    , action' :: rc -> hi -> i -> Eff effs ds
    , items' :: rc -> c i
    } ->
    Fixture c rc tc effs hi
  Single ::
    (Show ds) =>
    { config :: tc
    , singleAction :: rc -> Eff effs ds
    , checks :: Checks ds
    } ->
    Fixture c rc tc effs ()
  Single' ::
    (Show ds) =>
    { depends :: Hook rc effs loc pi hi
    , config' :: tc
    , singleAction' :: rc -> hi -> Eff effs ds
    , checks' :: Checks ds
    } ->
    Fixture c rc tc effs hi

data Node m rc tc effs hi where
  Hook ::
    (Frequency loc) =>
    { path :: Path
    , hook :: Hook rc effs loc hi o
    , subNodes :: m (Node m rc tc effs o)
    } ->
    Node m rc tc effs hi
  Fixture ::
    { path :: Path
    , fixture :: Fixture m rc tc effs hi
    } ->
    Node m rc tc effs hi

data ExeParams m rc tc effs where
  ExeParams ::
    { suite :: m (Node m rc tc effs ())
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