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


-- TODO:: look into listLike

--

class Frequency a where
  frequency :: Hz

class CanDependOn a b

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

data Hook m rc hz i o where
  Before ::
    (Frequency hz) =>
    { action :: rc -> m o
    } ->
    Hook m rc hz () o
  Before' ::
    (Frequency phz, Frequency hz, CanDependOn hz phz) =>
    { depends :: Hook m rc phz pi i
    , action' :: rc -> i -> m o
    } ->
    Hook m rc hz i o
  After ::
    (Frequency hz) =>
    { afterAction :: rc -> m ()
    } ->
    Hook m rc hz () ()
  After' ::
    (Frequency phz, Frequency hz, CanDependOn hz phz) =>
    { afterDepends :: Hook m rc phz pi i
    , afterAction' :: rc -> m ()
    } ->
    Hook m rc hz i i
  Around ::
    (Frequency hz) =>
    { setup :: rc -> m o
    , teardown :: rc -> o -> m ()
    } ->
    Hook m rc hz () o
  Around' ::
    (Frequency phz, Frequency hz, CanDependOn hz phz) =>
    { depends :: Hook m rc phz pi i
    , setup' :: rc -> i -> m o
    , teardown' :: rc -> o -> m ()
    } ->
    Hook m rc hz i o

hookFrequency :: forall m rc hz i o. (Frequency hz) => Hook m rc hz i o -> Hz
hookFrequency _ = frequency @hz

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
    , depends :: Hook m rc hz pi hi
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
    , depends :: Hook m rc hz pi hi
    , action' :: rc -> hi -> i -> m ds
    , items' :: rc -> c i
    } ->
    Fixture m c rc tc hi

data Node m c rc tc hi where
  Hook ::
    (Frequency hz) =>
    { path :: Path
    , hook :: Hook m rc hz hi o
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

-- Todo
-- see weeder :: HIE
-- start with:: https://github.com/theGhostJW/pyrethrum-extras/blob/master/src/Language/Haskell/TH/Syntax/Extended.hs
-- see also:: https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/Language-Haskell-TH-Syntax.html#t:Name
-- part 5 reinstate filtering // tree shaking
