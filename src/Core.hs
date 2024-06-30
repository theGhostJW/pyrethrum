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
    { depends :: Hook m rc phz pi i,
      action' :: rc -> i -> m o
    } ->
    Hook m rc hz i o
  After ::
    (Frequency hz) =>
    { afterAction :: rc -> m ()
    } ->
    Hook m rc hz () ()
  After' ::
    (Frequency phz, Frequency hz, CanDependOn hz phz) =>
    { afterDepends :: Hook m rc phz pi i,
      afterAction' :: rc -> m ()
    } ->
    Hook m rc hz i i
  Around ::
    (Frequency hz) =>
    { setup :: rc -> m o,
      teardown :: rc -> o -> m ()
    } ->
    Hook m rc hz () o
  Around' ::
    (Frequency phz, Frequency hz, CanDependOn hz phz) =>
    { depends :: Hook m rc phz pi i,
      setup' :: rc -> i -> m o,
      teardown' :: rc -> o -> m ()
    } ->
    Hook m rc hz i o

hookFrequency :: forall m rc hz i o. (Frequency hz) => Hook m rc hz i o -> Hz
hookFrequency _ = frequency @hz

data DataSource i = ItemList [i] | Property i deriving (Show, Functor)

getConfig :: Fixture m rc fc hi -> fc
getConfig = \case
  Full {config} -> config
  Full' {config'} -> config'
  Direct {config} -> config
  Direct' {config'} -> config'

fixtureEmpty :: forall m rc fc hi. rc -> Fixture m rc fc hi -> Bool
fixtureEmpty rc = \case
  Full {items} -> dsEmpty $ items rc
  Full' {items'} -> dsEmpty $ items' rc
  Direct {items} -> dsEmpty $ items rc
  Direct' {items'} -> dsEmpty $ items' rc
  where
    dsEmpty = \case
      ItemList itms -> null itms
      Property {} -> False

data Fixture m rc fc hi where
  Full ::
    (Item i ds, Show as) =>
    { config :: fc,
      action :: rc -> i -> m as,
      parse :: as -> Either ParseException ds,
      items :: rc -> DataSource i
    } ->
    Fixture m rc fc ()
  Full' ::
    (Item i ds, Show as) =>
    { config' :: fc,
      depends :: Hook m rc hz pi hi,
      action' :: rc -> hi -> i -> m as,
      parse' :: as -> Either ParseException ds,
      items' :: rc -> DataSource i
    } ->
    Fixture m rc fc hi
  Direct ::
    (Item i ds) =>
    { config :: fc,
      action :: rc -> i -> m ds,
      items :: rc -> DataSource i
    } ->
    Fixture m rc fc ()
  Direct' ::
    (Item i ds) =>
    { config' :: fc,
      depends :: Hook m rc hz pi hi,
      action' :: rc -> hi -> i -> m ds,
      items' :: rc -> DataSource i
    } ->
    Fixture m rc fc hi

data Node m rc fc hi where
  Hook ::
    (Frequency hz) =>
    { path :: Path,
      hook :: Hook m rc hz hi o,
      subNodes :: [Node m rc fc o]
    } ->
    Node m rc fc hi
  Fixture ::
    { path :: Path,
      fixture :: Fixture m rc fc hi
    } ->
    Node m rc fc hi

data ExeParams m rc fc where
  ExeParams ::
    { suite :: [Node m rc fc ()],
      interpreter :: forall a. m a -> IO (Either (CallStack, SomeException) a),
      runConfig :: rc
    } ->
    ExeParams m rc fc

-- Todo
-- see weeder :: HIE
-- start with:: https://github.com/theGhostJW/pyrethrum-extras/blob/master/src/Language/Haskell/TH/Syntax/Extended.hs
-- see also:: https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/Language-Haskell-TH-Syntax.html#t:Name
-- part 5 reinstate filtering // tree shaking
