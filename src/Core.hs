module Core where

import CoreUtils (Hz (..))
import DSL.Internal.NodeLog (LogSink, Path)
import Filter (Filters)
import CoreTypeFamilies ( HasTestFields, DataSource (..) )

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

data Hook m hz i o where
  Before ::
    (Frequency hz) =>
    { action :: m o
    } ->
    Hook m hz () o
  Before' ::
    (Frequency phz, Frequency hz, CanDependOn hz phz) =>
    { depends :: Hook m phz pi i,
      action' :: i -> m o
    } ->
    Hook m hz i o
  After ::
    (Frequency hz) =>
    { afterAction :: m ()
    } ->
    Hook m hz () ()
  After' ::
    (Frequency phz, Frequency hz, CanDependOn hz phz) =>
    { afterDepends :: Hook m phz pi i,
      afterAction' :: m ()
    } ->
    Hook m hz i i
  Around ::
    (Frequency hz) =>
    { setup :: m o,
      teardown :: o -> m ()
    } ->
    Hook m hz () o
  Around' ::
    (Frequency phz, Frequency hz, CanDependOn hz phz) =>
    { depends :: Hook m  phz pi i,
      setup' :: i -> m o,
      teardown' :: o -> m ()
    } ->
    Hook m hz i o

hookFrequency :: forall m hz i o. (Frequency hz) => Hook m hz i o -> Hz
hookFrequency _ = frequency @hz

getConfig :: Fixture m rc fc hi -> fc
getConfig = \case
  Full {config} -> config
  Full' {config'} -> config'
  Direct {config} -> config
  Direct' {config'} -> config'

fixtureEmpty :: forall m rc fc hi. rc -> Fixture m rc fc hi -> Bool
fixtureEmpty rc = 
  \case
  Full {dataSource} -> dsEmpty $ dataSource rc
  Full' {dataSource'} -> dsEmpty $ dataSource' rc
  Direct {dataSource} -> dsEmpty $ dataSource rc
  Direct' {dataSource'} -> dsEmpty $ dataSource' rc
  where
    dsEmpty :: forall i vs. DataSource i vs -> Bool
    dsEmpty = \case
      Items itms -> case itms of
        [] -> True
        _ -> False
      Property {} -> False

data Fixture m rc fc hi where
  Full ::
    (HasTestFields i vs, Show as) =>
    { config :: fc,
      action :: i -> m as,
      parse :: as -> Either ParseException vs,
      dataSource :: rc -> DataSource i vs
    } ->
    Fixture m rc fc ()
  Full' ::
    (HasTestFields i vs, Show as) =>
    { config' :: fc,
      depends :: Hook m hz pi hi,
      action' :: hi -> i -> m as,
      parse' :: as -> Either ParseException vs,
      dataSource' :: rc -> DataSource i vs
    } ->
    Fixture m rc fc hi
  Direct ::
    (HasTestFields i vs) =>
    { config :: fc,
      action :: i -> m vs,
      dataSource :: rc -> DataSource i vs
    } ->
    Fixture m rc fc ()
  Direct' ::
    (HasTestFields i vs) =>
    { config' :: fc,
      depends :: Hook m hz pi hi,
      action' :: hi -> i -> m vs,
      dataSource' :: rc -> DataSource i vs
    } ->
    Fixture m rc fc hi

data Node m rc fc hi where
  Hook ::
    (Frequency hz) =>
    { path :: Path,
      hook :: Hook m hz hi o,
      subNodes :: [Node m rc fc o]
    } ->
    Node m rc fc hi
  Fixture ::
    { path :: Path,
      fixture :: Fixture m rc fc hi
    } ->
    Node m rc fc hi

data Mode
  = Run
  | Listing
      { includeSteps :: Bool,
        includeChecks :: Bool
      }

data SuiteExeParams m rc fc where
  MkSuiteExeParams ::
    { suite :: [Node m rc fc ()],
      mode :: Mode,
      filters :: Filters rc fc,
      interpreter :: forall a. LogSink -> m a -> IO a,
      runConfig :: rc
    } ->
    SuiteExeParams m rc fc

-- Todo
-- see weeder :: HIE
-- start with:: https://github.com/theGhostJW/pyrethrum-extras/blob/master/src/Language/Haskell/TH/Syntax/Extended.hs
-- see also:: https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/Language-Haskell-TH-Syntax.html#t:Name
-- part 5 reinstate filtering // tree shaking
