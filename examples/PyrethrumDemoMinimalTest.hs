module PyrethrumDemoMinimalTest where

import Check (Checks, chk)
import Core (After, Before, Once, ParseException, Thread)
import DSL.Internal.NodeLog (NodeLog (..), UserLog (Info))
import DSL.OutEffect (out)
import Effectful (Eff)
import PyrethrumBase (
  Action,
  Depth (..),
  Fixture,
  HasLog,
  Hook (..),
  LogEffs,
  RunConfig (..),
  FixtureConfig (..),
  FixtureConfig, Country (..), Environment (..), fxCfg, mkFull', runConfig, environment,
 )
import PyrethrumExtras (txt)
import CoreTypeFamilies (DataSource (Items))
import GHC.Records (HasField)
import PyrethrumDemoTest hiding (chkIncomeLessThan10, valLessThan10, VS, runSomethingToDoWithTestDepth, AS, Item2, items2, parse2, action2, infoThreadHook, config2, addOnceIntHook, HookInfo, logShow, log)

{-
Note:: tried alternative with individual hook types but the results
were starting to look more complex than the original so abandonned.
-}

log :: (HasLog es) => Text -> Eff es ()
log = out . User . Info

logShow :: (HasLog es, Show a) => a -> Eff es ()
logShow = out . User . Info . txt

{- Demonstraits using partial effect
  type LogEffs a = forall es. (Out NodeLog :> es) => Eff es a

  Hook has all the effects of the application but will compile with
  an action that only requires a sublist of these effects
-}
logReturnInt :: () ->  LogEffs Int
logReturnInt _a = log "Returning One" >> pure 1

runSomethingToDoWithTestDepth :: Depth -> Action ()
runSomethingToDoWithTestDepth = logShow

demoOnceAfterHook :: Hook Once After () ()
demoOnceAfterHook =
  AfterHook
    { afterAction =  log "After all tests"
    }

intOnceHook :: Hook Once Before () Int
intOnceHook =
  BeforeHook'
    { depends = PyrethrumDemoMinimalTest.demoOnceAfterHook
    , action' = PyrethrumDemoMinimalTest.logReturnInt 
    }

addOnceIntHook :: Hook Once Before Int Int
addOnceIntHook =
  BeforeHook'
    { depends = PyrethrumDemoTest.intOnceHook
    , action' =
        \i -> do
          log $ "beforeAll' " <> txt i
          pure $ i + 1
    }

_intThreadHook :: Hook Thread Before () Int
_intThreadHook = BeforeHook $ do
  log "deriving meaning of life' "
  pure 42

data HookInfo = HookInfo
  { message :: Text
  , value :: Int
  }
  deriving (Show, Generic)

type BreakIt = Hook Thread Before Int HookInfo

infoThreadHook :: BreakIt
infoThreadHook = BeforeHook' PyrethrumDemoMinimalTest.intOnceHook $ \i -> do
  log $ "beforeThread' " <> txt i
  pure $ HookInfo "Hello there" i

-- ############### Test the Lot Child ###################

config2 :: FixtureConfig
config2 = FxCfg "test" DeepRegression

test2 :: Fixture HookInfo
test2 = mkFull' config2 infoThreadHook action2 parse2 items2

action2 ::  HookInfo -> Item2 -> Action AS
-- action2 RunConfig{country, depth, environment} HookInfo{value = hookVal} itm = do
action2 HookInfo{value = hookVal} itm = do
  logShow itm
  RunConfig {country, depth} <- runConfig
  when (country == AU )$
    log "Aus test"
  when (country == NZ) $
    log "NZ test"
  runSomethingToDoWithTestDepth depth
  env <- environment
  unless (env == Prod) $
    log "Completing payment with test credit card"
  pure $ AS (itm.value + 1 + hookVal) $ txt itm.value

parse2 :: AS -> Either ParseException VS
parse2 AS{..} = pure VS{..}

data AS = AS
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

data VS = VS
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

data Item2 = Item2
  { id :: Int
  , title :: Text
  , value :: Int
  , checks :: Checks VS
  }
  deriving (Show, Read)

valLessThan10 :: (Ord a, HasField "value" r a, Num a) => r -> Bool
valLessThan10 ds = ds.value < 10

chkIncomeLessThan10 :: (Ord a, HasField "value" r a, Num a) => Checks r
chkIncomeLessThan10 = chk "test3" valLessThan10 

items2 :: RunConfig -> DataSource Item2 VS
items2 rc =
  Items $ filter
    (\i -> rc.depth == Regression || i.id < 10)
    [ Item2
        { id = 1
        , title = "test the value is one"
        , value = 2
        , checks =
            chk "test" ((== 1) . (.value))
              <> chk "test2" (\VS{..} -> value < 10)
              <> chkIncomeLessThan10
        }
    ]

-- ############### Construct Tests ###################
-- this will be generated either by implmenting deriving,
-- check out DeriveAnyClass or template haskell
-- could also look into creating un unconstrained data types
-- all members of a convertable typeclass (specialize??)
-- and converting to a true test fixture at the bottom of the file
-- after deriveJson and Item
-- TODO: precompiler template haskell
-- need to check error messages carefully
-- finalise templatehaskell vs deriving for these classes

-- ############### Demo Default Configs ###################

cfg :: FixtureConfig
cfg = fxCfg "test"
