module TestCompilerErrorDemo where

import Check
import Core (ParseException)
import DSL.Internal.NodeLog (NodeLog, Path (NodePath))
import DSL.OutEffect (Out)
import Effectful as EF
  ( Eff,
    type (:>),
  )
import PyrethrumBase
import PyrethrumExtras (txt)
import WebDriverEffect as WE
import WebDriverSpec (DriverStatus (Ready))
import DSL.Logging (log)
import GHC.TypeLits qualified as TL

-- ########## Types #############

data AS = AS
  { status :: DriverStatus,
    checkButtonText :: Text
  }
  deriving (Show)
data DS = DS
  { status :: DriverStatus,
    checkButtonText :: Text
  }
  deriving (Show)

data Data = Item
  { id :: Int,
    title :: Text,
    checks :: Checks DS
  }
  deriving (Show, Read)

-- ########## Alternate Types #############

data ASAlt = ASAlt
  { status :: DriverStatus,
    checkButtonText :: Text
  }
  deriving (Show)
data DSAlt = DSAlt
  { status :: DriverStatus,
    checkButtonText :: Text
  }
  deriving (Show)

data DataAlt = ItemAlt
  { id :: Int,
    title :: Text,
    checks :: Checks DSAlt
  }
  deriving (Show, Read)


-- #######################################

compilesSuite :: Suite
compilesSuite =
  [Fixture (NodePath "WebDriverDemo" "test") test]

test :: Fixture ()
test = Full config action parse items

config :: FixtureConfig
config = FxCfg "test" DeepRegression

driver_status :: (WebUI :> es, Out NodeLog :> es) => Eff es DriverStatus
driver_status = do 
  status <- driverStatus
  log $ "the driver status is: " <> txt status
  pure status

action ::  RunConfig -> Data -> Eff es AS
action _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blahh"}

parse :: AS -> Either ParseException DS
parse AS {..} = pure $ DS {..}

items :: RunConfig -> DataSource Data
items _rc =
  ItemList [ ]

-- #### Compiler Error Wrong DataSource Data Type #### --

failsSuite :: Suite
failsSuite =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt]

testAlt :: Fixture ()
testAlt = Full config action parse itemsAlt

itemsAlt :: RunConfig -> DataSource DataAlt
itemsAlt _rc =
  ItemList [ ] 

-- #### Compiler Error Wrong Parse Result Data Type #### --

failsSuite1 :: Suite
failsSuite1 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt2]

testAlt2 :: Fixture ()
testAlt2 = Full config action parseAlt2 items

parseAlt2 :: AS -> Either ParseException DSAlt
parseAlt2 AS {..} = pure $ DSAlt {..}

-- #### Compiler Error ApState Result Data Type #### --

failsSuite3 :: Suite
failsSuite3 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt3]

testAlt3 :: Fixture ()
testAlt3 = Full config action3 parse items

action3 ::  RunConfig -> Data -> Eff es ASAlt
action3 _rc _i = 
  pure $ ASAlt {status = Ready, checkButtonText = "Blahh"}

-- #### Compiler Error Wrrong Data Source in Action #### --


failsSuite4 :: Suite
failsSuite4 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt4]

testAlt4 :: Fixture ()
testAlt4 = Full config action4 parse items

action4 ::  RunConfig -> DataAlt -> Eff es AS
action4 _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blahh"}

