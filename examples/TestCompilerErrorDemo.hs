{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

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
import CoreTypeFamilies (DataSource (..))

-- ########## Types #############

data AS = AS
  { status :: DriverStatus,
    checkButtonText :: Text
  }
  deriving (Show)
data VS = VS
  { status :: DriverStatus,
    checkButtonText :: Text
  }
  deriving (Show)

data Data = HasTestFields
  { id :: Int,
    title :: Text,
    checks :: Checks VS
  }
  deriving (Show, Read)

-- ########## Alternate Types #############

data ASAlt = ASAlt
  { status1 :: DriverStatus,
    checkButtonText1 :: Text
  }
  deriving (Show)
data VSAlt = VSAlt
  { statusAlt :: DriverStatus,
    checkButtonTextAlt :: Text
  }
  deriving (Show)

data DataAlt = ItemAlt
  { id :: Int,
    title :: Text,
    checks :: Checks VSAlt
  }
  deriving (Show, Read)


-- #######################################

compilesSuite :: Suite
compilesSuite =
  [Fixture (NodePath "WebDriverDemo" "test") test]

test :: Fixture ()
test = mkFull config action parse data'

config :: FixtureConfig
config = FxCfg "test" DeepRegression

driver_status :: (WebUI :> es, Out NodeLog :> es) => Eff es DriverStatus
driver_status = do 
  status <- driverStatus
  log $ "the driver status is: " <> txt status
  pure status

action ::  RunConfig -> Data -> Eff es AS
action _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blah"}

parse :: AS -> Either ParseException VS
parse AS {..} = pure $ VS {..}

data' :: RunConfig -> DataSource Data VS
data' _rc =
  Items [ ]

-- -- #### Compiler Error Data  missing Checks #### --

-- NOTE :: this error will only come up in a later phase of compilation
-- so it may not appear when the other errors in this file are active.
-- To see this error comment out the failures below this case

failsSuite2 :: Suite
failsSuite2 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt2']

testAlt2' :: Fixture ()
testAlt2' = Full config action2 parseAlt2 data2

action2 ::  RunConfig -> Data2 -> Eff es AS
action2 _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blah"}

parseAlt2 :: AS -> Either ParseException VS
parseAlt2 AS {..} = pure $ VS {..}

data2 :: RunConfig -> DataSource Data2 VS 
data2 _rc =
  Items [ Item2 1 "one" ]

data Data2 = Item2
  { id :: Int,
   title :: Text
  }
  deriving (Show, Read)

-- #### Compiler Error - Data Source Mismatch #### --


failsSuite4 :: Suite
failsSuite4 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt4]

testAlt4 :: Fixture ()
testAlt4 = Full config action4 parse data'

action4 ::  RunConfig -> DataAlt -> Eff es AS
action4 _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blah"}


-- #### Compiler Error Action ApState Mismatch with Parser In #### --

failsSuite3 :: Suite
failsSuite3 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt3]

testAlt3 :: Fixture ()
testAlt3 = Full config action3 parse data'

action3 ::  RunConfig -> Data -> Eff es ASAlt
action3 _rc _i = 
  pure $ ASAlt {status1 = Ready, checkButtonText1 = "Blahh"}


-- #### Compiler Error Wrong DataSource Data Type - Direct #### --
-- example of bad error message
failsSuite5 :: Suite 
failsSuite5 = 
  [Fixture (NodePath "WebDriverDemo" "test") testAlt5]

testAlt5 :: Fixture ()
testAlt5 = Direct config5 action5 data'

config5 :: FixtureConfig
config5 = FxCfg "test" DeepRegression


dataWrongType5 :: RunConfig -> DataSource VSAlt DataAlt
dataWrongType5 _rc =
  Items [ ] 

action5 ::  RunConfig -> Data -> Eff es AS
action5 _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blah"}


-- #### Compiler Error Wrong Parse Result Data Type (Incorrect Type Signature) #### --

-- Bad checks :: type signature is wrong for data1 ~ VSAlt is actually VS
-- can't improve this as can't get compile time type of specific field
-- Checks without writing a typeclass instance per .
-- At least there is a local error on Items
-- Removing the type signature for data 1 will invoke the custom
-- type error

failsSuite1 :: Suite
failsSuite1 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt1]

testAlt1 :: Fixture ()
testAlt1 = Full config action1 parseAlt1 data1

action1 ::  RunConfig -> Data1 -> Eff es AS
action1 _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blah"}

parseAlt1 :: AS -> Either ParseException VSAlt
parseAlt1 AS {status, checkButtonText} = pure $ VSAlt {
  statusAlt = status, 
  checkButtonTextAlt = checkButtonText}

-- lies about the data type its actually VS
data1 :: RunConfig -> DataSource Data1 VSAlt 
data1 _rc =
  Items [ 
   Item1{ 
    id = 1,
    title = "one",
    checks = chk "Driver is ready" (\v -> v.status == Ready)
  }]

data Data1 = Item1
  { id :: Int,
    title :: Text,
    checks :: Checks VS
  }
  deriving (Show, Read)


-- #### Compiler Error Wrong Parse Result Does Not Match Checks ### --


failsSuite11 :: Suite
failsSuite11 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt11]

testAlt11 :: Fixture ()
testAlt11 = Full config action11 parseAlt11 data11

action11 ::  RunConfig -> Data11 -> Eff es AS
action11 _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blah"}

parseAlt11 :: AS -> Either ParseException VSAlt 
parseAlt11 AS {status, checkButtonText} = pure $ VSAlt {
  statusAlt = status, 
  checkButtonTextAlt = checkButtonText}

-- lies about the data type its actually VS
data11 :: RunConfig -> DataSource Data11 VS 
data11 _rc =
  Items [ 
   Item11{ 
    id = 1,
    title = "one",
    checks = chk "Driver is ready" (\v -> v.status == Ready)
  }]

data Data11 = Item11
  { id :: Int,
    title :: Text,
    checks :: Checks VS
  }
  deriving (Show, Read)