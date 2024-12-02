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
test = Full config action parse data'

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

parse :: AS -> Either ParseException DS
parse AS {..} = pure $ DS {..}

data' :: RunConfig -> DataSource Data
data' _rc =
  Items [ ]

  {-
  
  'Text (of kind SYMBOL →ERRORMESSAGE.) Emits the
symbol verbatim. Note that this is not
Data.Text.Text.
• 'ShowType (of kind K →ERRORMESSAGE.) Prints the name
of the given type.
• '(:<>:) (of kind ERRORMESSAGE →ERRORMESSAGE →
ERRORMESSAGE.) Concatenate two ERRORMESSAGEs
side-by-side.
• '(:$$:) (of kind ERRORMESSAGE →ERRORMESSAGE →
ERRORMESSAGE.) Append one ERRORMESSAGE vertically
atop another.
  
  instance
( TypeError
( Text "Attempting to interpret a number as a
function "↪→
:$$: Text "in the type `"
:<>: ShowType (a -> b)
:<>: Text "'"
:$$: Text "Did you forget to specify the function
you wanted?"↪→
)
) => Num (a -> b) where

  
  -}

{-

instance TypeError
 ( TL.Text "Attempting to interpret a number as a function "
    :$$: TL.Text "in the type `"
    :<>: ShowType (a -> b)
    :<>: TL.Text "'"
    :$$: TL.Text "Did you forget to specify the function you wanted?"
  )
 => Num (a -> b) where


a = 1 True


--  https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_errors.html
instance TypeError (TL.Text "Cannot 'Show' functions." :$$:
                    TL.Text "Perhaps there is a missing argument?")
         => Show (a -> b) where
   showsPrec = error "unreachable"

main = print negate

class FixtureClass a

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Show)

class IsPerson a

-- instance IsPerson Person

instance TypeError
  ( 'Text "AH OH"
  :$$: 'Text "ARGHHHH"
  :<>: ShowType a
  :$$: 'Text "ARGHHHH"
  ) => IsPerson a


peter :: Person
peter = Person "Peter" 23 

usePerson :: IsPerson p => p -> Text
usePerson = txt

shouldFail = usePerson peter

-}

{-
types of error:: 

In fixture

data Fixture hi where
  Full ::
    (C.Item i ds, Show as) =>
    { config :: FixtureConfig,
      action :: RunConfig -> i -> Action as,
      parse :: as -> Either C.ParseException ds,
      dataSource :: RunConfig -> C.DataSource i
    } ->
    Fixture ()
  Full' ::
    (C.Item i ds, Show as, C.Frequency hz) =>
    { config' :: FixtureConfig,
      depends :: Hook hz pw pi a,
      action' :: RunConfig -> a -> i -> Action as,
      parse' :: as -> Either C.ParseException ds,
      dataSource' :: RunConfig -> C.DataSource i
    } ->
    Fixture a
  Direct ::
    forall i ds.
    (C.Item i ds) =>
    { config :: FixtureConfig,
      action :: RunConfig -> i -> Action ds,
      dataSource :: RunConfig -> C.DataSource i
    } ->
    Fixture ()
  Direct' ::
    (C.Item i ds, C.Frequency hz) =>
    { config' :: FixtureConfig,
      depends :: Hook hz pw pi a,
      action' :: RunConfig -> a -> i -> Action ds,
      dataSource' :: RunConfig -> C.DataSource i
    } ->
    Fixture a


-- wrong hook input 
 -- fixture
 -- hook


-}

-- #### Compiler Error Wrong DataSource Data Type #### --
{-

failsSuite :: Suite
failsSuite =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt2]

-- testAlt :: Fixture ()
-- testAlt = Full config action parse dataWrongType

dataWrongType :: RunConfig -> DataSource DataAlt
dataWrongType _rc =
  Items [ ] 

testAlt2 :: Fixture ()
testAlt2 = mkFull config action parse dataWrongType

testAlt3 :: Fixture ()
testAlt3 = mkFullDemoErrMsgs config action parse dataWrongType


-- #### Compiler Error Wrong Parse Result Data Type #### --

failsSuite1 :: Suite
failsSuite1 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt2]

testAlt2 :: Fixture ()
testAlt2 = Full config action parseAlt2 data'

parseAlt2 :: AS -> Either ParseException DSAlt
parseAlt2 AS {..} = pure $ DSAlt {..}

-- #### Compiler Error ApState Result Data Type #### --

failsSuite3 :: Suite
failsSuite3 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt3]

testAlt3 :: Fixture ()
testAlt3 = Full config action3 parse data'

action3 ::  RunConfig -> Data -> Eff es ASAlt
action3 _rc _i = 
  pure $ ASAlt {status = Ready, checkButtonText = "Blahh"}

-- #### Compiler Error Wrong Data Source in Action #### --


failsSuite4 :: Suite
failsSuite4 =
  [Fixture (NodePath "WebDriverDemo" "test") testAlt4]

testAlt4 :: Fixture ()
testAlt4 = Full config action4 parse data'

action4 ::  RunConfig -> DataAlt -> Eff es AS
action4 _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blah"}



-- #### Compiler Error Wrong DataSource Data Type - Direct #### --

failsSuite5 :: Suite 
failsSuite5 = 
  [Fixture (NodePath "WebDriverDemo" "test") testAlt5]

testAlt5 :: Fixture ()
testAlt5 = Direct config action5 dataWrongType

action5 ::  RunConfig -> Data -> Eff es AS
action5 _rc _i = 
  pure $ AS {status = Ready, checkButtonText = "Blah"}


--- simplified GPT 1 ~ note different type parameters

-- Type family to check if two types match
type family CheckTypesMatch a b :: Constraint where
  CheckTypesMatch a a = ()  -- Types match, no error
  CheckTypesMatch a b = TypeError
    ( 'Text "Type mismatch between 'inFnc' and 'outFnc'."
    :$$: 'Text "The output type of 'inFnc' is "
    :<>: 'ShowType a
    :$$: 'Text "But the input type of 'outFnc' is "
    :<>: 'ShowType b
    )

-- Define the IntOut data type with the custom type error
data IntOut where
  IntOut ::
    CheckTypesMatch inTyp inTyp1 =>  -- Constraint to ensure types match
    { inFnc  :: () -> inTyp,
      outFnc :: inTyp1 -> inTyp1 
    } -> IntOut

-- Correct usage: 'inTyp' and 'inTyp1' are both Int
goodIntOut :: IntOut
goodIntOut = IntOut
  { inFnc  = \() -> (42 :: Int),
    outFnc = \x -> 1 + x
  }

-- Incorrect usage: 'inTyp' is Int, 'inTyp1' is String
-- This will trigger the custom compiler error
badIntOut :: IntOut
badIntOut = IntOut
  { inFnc  = \() -> (42 :: Int),
    outFnc = toLower
  }

  --- simplified GPT 2 ~ note same type parameters


  -- Type family to extract the result type of a function
type family ResultType f where
    ResultType (a -> b) = b
  
  -- Type family to extract the argument type of a function
type family ArgType f where
    ArgType (a -> b) = a
  
  -- Type family to check if two types match and provide a custom error if not
type family CheckTypesMatch2 a b :: Constraint where
    CheckTypesMatch2 a a = ()  -- Types match, constraint satisfied
    CheckTypesMatch2 a b = TypeError
      ( 'Text "Type mismatch between 'inFnc' and 'outFnc'."
      :$$: 'Text "The output type of 'inFnc' is "
      :<>: 'ShowType a
      :$$: 'Text "But the input type of 'outFnc' is "
      :<>: 'ShowType b
      )
  
  -- Define the IntOut data type with the custom type error using the same type parameter
data IntOut2 where
    IntOut2 ::
      ( ResultType inFnc ~ inTyp,
        ArgType outFnc ~ inTyp,
        CheckTypesMatch2 (ResultType inFnc) (ArgType outFnc)
      ) =>
      { inFnc  :: inFnc,
        outFnc :: outFnc
      } -> IntOut2

-- Correct functions where types match
goodInFnc :: () -> Int
goodInFnc () = 42

goodOutFnc :: Int -> ()
goodOutFnc _ = ()

-- This should compile successfully
goodIntOut2 :: IntOut2
goodIntOut2 = IntOut2
  { inFnc  = goodInFnc,
    outFnc = goodOutFnc
  }

-- Incorrect functions where types do not match
badInFnc :: () -> Int
badInFnc () = 42

badOutFnc :: String -> ()
badOutFnc _ = ()

-- This will trigger the custom compiler error
badIntOut2 :: IntOut2
badIntOut2 = IntOut2
  { inFnc  = badInFnc,
    outFnc = badOutFnc
  }



-- Define the IntOut data type with the custom type error using the same type parameter
data IntOut3 where
  IntOut3 ::
    ( ResultType inFnc ~ inTyp,
      ArgType outFnc ~ inTyp,
      CheckTypesMatch (ResultType inFnc) (ArgType outFnc)
    ) =>
    { inFnc  :: inFnc,
      outFnc :: outFnc
    } -> IntOut3

-}