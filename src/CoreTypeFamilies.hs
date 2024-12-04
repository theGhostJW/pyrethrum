{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}

module CoreTypeFamilies where

import GHC.TypeLits (TypeError)
import GHC.TypeError as TE (ErrorMessage(..)) 
import GHC.Records (HasField)
import Data.Aeson (ToJSON)
import Check (Checks, chk)
import Effectful (Eff)
import GHC.Show as SH (Show(..)) 

type ErrorHeader = 'Text "Pyrethrum Fixture Type Error"

data DataSource i vs = Items [DataElm i vs] | Property (DataElm i vs) 

type DataElm i vs = (Item i vs) => i

data EgItem = EgItem { title :: Text, checks :: Checks Int } deriving (Show, Read)

ds :: DataSource EgItem Int
ds = Items [
  EgItem "one"  $ chk "fail all" (const False), 
  EgItem "two" $ chk "fail all" (const False)
  ]
-- instance Show (DataSource i vs) where
--     show (Items xs) = SH.show xs
--     show (Property x) = SH.show x






type family DataSourceType dataSource where
    DataSourceType (rc -> DataSource vs i) = i

type family ActionInType action where
    ActionInType (hi -> rc -> i -> m as) = i
    ActionInType (rc -> i -> m as) = i

type family ActionOutType action where
    -- Note must be a concrete monad not a type variable
    -- this does not work
    -- ActionOutType (rc -> i -> m as) = as
    ActionOutType (rc -> i -> Eff es as) = as
    ActionOutType (hi -> rc -> i -> Eff es as) = as

type family ParserInType parser where
    ParserInType (as -> Either l vs) = as

type family ParserOutType parser where
    ParserOutType (as -> Either l vs) = vs

type family ValStateType dataSource where
    ValStateType (rc -> DataSource vs i) = vs

-- class ValStateType2 dataSource where
-- instance ValStateType2 dataSource where
--     ValStateType2 (forall rc i. rc -> DataSource i) = i

type family DataSourceMatchesAction ds ai :: Constraint where
    DataSourceMatchesAction ds ds = ()  -- Types match, constraint satisfied
    DataSourceMatchesAction ds ai = TypeError
      ( 
        ErrorHeader
        :$$: 'Text "❌ ~ dataSource -> action"
        :$$: 'Text "The dataSource returns elements of type: "
        :<>: 'ShowType ds
        :$$: 'Text "    but the action expects an input of type: "
        :<>: 'ShowType ai
        :$$: 'Text "As dataSource elements form the input for the action"
        :<>: 'Text " their types must match."
        :$$: 'Text "Either: "
        :$$: 'Text "1. Change the dataSource element type to: "
        :<>: 'ShowType ai
        :$$: 'Text "     so the dataSource elements match the input for the action."
        :$$: 'Text "Or"
        :$$: 'Text "2. Change the action input type to: "
        :<>: 'ShowType ds
        :$$: 'Text "     so the action input type matches the dataSource elements"
      )


type family ActionMatchesParser aOut pIn :: Constraint where
    ActionMatchesParser aOut aOut = ()  -- Types match, constraint satisfied
    ActionMatchesParser aOut pIn = TypeError
      ( 
        ErrorHeader
        :$$: 'Text "❌ ~ action -> parser"
        :$$: 'Text "The action returns elements of type: "
        :<>: 'ShowType aOut
        :$$: 'Text "    but the parser expects an input of type: "
        :<>: 'ShowType pIn
        :$$: 'Text "As the action output is the input for the parser"
        :<>: 'Text " their types must match."
        :$$: 'Text "Either: "
        :$$: 'Text "1. Change the action output type to: "
        :<>: 'ShowType pIn
        :$$: 'Text "     so the action output matches the input for the parser."
        :$$: 'Text "Or"
        :$$: 'Text "2. Change the parser input type to: "
        :<>: 'ShowType aOut
        :$$: 'Text "     so the parser input type matches the action output"
      )

type family ParserMatchesValState pOut vs :: Constraint where
    ParserMatchesValState pOut pOut = ()  -- Types match, constraint satisfied
    ParserMatchesValState pOut vs = TypeError
      ( 
        ErrorHeader
        :$$: 'Text "❌ ~ parser -> dataSource checks (ValState)"
        :$$: 'Text "The parser returns elements of type: "
        :<>: 'ShowType pOut
        :$$: 'Text "    but the checks on the DataSource an input of type: "
        :<>: 'ShowType vs
        :$$: 'Text "As the parser output is the input for the dataSource checks input (ValState))"
        :<>: 'Text " their types must match."
        :$$: 'Text "Either: "
        :$$: 'Text "1. Change the parser output type to: "
        :<>: 'ShowType vs
        :$$: 'Text "     so the parser output matches dataSource checks input (ValState)."
        :$$: 'Text "Or"
        :$$: 'Text "2. Change the dataSource checks (ValState) input type to: "
        :<>: 'ShowType pOut
        :$$: 'Text "     so the value state input type matches the parser output"
      )


type FixtureTypeCheckFull action parser dataSource vs = 
    ( 
      DataSourceMatchesAction (DataSourceType dataSource) (ActionInType action)
    , ActionMatchesParser (ActionOutType action) (ParserInType parser)
    , ParserMatchesValState (ParserOutType parser) (ValStateType dataSource)
    )

type FixtureTypeCheckDirect action dataSource  = 
    ( DataSourceMatchesAction (DataSourceType dataSource) (ActionInType action)
    -- reword this when it works needs a separate rule with different wording caus there is no parser
    , ParserMatchesValState (ActionOutType action) (ValStateType dataSource)
    )

type HasTitle a = HasField "title" a Text

type HasId a = HasField "id" a Int

class (HasTitle a, Show a, ToJSON a, Eq a) => Config a

type Item i vs = (HasTitle i, HasId i, HasField "checks" i (Checks vs), Show i, Show vs) 
type HasChecks i vs = HasField "checks" i (Checks vs)

{-
------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

import GHC.Records (HasField)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Kind (Constraint)

-- Original type synonym (commented out)
-- type Item i vs = (HasTitle i, HasId i, HasField "checks" i (Checks vs), Show i, Show vs)

-- Convert to type class with functional dependency
class (HasTitle i, HasId i, HasField "checks" i (Checks vs), Show i, Show vs) => Item i vs | i -> vs

-- Provide an instance (if necessary)
instance (HasTitle i, HasId i, HasField "checks" i (Checks vs), Show i, Show vs) => Item i vs

type ValStateType i = VsFromItem i

-- Helper type family to get 'vs' from 'i' using the 'Item' class
type family VsFromItem i where
  VsFromItem i = VsFromItemClass i

-- Define a type family that leverages the functional dependency
type family VsFromItemClass i where
  VsFromItemClass i = Vs -- 'Vs' is determined by the functional dependency 'i -> vs' in 'Item i vs'

-- Since 'Item i vs' implies 'i -> vs', we can define:
type ValStateType i = VsFor i

-- We need a way to refer to 'vs' given 'i'
-- We can use a type class to achieve this:
class Item i vs => HasValStateType i vs | i -> vs

-- Provide an instance
instance Item i vs => HasValStateType i vs

-- Now, 'ValStateType i' can be defined as:
type ValStateType i = VsFor i

type family VsFor i where
  -- We can't extract 'vs' directly in a type family, but we can use 'HasValStateType' in constraints
  -- So, we use 'ValStateType i' in constraints where necessary
    {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE TypeOperators #-}
  {-# LANGUAGE DataKinds #-}
  
  import GHC.TypeLits (TypeError, ErrorMessage(..))
  
  -- Type-level 'If' construct
  type family IfThenElse (cond :: Bool) (trueBranch :: k) (falseBranch :: k) :: k where
    IfThenElse 'True trueBranch _ = trueBranch
    IfThenElse 'False _ falseBranch = falseBranch
  
  -- Custom type error when parser output doesn't match 'vs' from 'i'
  type family ParserMatchesValState parserOut vs :: Constraint where
    ParserMatchesValState parserOut vs =
      IfThenElse (parserOut == vs)
        (() :: Constraint)
        (TypeError
          ( 'Text "Pyrethrum Fixture Type Error:"
          ':$$: 'Text "❌ Mismatch between parser output and DataSource checks (ValState)"
          ':$$: 'Text "Parser returns: " ':<>: 'ShowType parserOut
          ':$$: 'Text "But the 'checks' field in DataSource elements expects: " ':<>: 'ShowType vs
          ':$$: 'Text ""
          ':$$: 'Text "These types must match because the parser output is used as input for the checks."
          ':$$: 'Text "Possible fixes:"
          ':$$: 'Text "1. Change the parser output type to: " ':<>: 'ShowType vs
          ':$$: 'Text "   so it matches the expected ValState."
          ':$$: 'Text "2. Change the 'checks' field type in DataSource elements to: " ':<>: 'ShowType parserOut
          ':$$: 'Text "   so it matches the parser output."
          )
        )        
        
        
        type FixtureTypeCheckFull action parser dataSource =
          ( DataSourceMatchesAction (DataSourceType dataSource) (ActionInType action)
          , ActionMatchesParser (ActionOutType action) (ParserInType parser)
          , HasValStateType (DataSourceType dataSource) vs  -- Ensure we have 'vs' from 'i'
          , ParserMatchesValState (ParserOutType parser) vs  -- Compare 'parser' output with 'vs'
          )

data Fixture a where
  Full ::
    ( dataSource ~ (RunConfig -> DataSource i)
    , action ~ (RunConfig -> i -> Action as)
    , parser ~ (as -> Either ParseException parserOut)
    , FixtureTypeCheckFull action parser dataSource
    , Item i vs
    , HasValStateType i vs  -- Ensure we can get 'vs' from 'i'
    , Show as
    ) =>
    { config     :: FixtureConfig
    , action     :: action
    , parse      :: parser
    , dataSource :: dataSource
    } ->
    Fixture a
-}