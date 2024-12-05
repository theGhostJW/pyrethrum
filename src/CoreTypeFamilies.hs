{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}

module CoreTypeFamilies where

import GHC.TypeLits (TypeError)
import GHC.TypeError as TE (ErrorMessage(..)) 
import GHC.Records (HasField)
import Data.Aeson (ToJSON)
import Check (Checks)
import Effectful (Eff)

type ErrorHeader = 'Text "Pyrethrum Fixture Type Error"

data DataSource i vs where 
  Items :: HasTestFields i vs => [i] -> DataSource i vs
  Property :: HasTestFields i vs => i -> DataSource i vs

type DataElm i vs = (HasTestFields i vs) => i

data EgItem = EgItem { id :: Int, title :: Text} deriving (Show, Read)


type family DataSourceType dataSource where
    DataSourceType (rc -> DataSource i vs) = i

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
    ValStateType (rc -> DataSource i vs) = vs

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
        :$$: 'Text "As the action return value is the input for the parser"
        :<>: 'Text " their types must match."
        :$$: 'Text "Either: "
        :$$: 'Text "1. Change the action return type to: "
        :<>: 'ShowType pIn
        :$$: 'Text "     so the action return type matches the input for the parser."
        :$$: 'Text "Or"
        :$$: 'Text "2. Change the parser input type to: "
        :<>: 'ShowType aOut
        :$$: 'Text "     so the parser input type matches the action return type"
      )

type family ParserMatchesValState pOut vs :: Constraint where
    ParserMatchesValState pOut pOut = ()  -- Types match, constraint satisfied
    ParserMatchesValState pOut vs = TypeError
      ( 
        ErrorHeader
        :$$: 'Text "❌ ~ parser -> checks (on dataSource items)"
        :$$: 'Text "The parser returns elements of type: "
        :<>: 'ShowType pOut
        :$$: 'Text "    but the checks on the DataSource expect an input of type: "
        :<>: 'ShowType vs
        :$$: 'Text "As the value returned by the parser forms the input for checks"
        :<>: 'Text " their types must match."
        :$$: 'Text "Either: "
        :$$: 'Text "1. Change the parser return type to: "
        :<>: 'ShowType vs
        :$$: 'Text "     so the parser return type matches dataSource checks input."
        :$$: 'Text "Or"
        :$$: 'Text "2. Change the dataSource checks input type to: "
        :<>: 'ShowType pOut
        :$$: 'Text "     so the checks input type matches the parser return type"
      )

type family ActionMatchesValState pOut vs :: Constraint where
    ActionMatchesValState pOut pOut = ()  -- Types match, constraint satisfied
    ActionMatchesValState pOut vs = TypeError
      (
        ErrorHeader
        :$$: 'Text "❌ ~ action -> checks (on dataSource items)"
        :$$: 'Text "The action returns elements of type: "
        :<>: 'ShowType pOut
        :$$: 'Text "    but the checks on the DataSource expect an input of type: "
        :<>: 'ShowType vs
        :$$: 'Text "In `Direct` fixtures the action return value is the input for checks,"
        :<>: 'Text " so their types must match."
        :$$: 'Text "Either: "
        :$$: 'Text "1. Change the action return type to: "
        :<>: 'ShowType vs
        :$$: 'Text "     so the action return value matches dataSource checks input."
        :$$: 'Text "Or"
        :$$: 'Text "2. Change the dataSource checks input type to: "
        :<>: 'ShowType pOut
        :$$: 'Text "     so the checks input type matches the parser return type"
      )


type FixtureTypeCheckFull action parser dataSource vs = 
    ( 
      DataSourceMatchesAction (DataSourceType dataSource) (ActionInType action)
    , ActionMatchesParser (ActionOutType action) (ParserInType parser)
    , ParserMatchesValState (ParserOutType parser) (ValStateType dataSource)
    )

type FixtureTypeCheckDirect action dataSource  = 
    ( DataSourceMatchesAction (DataSourceType dataSource) (ActionInType action)
    , ActionMatchesValState (ActionOutType action) (ValStateType dataSource)
    )

type HasTitle a = HasField "title" a Text

type HasId a = HasField "id" a Int

class (HasTitle a, Show a, ToJSON a, Eq a) => Config a

class   (
  HasField "title" i Text, 
  HasField "id" i Int, 
  HasField "checks" i (Checks vs), 
  Show i, 
  Show vs) => HasTestFields i vs

instance (HasField "title" i Text, HasField "id" i Int, HasField "checks" i (Checks vs), Show i, Show vs) => HasTestFields i vs
