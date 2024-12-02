{-# LANGUAGE UndecidableInstances #-}

module CoreTypeFamilies where

import GHC.TypeLits (TypeError, Symbol)
import GHC.TypeError as TE (ErrorMessage(..)) 
import GHC.Records (HasField)
import Data.Aeson (ToJSON)
import Check (Checks)
import Text.Read (Lexeme(Symbol))

type ErrorHeader = 'Text "Pyrethrum Fixture Type Error"

type family DataSourceType dataSource where
    DataSourceType (rc -> ds i) = i

type family ActionInType action where
    ActionInType (rc -> i -> m as) = i
    ActionInType (hi -> rc -> i -> m as) = i

type family ActionOutType action where
    ActionOutType (rc -> i -> m as) = as
    ActionOutType (hi -> rc -> i -> m as) = as

type family ParserInType parser where
    ParserInType (as -> Either l vs) = as

type family ParserOutType parser where
    ParserOutType (as -> Either l vs) = vs

type family ValStateType item where
    ValStateType (Item i vs) = vs

type family DataSourceMatchesAction ds ai :: Constraint where
    DataSourceMatchesAction ds ds = ()  -- Types match, constraint satisfied
    DataSourceMatchesAction ds ai = TypeError
      ( 
        ErrorHeader
        :$$: 'Text "The dataSource returns elements of type: "
        :<>: 'ShowType ds
        :$$: 'Text "    but the action expects an input of type: "
        :<>: 'ShowType ai
        :$$: 'Text "As dataSource elements form the input for the action"
        :<>: 'Text " their types must match."
        :$$: 'Text "Either: "
        :$$: 'Text "1. change the action input type to: "
        :<>: 'ShowType ds
        :$$: 'Text "     so the action input type matches the dataSource elements"
        :$$: 'Text "Or"
        :$$: 'Text "2. change the dataSource element type to: "
        :<>: 'ShowType ai
        :$$: 'Text "     so the dataSource elements match the input for the action."
      )

type family ActionMatchesParser aOut pIn :: Constraint where
    ActionMatchesParser aOut aOut = ()  -- Types match, constraint satisfied
    ActionMatchesParser aOut pIn = TypeError
      ( 
        ErrorHeader
        :$$: 'Text "The action returns elements of type: "
        :<>: 'ShowType aOut
        :$$: 'Text "    but the parser expects an input of type: "
        :<>: 'ShowType pIn
        :$$: 'Text "As the action output is the input for the parser"
        :<>: 'Text " their types must match."
        :$$: 'Text "Either: "
        :$$: 'Text "1. change the parser input type to: "
        :<>: 'ShowType aOut
        :$$: 'Text "     so the parser input type matches the action output"
        :$$: 'Text "Or"
        :$$: 'Text "2. change the action output type to: "
        :<>: 'ShowType pIn
        :$$: 'Text "     so the action output matches the input for the parser."
      )

type family ParserMatchesValState pOut vs :: Constraint where
    ParserMatchesValState pOut pOut = ()  -- Types match, constraint satisfied
    ParserMatchesValState pOut vs = TypeError
      ( 
        ErrorHeader
        :$$: 'Text "The parser returns elements of type: "
        :<>: 'ShowType pOut
        :$$: 'Text "    but the checks on the DataSource an input of type: "
        :<>: 'ShowType vs
        :$$: 'Text "As the parser output is the input for the value state"
        :<>: 'Text " their types must match."
        :$$: 'Text "Either: "
        :$$: 'Text "1. change the value state input type to: "
        :<>: 'ShowType pOut
        :$$: 'Text "     so the value state input type matches the parser output"
        :$$: 'Text "Or"
        :$$: 'Text "2. change the parser output type to: "
        :<>: 'ShowType vs
        :$$: 'Text "     so the parser output matches the input for the value state."
      )

type DataSourceMatch ds ai = DataSourceMatchesAction (DataSourceType ds) (ActionInType ai)

type FixtureTypeCheckFull action parser dataSource  = 
    ( DataSourceMatchesAction (DataSourceType dataSource) (ActionInType action)
    , ActionMatchesParser (ActionOutType action) (ParserInType parser)
    , ParserMatchesValState (ParserOutType parser) (ValStateType (DataSourceType dataSource))
    )

type FixtureTypeCheckDirect action dataSource  = 
    ( DataSourceMatchesAction (DataSourceType dataSource) (ActionInType action)
    , ParserMatchesValState (ActionOutType action) (ValStateType (DataSourceType dataSource))
    )

type HasTitle a = HasField "title" a Text

type HasId a = HasField "id" a Int

class (HasTitle a, Show a, ToJSON a, Eq a) => Config a

type Item i vs = (HasTitle i, HasId i, HasField "checks" i (Checks vs), Show i, Show vs) 


