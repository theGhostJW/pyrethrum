{-# LANGUAGE UndecidableInstances #-}

module CoreTypeFamilies where

import GHC.TypeLits (TypeError)
import GHC.TypeError (ErrorMessage(..))
import GHC.Records (HasField)
import Data.Aeson (ToJSON)
import Check (Checks)

type family DataSourceType dataSource where
    DataSourceType (rc -> ds i) = i

type family ActionInType action where
    ActionInType (rc -> i -> m as) = i

type family ActionInType' action where
    ActionInType' (hi -> rc -> i -> m as) = i

type family ActionOutType action where
    ActionOutType (rc -> i -> m as) = as

type family ActionOutType' action where
    ActionOutType' (hi -> rc -> i -> m as) = as

type family ValStateType item where
    ValStateType (Item i vs) = vs

type family ParserInType parser where
    ParserInType (as -> Either l vs) = as

type family ParserOutType parser where
    ParserOutType (as -> Either l vs) = vs

type family DataSourceMatchesAction ds ai :: Constraint where
    DataSourceMatchesAction ds ds = ()  -- Types match, constraint satisfied
    DataSourceMatchesAction ds ai = TypeError
      ( 
      'Text "Pyrethrum Fixture Type Error"
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

type DataSourceMatch ds ai = DataSourceMatchesAction (DataSourceType ds) (ActionInType ai)

type HasTitle a = HasField "title" a Text

type HasId a = HasField "id" a Int

class (HasTitle a, Show a, ToJSON a, Eq a) => Config a

type Item i vs = (HasTitle i, HasId i, HasField "checks" i (Checks vs), Show i, Show vs) 


