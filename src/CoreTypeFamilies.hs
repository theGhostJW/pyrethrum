module CoreTypeFamilies where

import GHC.TypeLits (TypeError)
import GHC.TypeError (ErrorMessage(..))

type family DataSourceType dataSource where
    DataSourceType (rc -> ds i) = i

type family ActionInputType action where
    ActionInputType (rc -> i -> m as) = i

type family ActionInputType' action where
    ActionInputType' (hi -> rc -> i -> m as) = i

type family DataSourceMatchesAction ds ai :: Constraint where
    DataSourceMatchesAction ds ds = ()  -- Types match, constraint satisfied
    DataSourceMatchesAction ds ai = TypeError
      ( 
      'Text "Pyrethrum Fixture Type Error ~ dataSource / action mismatch"
        :$$: 'Text "The type of the `dataSource` elements does not match the input type of the `action` function."
        :$$: 'Text ""
        :$$: 'Text "The data elements of the `dataSource` are the input to the `action`"
        :$$: 'Text "so their types must match."
        :$$: 'Text ""
        :$$: 'Text "The 'dataSource' elements for this fixture are of type: `"
        :<>: 'ShowType ds
        :<>: 'Text "`."
        :$$: 'Text "This does not match input type of the `action`, which is: `"
        :<>: 'ShowType ai
        :<>: 'Text "`."
        :$$: 'Text "Either: "
        :$$: 'Text "1. change the `action` input type to: "
        :<>: 'Text "`"
        :<>: 'ShowType ds
        :<>: 'Text "`"
        :$$: 'Text "     so the action input type matches the `dataSource` elements"
        :$$: 'Text "Or"
        :$$: 'Text "2. change the `dataSource` type to: "
        :<>: 'Text "`"
        :<>: 'ShowType ai
        :<>: 'Text "`"
        :$$: 'Text "     so the dataSource elements match the input for the `action`."
        :$$: 'Text ""
      )