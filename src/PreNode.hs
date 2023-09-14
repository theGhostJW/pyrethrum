module PreNode where

data Cardinality = Once | Thread | Each deriving (Show, Eq)

data AbstractPreNode rc tc m i where
  Before ::
    { title :: Text
    , cardinality :: Cardinality
    , action :: rc -> m o
    , subNodes :: [AbstractPreNode rc tc m o]
    } ->
    AbstractPreNode rc tc m i
  Before' ::
    { title :: Text
    , cardinality :: Cardinality
    , childAction :: i -> rc -> m o
    , subNodes :: [AbstractPreNode rc tc m o]
    } ->
    AbstractPreNode rc tc m i
  After ::
    { title :: Text
    , cardinality :: Cardinality
    , before :: AbstractPreNode rc tc m i
    , after :: rc -> m ()
    } ->
    AbstractPreNode rc tc m i
  Resource ::
    { title :: Text
    , cardinality :: Cardinality
    , setUp :: rc -> m a
    , tearDown :: a -> m ()
    } ->
    AbstractPreNode rc tc m i
  Test ::
    { config :: tc
    , items :: [TestItem rc tc m i]
    } ->
    AbstractPreNode rc tc m ()

data TestItem rc tc m i = TestItem
  { id :: Int
  , title :: Text
  , test :: rc -> i -> m ()
  , chkText :: Text
  }