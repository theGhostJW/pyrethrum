module PreNode where

import qualified Core as C

data Frequency = Once | Thread | Each deriving (Show, Eq)

data PreNode m i where
  Before ::
    { title :: Text
    , frequency :: Frequency
    , action :: i -> m o
    , subNodes :: [PreNode m o]
    } ->
    PreNode m i 
  After ::
    { title' :: Text
    , frequency' :: Frequency
    , subNode :: PreNode m ()
    , after :: m ()
    } ->
    PreNode m ()
  Resource ::
    { title :: Text
    , frequency :: Frequency
    , setUp :: i -> m o
    , subNodes' :: [PreNode m o]
    , tearDown :: o -> m ()
    } ->
    PreNode m i
  Test ::
    { tests :: [i -> m ()]
    } -> PreNode m i



data TestItem rc tc m i = TestItem
  { id :: Int
  , title :: Text
  , test :: rc -> i -> m ()
  , chkText :: Text
  }

-- type Suite rc tc effs = [SuiteElement rc tc effs ()]

-- data SuiteElement rc tc effs i where
--   Hook ::
--     { path :: Path
--     , hook :: Hook rc tc effs loc i o
--     , subNodes :: [SuiteElement rc tc effs o]
--     } ->
--     SuiteElement rc tc effs i 
--   Test ::
--     { path :: Path
--     , test :: Test rc tc effs i
--     } ->
--     SuiteElement rc tc effs i 