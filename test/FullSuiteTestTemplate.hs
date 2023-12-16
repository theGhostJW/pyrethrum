module FullSuiteTestTemplate where
import DSL.Internal.ApEvent (Path)

data Template
  = OnceBefore
      { path :: Path
      , delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | OnceAfter
      { path :: Path
      , delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | OnceAround
      { path :: Path
      , delay :: Int
      , passSetup :: Bool
      , passTeardown :: Bool
      , subNodes :: [Template]
      }
  | ThreadBefore
      { path :: Path
      , delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | ThreadAfter
      { path :: Path
      , delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | ThreadAround
      { path :: Path
      , delay :: Int
      , passSetup :: Bool
      , passTeardown :: Bool
      , subNodes :: [Template]
      }
  | EachBefore
      { path :: Path
      , delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | EachAfter
      { path :: Path
      , delay :: Int
      , pass :: Bool
      , subNodes :: [Template]
      }
  | EachAround
      { path :: Path
      , delay :: Int
      , passSetup :: Bool
      , passTeardown :: Bool
      , subNodes :: [Template]
      }
  | Test
      { path :: Path
      , testItems :: [TestItem]
      }
  deriving (Show, Eq)


data TestItem = TestItem
  { id :: Int
  , title :: Text
  , delay :: Int
  , pass :: Bool
  }
  deriving (Show, Eq)
