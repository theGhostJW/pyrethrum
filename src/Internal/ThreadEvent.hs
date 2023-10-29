module Internal.ThreadEvent where
  
import Data.Aeson.TH (deriveToJSON, deriveJSON, defaultOptions)

-- needs a separate module to avoid field name conflicts
-- can move when this is no longer a limitation of ghc

newtype PException = PException {displayText :: [Text]} deriving (Show, Eq, Ord)
newtype SThreadId = SThreadId {display :: Text} deriving (Show, Generic, Eq, Ord)

data FrameworkEventType
  = OnceHook
  | OnceHookRelease
  | ThreadHook
  | ThreadHookRelease
  | FixtureOnceHook
  | FixtureOnceHookRelease
  | FixtureThreadHook
  | FixtureThreadHookRelease
  | TestHook
  | TestHookRelease
  | Group
  | Fixture
  | Test
  deriving (Show, Eq, Ord, Enum)


data ThreadEvent l a
  = StartExecution
      { idx :: Int
      , threadId :: SThreadId
      }
  | Start
      { idx :: Int
      , threadId :: SThreadId
      , eventType :: FrameworkEventType
      , loc :: l
      }
  | End
      { idx :: Int
      , threadId :: SThreadId
      , eventType :: FrameworkEventType
      , loc :: l
      }
  | Failure
      { idx :: Int
      , threadId :: SThreadId
      , msg :: Text
      , exception :: PException
      , parentEventType :: FrameworkEventType
      , loc :: l
      }
  | ParentFailure
      { idx :: Int
      , threadId :: SThreadId
      , exception :: PException
      , loc :: l
      , fEventType :: FrameworkEventType
      , parentLoc :: l
      , parentEventType :: FrameworkEventType
      }
  | ApEvent
      { idx :: Int
      , threadId :: SThreadId
      , event :: a
      }
  | EndExecution
      { idx :: Int
      , threadId :: SThreadId
      }
  deriving (Show)

$(deriveToJSON defaultOptions ''SThreadId)
$(deriveJSON defaultOptions ''FrameworkEventType)
$(deriveToJSON defaultOptions ''PException)
