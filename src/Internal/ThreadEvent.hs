module Internal.ThreadEvent where

import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)

-- needs a separate module to avoid field name conflicts
-- can move when this is no longer a limitation of ghc

newtype PException = PException {displayText :: [Text]} deriving (Show, Eq, Ord)
newtype SThreadId = SThreadId {display :: Text} deriving (Show, Generic, Eq, Ord)

data HookPos = Before | After | SetUp | TearDown deriving (Show, Eq, Ord)

data Frequency = Once | Thread | Each deriving (Show, Eq, Ord)
data EventType
    = Hook Frequency HookPos
    | Test
    deriving (Show, Eq, Ord)

data ThreadEvent l a
    = StartExecution
        { idx :: Int
        , threadId :: SThreadId
        }
    | Start
        { idx :: Int
        , threadId :: SThreadId
        , eventType :: EventType
        , loc :: l
        }
    | End
        { idx :: Int
        , threadId :: SThreadId
        , eventType :: EventType
        , loc :: l
        }
    | Failure
        { idx :: Int
        , threadId :: SThreadId
        , msg :: Text
        , exception :: PException
        , loc :: l
        }
    | ParentFailure
        { idx :: Int
        , threadId :: SThreadId
        , loc :: l
        , eventType :: EventType
        , parentLoc :: l
        , parentEventType :: EventType
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
$(deriveJSON defaultOptions ''Frequency)
$(deriveJSON defaultOptions ''HookPos)
$(deriveJSON defaultOptions ''EventType)
$(deriveToJSON defaultOptions ''PException)
