module Internal.ThreadEvent where

import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import qualified UnliftIO.Concurrent as C
import BasePrelude (read)

-- needs a separate module to avoid field name conflicts
-- can move when this is no longer a limitation of ghc

newtype PException = PException {displayText :: [Text]} deriving (Show, Eq, Ord)
type ThreadId = Int

-- ThreadId 5 -> 5
mkThreadId :: C.ThreadId -> ThreadId
mkThreadId = read . drop 9 . show

data HookPos = Before | After | Setup | Teardown deriving (Show, Eq, Ord)

data Frequency = Once | Thread | Each deriving (Show, Eq, Ord)
data EventType
    = Hook Frequency HookPos
    | Test
    deriving (Show, Eq, Ord)

 
evtTypeToFrequency :: EventType -> Frequency
evtTypeToFrequency = \case
    Hook f _ -> f
    Test -> Each

onceEventType :: EventType -> Bool
onceEventType = (== Once) . evtTypeToFrequency

isStart :: ThreadEvent a b -> Bool
isStart = \case
    Start{} -> True
    _ -> False

data ThreadEvent l a
    = StartExecution
        { idx :: Int
        , threadId :: ThreadId
        }
    | Start
        { idx :: Int
        , threadId :: ThreadId
        , eventType :: EventType
        , loc :: l
        }
    | End
        { idx :: Int
        , threadId :: ThreadId
        , eventType :: EventType
        , loc :: l
        }
    | Failure
        { idx :: Int
        , threadId :: ThreadId
        , exception :: PException
        , loc :: l
        }
    | ParentFailure
        { idx :: Int
        , threadId :: ThreadId
        , loc :: l
        , eventType :: EventType
        , failLoc :: l
        , failEventType :: EventType
        }
    | ApEvent
        { idx :: Int
        , threadId :: ThreadId
        , event :: a
        }
    | EndExecution
        { idx :: Int
        , threadId :: ThreadId
        }
    deriving (Show)

$(deriveJSON defaultOptions ''Frequency)
$(deriveJSON defaultOptions ''HookPos)
$(deriveJSON defaultOptions ''EventType)
$(deriveToJSON defaultOptions ''PException)
