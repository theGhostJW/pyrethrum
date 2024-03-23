module Internal.ThreadEvent where

import BasePrelude (read)
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import UnliftIO.Concurrent qualified as C

-- needs a separate module to avoid field name conflicts
-- can move when this is no longer a limitation of ghc

newtype PException = PException {displayText :: [Text]} deriving (Show, Eq, Ord)
type ThreadId = Int

-- ThreadId 5 -> 5
mkThreadId :: C.ThreadId -> ThreadId
mkThreadId = read . drop 9 . show

data HookPos = Before | After | Setup | Teardown deriving (Show, Eq, Ord)

data Hz = Once | Thread | Each deriving (Show, Eq, Ord)

data SuiteEvent
    = Hook Hz HookPos
    | Test
    deriving (Show, Eq, Ord)
    
isSetup :: SuiteEvent -> Bool
isSetup = \case
    Hook _ Setup -> True
    _ -> False

isTeardown :: SuiteEvent -> Bool
isTeardown = \case
    Hook _ Teardown -> True
    _ -> False

evtTypeToFrequency :: SuiteEvent -> Hz
evtTypeToFrequency = \case
    Hook hz _ -> hz
    -- an individual test is always run once
    Test -> Once

isHookParentFailure :: ThreadEvent l a -> Bool
isHookParentFailure = \case
    ParentFailure{suiteEvent = s} -> isHook s
    _ -> False

isTest :: SuiteEvent -> Bool
isTest = \case
    Test{} -> True
    _ -> False

isTestParentFailure :: ThreadEvent l a -> Bool
isTestParentFailure = \case
    ParentFailure{suiteEvent = s} -> isTest s
    _ -> False

isTestLogItem :: ThreadEvent l a -> Bool
isTestLogItem li = (isTest <$> getSuiteEvent li) == Just True

isTestEventOrTestParentFailure :: ThreadEvent l a -> Bool
isTestEventOrTestParentFailure te = isTestParentFailure te || isTestLogItem te

isHook :: SuiteEvent -> Bool
isHook = \case
    Hook{} -> True
    _ -> False

hookWithHz :: Hz -> SuiteEvent -> Bool
hookWithHz hz = \case
    Hook hz' _ -> hz == hz'
    Test -> False

onceHook :: SuiteEvent -> Bool
onceHook = hookWithHz Once

threadHook :: SuiteEvent -> Bool
threadHook = hookWithHz Thread

onceSuiteEvent :: SuiteEvent -> Bool
onceSuiteEvent = (== Once) . evtTypeToFrequency

hasSuiteEvent :: (SuiteEvent -> Bool) -> ThreadEvent l a -> Bool
hasSuiteEvent p l = case l of
    StartExecution{} -> False
    Failure{} -> False
    ParentFailure{} -> False
    ApEvent{} -> False
    EndExecution{} -> False
    _ -> p l.suiteEvent

isStart :: ThreadEvent a b -> Bool
isStart = \case
    Start{} -> True
    _ -> False

isEnd :: ThreadEvent a b -> Bool
isEnd = \case
    End{} -> True
    _ -> False

suiteEventOrParentFailureSuiteEvent :: ThreadEvent a b -> Maybe SuiteEvent
suiteEventOrParentFailureSuiteEvent = \case
    StartExecution{} -> Nothing
    Start{suiteEvent = s} -> Just s
    End{suiteEvent = s} -> Just s
    Failure{} -> Nothing
    ParentFailure{suiteEvent = s} -> Just s
    ApEvent{} -> Nothing
    EndExecution{} -> Nothing

getSuiteEvent :: ThreadEvent a b -> Maybe SuiteEvent
getSuiteEvent = \case
    Start{suiteEvent} -> Just suiteEvent
    End{suiteEvent} -> Just suiteEvent
    ParentFailure{suiteEvent} -> Just suiteEvent
    _ -> Nothing

data ThreadEvent l a
    = StartExecution
        { idx :: Int
        , threadId :: ThreadId
        }
    | Start
        { idx :: Int
        , threadId :: ThreadId
        , suiteEvent :: SuiteEvent
        , loc :: l
        }
    | End
        { idx :: Int
        , threadId :: ThreadId
        , suiteEvent :: SuiteEvent
        , loc :: l
        }
    | Failure
        { idx :: Int
        , threadId :: ThreadId
        , suiteEvent :: SuiteEvent
        , loc :: l
        , exception :: PException
        }
    | ParentFailure
        { idx :: Int
        , threadId :: ThreadId
        , loc :: l
        , suiteEvent :: SuiteEvent
        , failLoc :: l
        , failSuiteEvent :: SuiteEvent
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

startSuiteEventLoc :: ThreadEvent l a -> Maybe l
startSuiteEventLoc te = case te of
    StartExecution{} -> Nothing
    EndExecution{} -> Nothing
    ApEvent{} -> Nothing
    Failure{} -> Nothing
    -- event will either have a start or be
    -- represented by a parent failure if skipped
    ParentFailure{loc} -> Just loc
    Start{loc} -> Just loc
    End{} -> Nothing

$(deriveJSON defaultOptions ''Hz)
$(deriveJSON defaultOptions ''HookPos)
$(deriveJSON defaultOptions ''SuiteEvent)
$(deriveToJSON defaultOptions ''PException)
