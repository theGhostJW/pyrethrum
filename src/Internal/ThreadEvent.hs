module Internal.ThreadEvent where

import BasePrelude (read)
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Filter (FilterResult)
import UnliftIO.Concurrent qualified as C
import CoreUtils



data HookPos = Before | After | Setup | Teardown deriving (Show, Eq, Ord)

data Hz = Once | Thread | Each deriving (Show, Eq, Ord)

data NodeType
  = Hook Hz HookPos
  | Test
  deriving (Show, Eq, Ord)

isSetup :: NodeType -> Bool
isSetup = \case
  Hook _ Setup -> True
  _ -> False

isTeardown :: NodeType -> Bool
isTeardown = \case
  Hook _ Teardown -> True
  _ -> False

evtTypeToFrequency :: NodeType -> Hz
evtTypeToFrequency = \case
  Hook hz _ -> hz
  -- an individual test is always run once
  Test -> Once

isSuiteEventFailureWith :: (NodeType -> Bool) -> ThreadEvent l a -> Bool
isSuiteEventFailureWith evntPredicate = \case
  ParentFailure {nodeType = s} -> evntPredicate s
  _ -> False

isOnceHookParentFailure :: ThreadEvent l a -> Bool
isOnceHookParentFailure = isSuiteEventFailureWith onceHook

isHookParentFailure :: ThreadEvent l a -> Bool
isHookParentFailure = isSuiteEventFailureWith isHook

isTest :: NodeType -> Bool
isTest = \case
  Test {} -> True
  _ -> False

isTestParentFailure :: ThreadEvent l a -> Bool
isTestParentFailure = \case
  ParentFailure {nodeType = s} -> isTest s
  _ -> False

isTestLogItem :: ThreadEvent l a -> Bool
isTestLogItem li = (isTest <$> getSuiteEvent li) == Just True

isTestEventOrTestParentFailure :: ThreadEvent l a -> Bool
isTestEventOrTestParentFailure te = isTestParentFailure te || isTestLogItem te

isHook :: NodeType -> Bool
isHook = \case
  Hook {} -> True
  _ -> False

hookWithHz :: Hz -> NodeType -> Bool
hookWithHz hz = \case
  Hook hz' _ -> hz == hz'
  Test -> False

onceHook :: NodeType -> Bool
onceHook = hookWithHz Once

threadHook :: NodeType -> Bool
threadHook = hookWithHz Thread

onceSuiteEvent :: NodeType -> Bool
onceSuiteEvent = (== Once) . evtTypeToFrequency

isChildless :: ThreadEvent l a -> Bool
isChildless =
  threadEventToBool
    ( \case
        Hook _hz pos -> pos == After
        Test {} -> True
    )

suitEvntToBool :: (NodeType -> Bool) -> Maybe NodeType -> Bool
suitEvntToBool = maybe False

threadEventToBool :: (NodeType -> Bool) -> ThreadEvent l a -> Bool
threadEventToBool prd = suitEvntToBool prd . getSuiteEvent

hasSuiteEvent :: (NodeType -> Bool) -> ThreadEvent l a -> Bool
hasSuiteEvent p l = case l of
  StartExecution {} -> False
  Failure {} -> False
  ParentFailure {} -> False
  NodeEvent {} -> False
  EndExecution {} -> False
  _ -> p l.nodeType

isStart :: ThreadEvent a b -> Bool
isStart = \case
  Start {} -> True
  _ -> False

isEnd :: ThreadEvent a b -> Bool
isEnd = \case
  End {} -> True
  _ -> False

suiteEventOrParentFailureSuiteEvent :: ThreadEvent a b -> Maybe NodeType
suiteEventOrParentFailureSuiteEvent = \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  StartExecution {} -> Nothing
  Start {nodeType = s} -> Just s
  End {nodeType = s} -> Just s
  Failure {} -> Nothing
  ParentFailure {nodeType = s} -> Just s
  NodeEvent {} -> Nothing
  EndExecution {} -> Nothing

getSuiteEvent :: ThreadEvent a b -> Maybe NodeType
getSuiteEvent = \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  Start {nodeType} -> Just nodeType
  End {nodeType} -> Just nodeType
  ParentFailure {nodeType} -> Just nodeType
  Failure {nodeType} -> Just nodeType
  StartExecution {} -> Nothing
  NodeEvent {} -> Nothing
  EndExecution {} -> Nothing

getHookInfo :: ThreadEvent a b -> Maybe (Hz, HookPos)
getHookInfo t =
  getSuiteEvent t >>= \case
    Hook hz pos -> Just (hz, pos)
    Test {} -> Nothing

data ThreadEvent l a
  = FilterLog
      { idx :: Int,
        threadId :: ThreadId,
        filterResuts :: FilterResult Text
      }
  | SuiteInitFailure
      { idx :: Int,
        reason :: Text
      }
  | StartExecution
      { idx :: Int,
        threadId :: ThreadId
      }
  | Start
      { idx :: Int,
        threadId :: ThreadId,
        nodeType :: NodeType,
        loc :: l
      }
  | End
      { idx :: Int,
        threadId :: ThreadId,
        nodeType :: NodeType,
        loc :: l
      }
  | Failure
      { idx :: Int,
        threadId :: ThreadId,
        nodeType :: NodeType,
        loc :: l,
        exception :: PException
      }
  | ParentFailure
      { idx :: Int,
        threadId :: ThreadId,
        loc :: l,
        nodeType :: NodeType,
        failLoc :: l,
        failSuiteEvent :: NodeType
      }
  | NodeEvent
      { idx :: Int,
        threadId :: ThreadId,
        event :: a
      }
  | EndExecution
      { idx :: Int,
        threadId :: ThreadId
      }
  deriving (Show)

startOrParentFailure :: ThreadEvent l a -> Bool
startOrParentFailure te = case te of
  FilterLog {} -> False
  SuiteInitFailure {} -> False
  StartExecution {} -> False
  EndExecution {} -> False
  NodeEvent {} -> False
  Failure {} -> False
  -- event will either have a start or be
  -- represented by a parent failure if skipped
  ParentFailure {} -> True
  Start {} -> True
  End {} -> False

startSuiteEventLoc :: ThreadEvent l a -> Maybe l
startSuiteEventLoc te = case te of
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  StartExecution {} -> Nothing
  EndExecution {} -> Nothing
  NodeEvent {} -> Nothing
  Failure {} -> Nothing
  -- event will either have a start or be
  -- represented by a parent failure if skipped
  ParentFailure {loc} -> Just loc
  Start {loc} -> Just loc
  End {} -> Nothing

$(deriveJSON defaultOptions ''Hz)
$(deriveJSON defaultOptions ''HookPos)
$(deriveJSON defaultOptions ''NodeType)