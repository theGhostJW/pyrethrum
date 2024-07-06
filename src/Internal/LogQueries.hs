module Internal.LogQueries where

import CoreUtils
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Filter (FilterResult)
import Internal.Logging

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

isSuiteEventFailureWith :: (NodeType -> Bool) -> Log l a -> Bool
isSuiteEventFailureWith evntPredicate l =
  evnt l & \case
    ParentFailure {nodeType = s} -> evntPredicate s
    _ -> False

isOnceHookParentFailure :: Log l a -> Bool
isOnceHookParentFailure = isSuiteEventFailureWith onceHook

isHookParentFailure :: Log l a -> Bool
isHookParentFailure = isSuiteEventFailureWith isHook

isTest :: NodeType -> Bool
isTest = \case
  Test {} -> True
  _ -> False

isTestParentFailure :: Log l a -> Bool
isTestParentFailure l = evnt l & \case
  ParentFailure {nodeType = s} -> isTest s
  _ -> False

isTestLogItem :: Log l a -> Bool
isTestLogItem li = (isTest <$> getSuiteEvent li) == Just True

isTestEventOrTestParentFailure :: Log l a -> Bool
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

isChildless :: Log l a -> Bool
isChildless =
  threadEventToBool
    ( \case
        Hook _hz pos -> pos == After
        Test {} -> True
    )

suitEvntToBool :: (NodeType -> Bool) -> Maybe NodeType -> Bool
suitEvntToBool = maybe False

threadEventToBool :: (NodeType -> Bool) -> Log l a -> Bool
threadEventToBool prd = suitEvntToBool prd . getSuiteEvent

hasSuiteEvent :: (NodeType -> Bool) -> Log l a -> Bool
hasSuiteEvent p l = evnt l & \case
  StartExecution {} -> False
  Failure {} -> False
  ParentFailure {} -> False
  NodeEvent {} -> False
  EndExecution {} -> False
  FilterLog {} -> False
  SuiteInitFailure {} -> False
  Start {nodeType} -> p nodeType
  End {nodeType} -> p nodeType




isStart :: Log a b -> Bool
isStart l = l.event & \case
  Start {} -> True
  _ -> False

isEnd :: Log a b -> Bool
isEnd l = l.event & \case
  End {} -> True
  _ -> False

suiteEventOrParentFailureSuiteEvent :: Log a b -> Maybe NodeType
suiteEventOrParentFailureSuiteEvent l = 
  l.event & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  StartExecution {} -> Nothing
  Start {nodeType = s} -> Just s
  End {nodeType = s} -> Just s
  Failure {} -> Nothing
  ParentFailure {nodeType = s} -> Just s
  NodeEvent {} -> Nothing
  EndExecution {} -> Nothing

getSuiteEvent :: Log a b -> Maybe NodeType
getSuiteEvent l = l.event & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  Start {nodeType} -> Just nodeType
  End {nodeType} -> Just nodeType
  ParentFailure {nodeType} -> Just nodeType
  Failure {nodeType} -> Just nodeType
  StartExecution {} -> Nothing
  NodeEvent {} -> Nothing
  EndExecution {} -> Nothing

getHookInfo :: Log a b -> Maybe (Hz, HookPos)
getHookInfo t =
  getSuiteEvent t >>= \case
    Hook hz pos -> Just (hz, pos)
    Test {} -> Nothing

startOrParentFailure :: Log l a -> Bool
startOrParentFailure l = l.event & \case
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

startSuiteEventLoc :: Log l a -> Maybe l
startSuiteEventLoc l = l.event & \case
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
