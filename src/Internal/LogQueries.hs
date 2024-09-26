module Internal.LogQueries where

import CoreUtils
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

isSuiteEventFailureWith :: (NodeType -> Bool) -> LogOLD l a -> Bool
isSuiteEventFailureWith evntPredicate l =
  evnt l & \case
    ParentFailure {nodeType = s} -> evntPredicate s
    _ -> False

isOnceHookParentFailure :: LogOLD l a -> Bool
isOnceHookParentFailure = isSuiteEventFailureWith onceHook

isHookParentFailure :: LogOLD l a -> Bool
isHookParentFailure = isSuiteEventFailureWith isHook

isTest :: NodeType -> Bool
isTest = \case
  Test {} -> True
  _ -> False

isTestParentFailure :: LogOLD l a -> Bool
isTestParentFailure l = evnt l & \case
  ParentFailure {nodeType = s} -> isTest s
  _ -> False

isTestLogItem :: LogOLD l a -> Bool
isTestLogItem li = (isTest <$> getSuiteEvent li) == Just True

isTestEventOrTestParentFailure :: LogOLD l a -> Bool
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

isChildless :: LogOLD l a -> Bool
isChildless =
  threadEventToBool
    ( \case
        Hook _hz pos -> pos == After
        Test {} -> True
    )

suitEvntToBool :: (NodeType -> Bool) -> Maybe NodeType -> Bool
suitEvntToBool = maybe False

threadEventToBool :: (NodeType -> Bool) -> LogOLD l a -> Bool
threadEventToBool prd = suitEvntToBool prd . getSuiteEvent

startEndNodeMatch :: (NodeType -> Bool) -> LogOLD l a -> Bool
startEndNodeMatch p l = evnt l & \case
  StartExecution {} -> False
  Failure {} -> False
  ParentFailure {} -> False
  NodeLog {} -> False
  EndExecution {} -> False
  FilterLog {} -> False
  SuiteInitFailure {} -> False
  Start {nodeType} -> p nodeType
  End {nodeType} -> p nodeType


isStart :: LogOLD a b -> Bool
isStart l = evnt l  & \case
  Start {} -> True
  _ -> False

isEnd :: LogOLD a b -> Bool
isEnd l = evnt l  & \case
  End {} -> True
  _ -> False

suiteEventOrParentFailureSuiteEvent :: LogOLD a b -> Maybe NodeType
suiteEventOrParentFailureSuiteEvent l = 
  evnt l  & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  StartExecution {} -> Nothing
  Start {nodeType = s} -> Just s
  End {nodeType = s} -> Just s
  Failure {} -> Nothing
  ParentFailure {nodeType = s} -> Just s
  NodeLog {} -> Nothing
  EndExecution {} -> Nothing

getSuiteEvent :: LogOLD a b -> Maybe NodeType
getSuiteEvent l = evnt l  & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  Start {nodeType} -> Just nodeType
  End {nodeType} -> Just nodeType
  ParentFailure {nodeType} -> Just nodeType
  Failure {nodeType} -> Just nodeType
  StartExecution {} -> Nothing
  NodeLog {} -> Nothing
  EndExecution {} -> Nothing

getHookInfo :: LogOLD a b -> Maybe (Hz, HookPos)
getHookInfo t =
  getSuiteEvent t >>= \case
    Hook hz pos -> Just (hz, pos)
    Test {} -> Nothing

startOrParentFailure :: LogOLD l a -> Bool
startOrParentFailure l = evnt l  & \case
  FilterLog {} -> False
  SuiteInitFailure {} -> False
  StartExecution {} -> False
  EndExecution {} -> False
  NodeLog {} -> False
  Failure {} -> False
  -- event will either have a start or be
  -- represented by a parent failure if skipped
  ParentFailure {} -> True
  Start {} -> True
  End {} -> False

startSuiteEventLoc :: LogOLD l a -> Maybe l
startSuiteEventLoc l = evnt l & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  StartExecution {} -> Nothing
  EndExecution {} -> Nothing
  NodeLog {} -> Nothing
  Failure {} -> Nothing
  -- event will either have a start or be
  -- represented by a parent failure if skipped
  ParentFailure {loc} -> Just loc
  Start {loc} -> Just loc
  End {} -> Nothing
