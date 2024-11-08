module Internal.LogQueries where

import CoreUtils
import Internal.Logging( NodeType(..),
      HookPos(..),
      Log(..),
      FullLog(..),
      FLog)

isSetup :: NodeType -> Bool
isSetup = \case
  Hook _ Setup -> True
  _ -> False

isBefore :: NodeType -> Bool
isBefore = \case
  Hook _ Before -> True
  _ -> False

isTeardown :: NodeType -> Bool
isTeardown = \case
  Hook _ Teardown -> True
  _ -> False


nodeFrequency :: NodeType -> Hz
nodeFrequency = \case
  Hook hz _ -> hz
  -- an individual test is always run once
  Test -> Once

isParentFailure = isParentFailureWith (const True)
isParentFailure :: FLog l a -> Bool

isParentFailureWith :: (NodeType -> Bool) -> FLog l a -> Bool
isParentFailureWith nodeTypePredicate l =
  l.log & \case
    ParentFailure {nodeType = s} -> nodeTypePredicate s
    _ -> False

isPreHookFailure :: FLog l a -> Bool
isPreHookFailure l = 
   l.log & \case
      Failure {nodeType} -> isSetup nodeType || isBefore nodeType
      _ -> False

isOnceHookParentFailure :: FLog l a -> Bool
isOnceHookParentFailure = isParentFailureWith onceHook

isHookParentFailure :: FLog l a -> Bool
isHookParentFailure = isParentFailureWith isHook

isTest :: NodeType -> Bool
isTest = \case
  Test {} -> True
  _ -> False

isTestParentFailure :: FLog l a -> Bool
isTestParentFailure l = l.log & \case
  ParentFailure {nodeType = s} -> isTest s
  _ -> False

isTestLogItem :: FLog l a -> Bool
isTestLogItem li = (isTest <$> getSuiteEvent li) == Just True

isTestEventOrTestParentFailure :: FLog l a -> Bool
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
onceSuiteEvent = (== Once) . nodeFrequency

isChildless :: FLog l a -> Bool
isChildless =
  threadEventToBool
    ( \case
        Hook _hz pos -> pos == After
        Test {} -> True
    )

suitEvntToBool :: (NodeType -> Bool) -> Maybe NodeType -> Bool
suitEvntToBool = maybe False

threadEventToBool :: (NodeType -> Bool) -> FLog l a -> Bool
threadEventToBool prd = suitEvntToBool prd . getSuiteEvent

nodeTypeMatch :: (NodeType -> Bool) -> FLog l a -> Bool
nodeTypeMatch p l = l.log & \case
  StartExecution {} -> False
  InitialisationFailure {} -> False
  NodeLog {} -> False
  node -> p node.nodeType


startEndNodeMatch :: (NodeType -> Bool) -> FLog l a -> Bool
startEndNodeMatch p l = l.log & \case
  StartExecution {} -> False
  InitialisationFailure {} -> False
  Failure {} -> False
  ParentFailure {} -> False
  NodeLog {} -> False
  EndExecution {} -> False
  FilterLog {} -> False
  SuiteInitFailure {} -> False
  Start {nodeType} -> p nodeType
  End {nodeType} -> p nodeType


isStart :: FLog a b -> Bool
isStart l = l.log  & \case
  Start {} -> True
  _ -> False

isEnd :: FLog a b -> Bool
isEnd l = l.log  & \case
  End {} -> True
  _ -> False

suiteEventOrParentFailureSuiteEvent :: FLog a b -> Maybe NodeType
suiteEventOrParentFailureSuiteEvent l = 
  l.log  & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  StartExecution {} -> Nothing
  Start {nodeType = s} -> Just s
  End {nodeType = s} -> Just s
  InitialisationFailure {} -> Nothing
  Failure {} -> Nothing
  ParentFailure {nodeType = s} -> Just s
  NodeLog {} -> Nothing
  EndExecution {} -> Nothing

getSuiteEvent :: FLog a b -> Maybe NodeType
getSuiteEvent l = l.log  & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  Start {nodeType} -> Just nodeType
  End {nodeType} -> Just nodeType
  ParentFailure {nodeType} -> Just nodeType
  InitialisationFailure {nodeType} -> Just nodeType
  Failure {nodeType} -> Just nodeType
  StartExecution {} -> Nothing
  NodeLog {} -> Nothing
  EndExecution {} -> Nothing

getHookInfo :: FLog a b -> Maybe (Hz, HookPos)
getHookInfo t =
  getSuiteEvent t >>= \case
    Hook hz pos -> Just (hz, pos)
    Test {} -> Nothing

startOrParentFailure :: FLog l a -> Bool
startOrParentFailure l = l.log  & \case
  FilterLog {} -> False
  SuiteInitFailure {} -> False
  StartExecution {} -> False
  EndExecution {} -> False
  NodeLog {} -> False
  Failure {} -> False
  InitialisationFailure {} -> False
  -- event will either have a start or be
  -- represented by a parent failure if skipped
  ParentFailure {} -> True
  Start {} -> True
  End {} -> False

startSuiteEventLoc :: FLog l a -> Maybe l
startSuiteEventLoc l = l.log & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  StartExecution {} -> Nothing
  EndExecution {} -> Nothing
  NodeLog {} -> Nothing
  Failure {} -> Nothing
  InitialisationFailure {} -> Nothing
  -- event will either have a start or be
  -- represented by a parent failure if skipped
  ParentFailure {loc} -> Just loc
  Start {loc} -> Just loc
  End {} -> Nothing
