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

isTeardownNode :: NodeType -> Bool
isTeardownNode = \case
  Hook _ Teardown -> True
  _ -> False

isTeardown :: FLog l a -> Bool
isTeardown = nodeTypeMatch isTeardownNode

nodeFrequency :: NodeType -> Hz
nodeFrequency = \case
  Hook hz _ -> hz
  -- an individual test is always run once
  Test -> Once

isBypassed :: FLog l a -> Bool
isBypassed = isBypassedWith (const True)

isBypassedWith :: (NodeType -> Bool) -> FLog l a -> Bool
isBypassedWith nodeTypePredicate l =
  l.log & \case
    Bypassed {nodeType = s} -> nodeTypePredicate s
    _ -> False

isPreHookFailure :: FLog l a -> Bool
isPreHookFailure l = 
   l.log & \case
      Failure {nodeType} -> isSetup nodeType || isBefore nodeType
      _ -> False

isOnceHookBypassed :: FLog l a -> Bool
isOnceHookBypassed = isBypassedWith onceHook

isOnceHook :: FLog l a -> Bool
isOnceHook = nodeTypeMatch onceHook 

isEachHook :: FLog l a -> Bool
isEachHook = nodeTypeMatch eachHook 

isHookBypassed :: FLog l a -> Bool
isHookBypassed = isBypassedWith isHook

isTest :: NodeType -> Bool
isTest = \case
  Test {} -> True
  _ -> False

isTestBypassed :: FLog l a -> Bool
isTestBypassed l = l.log & \case
  Bypassed {nodeType = s} -> isTest s
  _ -> False

isTestLogItem :: FLog l a -> Bool
isTestLogItem li = (isTest <$> getNodeType li) == Just True

isTestEventOrTestBypassed :: FLog l a -> Bool
isTestEventOrTestBypassed te = isTestBypassed te || isTestLogItem te

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

eachHook :: NodeType -> Bool
eachHook = hookWithHz Each

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
threadEventToBool prd = suitEvntToBool prd . getNodeType

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
  Bypassed {} -> False
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

suiteEventOrBypassedSuiteEvent :: FLog a b -> Maybe NodeType
suiteEventOrBypassedSuiteEvent l = 
  l.log  & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  StartExecution {} -> Nothing
  Start {nodeType = s} -> Just s
  End {nodeType = s} -> Just s
  InitialisationFailure {} -> Nothing
  Failure {} -> Nothing
  Bypassed {nodeType = s} -> Just s
  NodeLog {} -> Nothing
  EndExecution {} -> Nothing

getNodeType :: FLog a b -> Maybe NodeType
getNodeType l = l.log  & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  Start {nodeType} -> Just nodeType
  End {nodeType} -> Just nodeType
  Bypassed {nodeType} -> Just nodeType
  InitialisationFailure {nodeType} -> Just nodeType
  Failure {nodeType} -> Just nodeType
  StartExecution {} -> Nothing
  NodeLog {} -> Nothing
  EndExecution {} -> Nothing

getHookInfo :: FLog a b -> Maybe (Hz, HookPos)
getHookInfo t =
  getNodeType t >>= \case
    Hook hz pos -> Just (hz, pos)
    Test {} -> Nothing

startOrBypassed :: FLog l a -> Bool
startOrBypassed l = l.log  & \case
  FilterLog {} -> False
  SuiteInitFailure {} -> False
  StartExecution {} -> False
  EndExecution {} -> False
  NodeLog {} -> False
  Failure {} -> False
  InitialisationFailure {} -> False
  -- event will either have a start or be
  -- represented by a parent failure if skipped
  Bypassed {} -> True
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
  Bypassed {loc} -> Just loc
  Start {loc} -> Just loc
  End {} -> Nothing

getLoc :: FLog l a -> Maybe l
getLoc l = l.log & \case
  FilterLog {} -> Nothing
  SuiteInitFailure {} -> Nothing
  StartExecution {} -> Nothing
  EndExecution {} -> Nothing
  NodeLog {} -> Nothing 
  l' -> Just l'.loc

