module FullSuiteTestTemplate where

import CoreUtils (Hz (..))
import DSL.Internal.NodeLog (Path (..))
import Data.Map.Strict qualified as Map
import Internal.Logging (HookPos (..), NodeType (Hook))
import Internal.Logging qualified as L
import Prelude hiding (All, id)

data Spec = Spec {delay :: Int, directive :: Directive}
  deriving (Read, Show, Eq)

data Directive
  = Pass
  | Fail
  | PassThroughFail
  deriving (Ord, Eq, Read, Show)

-- | Defines when a spec is generated, on building the template (Preload) or when the test is run (Runtime)
data SpecGen
  = -- |
    --     A spec is genrated when loading the template and expected failures can be verified
    --     by comparing the pre-generated template results to actual results. The execution of
    --     these specs (specifically thread hooks), however, requires STM which could mask
    --     synchronisation bugs in testing.
    Preload
  | -- |
    --     A spec is generated when the test is run but the result will be non-deterministic and
    --     hence cannot be verified. On the other hand, the execution of these specs does not require
    --     any syncronisation and will not mask synchronisation bugs.
    Runtime
  deriving (Ord, Eq, Read, Show)

isPreload :: SpecGen -> Bool
isPreload = (==) Preload

data NSpec
  = All {spec :: Spec}
  | PassProb
      { genStrategy :: SpecGen,
        passPcnt :: Int8,
        -- % of failures that are pass through
        hookPassThroughErrPcnt :: Int8,
        minDelay :: Int,
        maxDelay :: Int
      }
  deriving (Show, Eq)

data EventPath = MkEventPath
  { path :: Path,
    nodeType :: NodeType
  }
  deriving (Show, Eq, Ord)

{- Given a list of templates, return a map of each event path to its expected preceeding
parent event path
-}
expectedParentPrecedingEvents :: [Template] -> Map EventPath EventPath
expectedParentPrecedingEvents = expectedSuiteEvntMap templateBeforeNodeType

expectedParentSubsequentEvents :: [Template] -> Map EventPath EventPath
expectedParentSubsequentEvents = expectedSuiteEvntMap templateAfterNodeType

testItemPath :: TestItem -> Path
testItemPath TestItem {..} = TestPath {..}

expectedSuiteEvntMap :: (Template -> Maybe NodeType) -> [Template] -> Map EventPath EventPath
expectedSuiteEvntMap getSuiteEvnt =
  foldl' (priorMap Nothing) Map.empty
  where
    priorMap :: Maybe EventPath -> Map EventPath EventPath -> Template -> Map EventPath EventPath
    priorMap mParentEvnt accMap t =
      case t of
        FullSuiteTestTemplate.Fixture {tests} ->
          mParentEvnt
            & maybe
              accMap
              ( \parent ->
                  foldl'
                    ( \accMap' testItem ->
                        let thisEvtPath = MkEventPath (testItemPath testItem) L.Test
                         in Map.insert thisEvtPath parent accMap'
                    )
                    accMap
                    tests
              )
        _ ->
          foldl' (priorMap nxtB4Evnt) nxtMap t.subNodes
          where
            thisTemplateEvntPaths = MkEventPath t.path <$> emittedHooks t
            nxtB4Evnt = (MkEventPath t.path <$> getSuiteEvnt t) <|> mParentEvnt
            nxtMap =
              mParentEvnt
                & maybe
                  accMap
                  ( \p ->
                      foldl'
                        (\accMap' thisEvtPath -> Map.insert thisEvtPath p accMap')
                        accMap
                        thisTemplateEvntPaths
                  )

templateBeforeNodeType :: Template -> Maybe NodeType
templateBeforeNodeType t =
  case t of
    FullSuiteTestTemplate.Fixture {} -> Nothing
    OnceAfter {} -> Nothing
    ThreadAfter {} -> Nothing
    EachAfter {} -> Nothing
    _ -> Just $ case t of
      OnceBefore {} -> Hook Once Before
      OnceAround {} -> Hook Once Setup
      ThreadBefore {} -> Hook Thread Before
      ThreadAround {} -> Hook Thread Setup
      EachBefore {} -> Hook Each Before
      EachAround {} -> Hook Each Setup

templateAfterNodeType :: Template -> Maybe NodeType
templateAfterNodeType t =
  case t of
    FullSuiteTestTemplate.Fixture {} -> Nothing
    OnceBefore {} -> Nothing
    ThreadBefore {} -> Nothing
    EachBefore {} -> Nothing
    _ -> Just $ case t of
      OnceAfter {} -> Hook Once After
      OnceAround {} -> Hook Once Teardown
      ThreadAfter {} -> Hook Thread After
      ThreadAround {} -> Hook Thread Teardown
      EachAfter {} -> Hook Each After
      EachAround {} -> Hook Each Teardown

emittedHooks :: Template -> [NodeType]
emittedHooks = \case
  FullSuiteTestTemplate.Fixture {} -> []
  OnceBefore {} -> [oh Before]
  OnceAfter {} -> [oh After]
  OnceAround {} -> [oh Setup, oh Teardown]
  ThreadBefore {} -> [th Before]
  ThreadAfter {} -> [th After]
  ThreadAround {} -> [th Setup, th Teardown]
  EachBefore {} -> [eh Before]
  EachAfter {} -> [eh After]
  EachAround {} -> [eh Setup, eh Teardown]
  where
    oh = Hook Once
    th = Hook Thread
    eh = Hook Each

data Template
  = OnceBefore
      { path :: Path,
        spec :: Spec,
        subNodes :: [Template]
      }
  | OnceAfter
      { path :: Path,
        spec :: Spec,
        subNodes :: [Template]
      }
  | OnceAround
      { path :: Path,
        setupSpec :: Spec,
        teardownSpec :: Spec,
        subNodes :: [Template]
      }
  | ThreadBefore
      { path :: Path,
        threadSpec :: NSpec,
        subNodes :: [Template]
      }
  | ThreadAfter
      { path :: Path,
        threadSpec :: NSpec,
        subNodes :: [Template]
      }
  | ThreadAround
      { path :: Path,
        setupThreadSpec :: NSpec,
        teardownThreadSpec :: NSpec,
        subNodes :: [Template]
      }
  | EachBefore
      { path :: Path,
        eachSpec :: NSpec,
        subNodes :: [Template]
      }
  | EachAfter
      { path :: Path,
        eachSpec :: NSpec,
        subNodes :: [Template]
      }
  | EachAround
      { path :: Path,
        eachSetupSpec :: NSpec,
        eachTeardownSpec :: NSpec,
        subNodes :: [Template]
      }
  | Fixture
      { path :: Path,
        tests :: [TestItem]
      }
  deriving (Show, Eq)

testEventPath :: TestItem -> EventPath
testEventPath = flip MkEventPath L.Test . testItemPath

allEventPaths :: Template -> [EventPath]
allEventPaths t = case t of
  Fixture {tests} -> testEventPath <$> tests
  _ ->
    catMaybes (MkEventPath t.path <<$>> [templateBeforeNodeType t, templateAfterNodeType t])
      <> concatMap allEventPaths t.subNodes

-- templateAfterNodeType

--     MkEventPath t.path : concatMap allEventPaths t.subNodes

countTests :: Template -> Int
countTests t = case t of
  FullSuiteTestTemplate.Fixture {tests} -> length tests
  _ -> sum $ countTests <$> t.subNodes

data TestItem = TestItem
  { id :: Int,
    title :: Text,
    spec :: Spec
  }
  deriving (Show, Eq)
