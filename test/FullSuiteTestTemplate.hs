module FullSuiteTestTemplate where

import DSL.Internal.NodeLog (Path (..))
import Data.Map.Strict qualified as Map
import CoreUtils (Hz (..))
import Internal.Logging (HookPos (..), NodeType (Hook))
import Internal.Logging qualified as L
import Prelude hiding (All, id)
data Spec = Spec {delay :: Int, result :: Result}
  deriving (Read, Show, Eq)
data Result
  = Pass
  | Fail
  deriving (Ord, Eq, Read, Show)

-- |Defines when a spec is generated, on building the template (Preload) or when the test is run (Runtime)
data SpecGen
  = 
    {-|
     A spec is genrated when loading the template and expected failures can be verified
     by comparing the pre-generated template results to actual results. The execution of
     these specs (specifically thread hooks), however, requires STM which could mask
     synchronisation bugs in testing.
    -}
    Preload
     {-|
     A spec is generated when the test is run but the result will be non-deterministic and
     hence cannot be verified. On the other hand, the execution of these specs does not require
     any syncronisation and will not mask synchronisation bugs.
    -}
  | Runtime
  deriving (Ord, Eq, Read, Show)

isPreload :: SpecGen -> Bool
isPreload = (==) Preload
data ManySpec
  = All Spec
  | PassProb
      { genStrategy :: SpecGen
      , passPcnt :: Int8
      , minDelay :: Int
      , maxDelay :: Int
      }
  deriving (Show, Eq)

data EventPath = MkEventPath
  { path :: Path
  , nodeType :: NodeType
  }
  deriving (Show, Eq, Ord)

{- Given a list of templates, return a map of each event path to its expected preceeding
parent event path
-}
expectedParentPrecedingEvents :: [Template] -> Map EventPath EventPath
expectedParentPrecedingEvents = expectedSuiteEvntMap templateBeforeEvnt

expectedParentSubsequentEvents :: [Template] -> Map EventPath EventPath
expectedParentSubsequentEvents = expectedSuiteEvntMap templateAfterEvnt

expectedSuiteEvntMap :: (Template -> Maybe NodeType) -> [Template] -> Map EventPath EventPath
expectedSuiteEvntMap getSuiteEvnt =
  foldl' (priorMap Nothing) Map.empty
 where
  priorMap :: Maybe EventPath -> Map EventPath EventPath -> Template -> Map EventPath EventPath
  priorMap mParentEvnt accMap t =
    case t of
      FullSuiteTestTemplate.Fixture{tests} ->
        mParentEvnt
          & maybe
            accMap
            ( \parent ->
                foldl'
                  ( \accMap' testItem ->
                      let
                        thisEvtPath = MkEventPath (testItemPath testItem) L.Test
                       in
                        Map.insert thisEvtPath parent accMap'
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

templateBeforeEvnt :: Template -> Maybe NodeType
templateBeforeEvnt t =
  case t of
    FullSuiteTestTemplate.Fixture{} -> Nothing
    OnceAfter{} -> Nothing
    ThreadAfter{} -> Nothing
    EachAfter{} -> Nothing
    _ -> Just $ case t of
      OnceBefore{} -> Hook Once Before
      OnceAround{} -> Hook Once Setup
      ThreadBefore{} -> Hook Thread Before
      ThreadAround{} -> Hook Thread Setup
      EachBefore{} -> Hook Each Before
      EachAround{} -> Hook Each Setup

templateAfterEvnt :: Template -> Maybe NodeType
templateAfterEvnt t =
  case t of
    FullSuiteTestTemplate.Fixture{} -> Nothing
    OnceBefore{} -> Nothing
    ThreadBefore{} -> Nothing
    EachBefore{} -> Nothing
    _ -> Just $ case t of
      OnceAfter{} -> Hook Once After
      OnceAround{} -> Hook Once Teardown
      ThreadAfter{} -> Hook Thread After
      ThreadAround{} -> Hook Thread Teardown
      EachAfter{} -> Hook Each After
      EachAround{} -> Hook Each Teardown

emittedHooks :: Template -> [NodeType]
emittedHooks = \case
  FullSuiteTestTemplate.Fixture{} -> []
  OnceBefore{} -> [oh Before]
  OnceAfter{} -> [oh After]
  OnceAround{} -> [oh Setup, oh Teardown]
  ThreadBefore{} -> [th Before]
  ThreadAfter{} -> [th After]
  ThreadAround{} -> [th Setup, th Teardown]
  EachBefore{} -> [eh Before]
  EachAfter{} -> [eh After]
  EachAround{} -> [eh Setup, eh Teardown]
 where
  oh = Hook Once
  th = Hook Thread
  eh = Hook Each

data Template
  = OnceBefore
      { path :: Path
      , spec :: Spec
      , subNodes :: [Template]
      }
  | OnceAfter
      { path :: Path
      , spec :: Spec
      , subNodes :: [Template]
      }
  | OnceAround
      { path :: Path
      , setupSpec :: Spec
      , teardownSpec :: Spec
      , subNodes :: [Template]
      }
  | ThreadBefore
      { path :: Path
      , threadSpec :: ManySpec
      , subNodes :: [Template]
      }
  | ThreadAfter
      { path :: Path
      , threadSpec :: ManySpec
      , subNodes :: [Template]
      }
  | ThreadAround
      { path :: Path
      , setupThreadSpec :: ManySpec
      , teardownThreadSpec :: ManySpec
      , subNodes :: [Template]
      }
  | EachBefore
      { path :: Path
      , eachSpec :: ManySpec
      , subNodes :: [Template]
      }
  | EachAfter
      { path :: Path
      , eachSpec :: ManySpec
      , subNodes :: [Template]
      }
  | EachAround
      { path :: Path
      , eachSetupSpec :: ManySpec
      , eachTeardownSpec :: ManySpec
      , subNodes :: [Template]
      }
  | Fixture
      { path :: Path
      , tests :: [TestItem]
      }
  deriving (Show, Eq)

countTests :: Template -> Int
countTests t = case t of
  FullSuiteTestTemplate.Fixture{tests} -> length tests
  _ -> sum $ countTests <$> t.subNodes

data TemplatePath = MkTemplatePath
  { template :: Template
  , path :: Path
  , nodeType :: NodeType
  , evntSpec :: ManySpec
  }
  deriving (Show, Eq)

testItemPath :: TestItem -> Path
testItemPath TestItem{..} = TestPath{..}

eventPaths :: Template -> [TemplatePath]
eventPaths t = case t of
  Fixture{tests} ->
    (\ti -> MkTemplatePath t (testItemPath ti) L.Test $ All ti.spec) <$> tests
  _ ->
    let
      recurse = concatMap eventPaths t.subNodes
      mkEvnt f p = MkTemplatePath t t.path (Hook f p)
     in
      case t of
        OnceBefore{spec} ->
          mkEvnt Once Before (All spec) : recurse
        OnceAfter{spec} ->
          mkEvnt Once After (All spec) : recurse
        OnceAround
          { setupSpec
          , teardownSpec
          } ->
            mkEvnt Once Setup (All setupSpec)
              : mkEvnt Once Teardown (All teardownSpec)
              : recurse
        ThreadBefore{threadSpec} ->
          mkEvnt Thread Before threadSpec : recurse
        ThreadAfter{threadSpec} ->
          mkEvnt Thread After threadSpec : recurse
        ThreadAround
          { setupThreadSpec
          , teardownThreadSpec
          } ->
            mkEvnt Thread Setup setupThreadSpec
              : mkEvnt Thread Teardown teardownThreadSpec
              : recurse
        EachBefore{eachSpec} ->
          mkEvnt Each Before eachSpec : recurse
        EachAfter{eachSpec} ->
          mkEvnt Each After eachSpec : recurse
        EachAround
          { eachSetupSpec
          , eachTeardownSpec
          } ->
            mkEvnt Each Setup eachSetupSpec
              : mkEvnt Each Teardown eachTeardownSpec
              : recurse

data TestItem = TestItem
  { id :: Int
  , title :: Text
  , spec :: Spec
  }
  deriving (Show, Eq)
