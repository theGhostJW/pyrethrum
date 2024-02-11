module FullSuiteTestTemplate where

import DSL.Internal.ApEvent (Path (..))
import Data.Map.Strict qualified as Map
import Internal.ThreadEvent (HookPos (..), Hz (..), SuiteEvent (..))
import Internal.ThreadEvent qualified as TE
import PyrethrumExtras (debug', (?))
import Prelude hiding (All, id)

data Spec = Spec {delay :: Int, result :: Result}
  deriving (Show, Eq)
data Result
  = Pass
  | Fail
  deriving (Show, Eq)

data ThreadSpec
  = All Spec
  | Some [Spec]
  deriving (Show, Eq)

specs :: Int -> ThreadSpec ->  [Spec]
specs maxThreads ts  = case ts of
  All s -> replicate maxThreads s
  Some ss -> 
    length ss == maxThreads 
        ? ss 
        $ error "ThreadSpec: mismatched number of specs"



data SuiteEventPath = SuiteEventPath
  { path :: Path
  , suiteEvent :: SuiteEvent
  }
  deriving (Show, Eq, Ord)

{- Given a list of templates, return a map of each event path to its expected preceeding
parent event path
-}
expectedParentPrecedingEvents :: [Template] -> Map SuiteEventPath SuiteEventPath
expectedParentPrecedingEvents = expectedSuiteEvntMap templateBeforeEvnt

expectedParentSubsequentEvents :: [Template] -> Map SuiteEventPath SuiteEventPath
expectedParentSubsequentEvents = expectedSuiteEvntMap templateAfterEvnt

expectedSuiteEvntMap :: (Template -> Maybe SuiteEvent) -> [Template] -> Map SuiteEventPath SuiteEventPath
expectedSuiteEvntMap getSuiteEvnt ts =
  foldl' (priorMap Nothing) Map.empty ts
 where
  priorMap :: Maybe SuiteEventPath -> Map SuiteEventPath SuiteEventPath -> Template -> Map SuiteEventPath SuiteEventPath
  priorMap mParentEvnt accMap t =
    case t of
      FullSuiteTestTemplate.Test{testItems} ->
        mParentEvnt
          & maybe
            accMap
            ( \parent ->
                foldl'
                  ( \accMap' testItem ->
                      let
                        thisEvtPath = SuiteEventPath (testItemPath testItem) TE.Test
                       in
                        Map.insert thisEvtPath parent accMap'
                  )
                  accMap
                  testItems
            )
      _ ->
        foldl' (priorMap nxtB4Evnt) nxtMap t.subNodes
       where
        thisTemplateEvntPaths = SuiteEventPath t.path <$> emittedHooks t
        nxtB4Evnt = (SuiteEventPath t.path <$> getSuiteEvnt t) <|> mParentEvnt
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

templateBeforeEvnt :: Template -> Maybe SuiteEvent
templateBeforeEvnt t =
  case t of
    FullSuiteTestTemplate.Test{} -> Nothing
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

templateAfterEvnt :: Template -> Maybe SuiteEvent
templateAfterEvnt t =
  case t of
    FullSuiteTestTemplate.Test{} -> Nothing
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

emittedHooks :: Template -> [SuiteEvent]
emittedHooks = \case
  FullSuiteTestTemplate.Test{} -> []
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
      , threadSpec :: ThreadSpec
      , subNodes :: [Template]
      }
  | ThreadAfter
      { path :: Path
      , threadSpec :: ThreadSpec
      , subNodes :: [Template]
      }
  | ThreadAround
      { path :: Path
      , setupThreadSpec :: ThreadSpec
      , teardownThreadSpec :: ThreadSpec
      , subNodes :: [Template]
      }
  | EachBefore
      { path :: Path
      , spec :: Spec
      , subNodes :: [Template]
      }
  | EachAfter
      { path :: Path
      , spec :: Spec
      , subNodes :: [Template]
      }
  | EachAround
      { path :: Path
      , setupSpec :: Spec
      , teardownSpec :: Spec
      , subNodes :: [Template]
      }
  | Test
      { path :: Path
      , testItems :: [TestItem]
      }
  deriving (Show, Eq)

data EventPath = EventPath
  { path :: Path
  , suiteEvent :: SuiteEvent
  , evntSpec :: ThreadSpec
  }
  deriving (Show, Eq)

testItemPath :: TestItem -> Path
testItemPath TestItem{..} = TestPath{..}

eventPaths :: Template -> [EventPath]
eventPaths t = case t of
  FullSuiteTestTemplate.Test{testItems} ->
    (\ti -> EventPath (testItemPath ti) TE.Test $ All (ti.spec)) <$> testItems
  _ ->
    let
      recurse = concatMap eventPaths t.subNodes
      mkEvnt f p r = EventPath t.path (Hook f p) r
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
        EachBefore{spec} ->
          mkEvnt Each Before (All spec) : recurse
        EachAfter{spec} ->
          mkEvnt Each After (All spec) : recurse
        EachAround
          { setupSpec
          , teardownSpec
          } ->
            mkEvnt Each Setup (All setupSpec)
              : mkEvnt Each Teardown (All teardownSpec)
              : recurse

-- map
--   (\testItem ->
--      mkEvnt (Test {id = id testItem, title = title testItem}))
--   testItems

data TestItem = TestItem
  { id :: Int
  , title :: Text
  , spec :: Spec
  }
  deriving (Show, Eq)
