module FullSuiteTestTemplate where

import DSL.Internal.ApEvent (Path (..))
import Data.Map.Strict qualified as Map
import Internal.ThreadEvent (HookPos (..), Hz (..), SuiteEvent (..))
import Internal.ThreadEvent qualified as TE
import Prelude hiding (id)
import PyrethrumExtras (debug')

data Result
  = Pass
  | Fail
  deriving (Show, Eq)

data ThreadResult
  = All Result
  | Some [Result]
  deriving (Show, Eq)

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
      , delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | OnceAfter
      { path :: Path
      , delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | OnceAround
      { path :: Path
      , delay :: Int
      , setupResult :: Result
      , teardownResult :: Result
      , subNodes :: [Template]
      }
  | ThreadBefore
      { path :: Path
      , delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | ThreadAfter
      { path :: Path
      , delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | ThreadAround
      { path :: Path
      , delay :: Int
      , setupResult :: Result
      , teardownResult :: Result
      , subNodes :: [Template]
      }
  | EachBefore
      { path :: Path
      , delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | EachAfter
      { path :: Path
      , delay :: Int
      , result :: Result
      , subNodes :: [Template]
      }
  | EachAround
      { path :: Path
      , delay :: Int
      , setupResult :: Result
      , teardownResult :: Result
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
  , result :: Result
  }
  deriving (Show, Eq)

testItemPath :: TestItem -> Path
testItemPath TestItem{..} = TestPath{..}

eventPaths :: Template -> [EventPath]
eventPaths t = case t of
  FullSuiteTestTemplate.Test{testItems} ->
     (\ti -> EventPath (testItemPath ti) TE.Test ti.result) <$> testItems
  _ ->
    let
      recurse = concatMap eventPaths t.subNodes
      mkEvnt f p r = EventPath t.path (Hook f p) r
     in
      case t of
        OnceBefore{result} ->
          mkEvnt Once Before result : recurse
        OnceAfter{result} ->
          mkEvnt Once After result : recurse
        OnceAround{setupResult
                  , teardownResult} ->
          mkEvnt Once Setup setupResult
            : mkEvnt Once Teardown teardownResult
            : recurse
        ThreadBefore{result} ->
          mkEvnt Thread Before result: recurse
        ThreadAfter{result} ->
          mkEvnt Thread After result: recurse
        ThreadAround{setupResult
                  , teardownResult} ->
          mkEvnt Thread Setup setupResult
            : mkEvnt Thread Teardown teardownResult
            : recurse
        EachBefore{result} ->
          mkEvnt Each Before result: recurse
        EachAfter{result} ->
          mkEvnt Each After result: recurse
        EachAround{setupResult
                  , teardownResult} ->
          mkEvnt Each Setup setupResult
            : mkEvnt Each Teardown teardownResult
            : recurse

-- map
--   (\testItem ->
--      mkEvnt (Test {id = id testItem, title = title testItem}))
--   testItems

data TestItem = TestItem
  { id :: Int
  , title :: Text
  , delay :: Int
  , result :: Result
  }
  deriving (Show, Eq)
