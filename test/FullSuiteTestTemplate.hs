module FullSuiteTestTemplate where

import DSL.Internal.ApEvent (Path (..))
import qualified Data.Map.Strict as Map
import Internal.ThreadEvent (HookPos (..), Hz (..), SuiteEvent (..))
import qualified Internal.ThreadEvent as TE
import Prelude hiding (id)
import PyrethrumExtras (debug')

data Result
  = Pass
  | Fail
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
  }
  deriving (Show, Eq)

testItemPath :: TestItem -> Path
testItemPath TestItem{..} = TestPath{..}

eventPaths :: Template -> [EventPath]
eventPaths t = case t of
  FullSuiteTestTemplate.Test{testItems} ->
    flip EventPath TE.Test . testItemPath <$> testItems
  _ ->
    let
      recurse = concatMap eventPaths t.subNodes
      mkEvnt f p = EventPath t.path $ Hook f p
     in
      case t of
        OnceBefore{} ->
          mkEvnt Once Before : recurse
        OnceAfter{} ->
          mkEvnt Once After : recurse
        OnceAround{} ->
          mkEvnt Once Setup
            : mkEvnt Once Teardown
            : recurse
        ThreadBefore{} ->
          mkEvnt Thread Before : recurse
        ThreadAfter{} ->
          mkEvnt Thread After : recurse
        ThreadAround{} ->
          mkEvnt Thread Setup
            : mkEvnt Thread Teardown
            : recurse
        EachBefore{} ->
          mkEvnt Each Before : recurse
        EachAfter{} ->
          mkEvnt Each After : recurse
        EachAround{} ->
          mkEvnt Each Setup
            : mkEvnt Each Teardown
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
