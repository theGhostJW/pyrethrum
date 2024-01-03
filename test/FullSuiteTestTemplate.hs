module FullSuiteTestTemplate where

import DSL.Internal.ApEvent (Path (..))
import Internal.ThreadEvent (HookPos (..), Hz (..), SuiteEvent (..))
import qualified Internal.ThreadEvent as TE
import PyrethrumExtras (uu)
import Prelude hiding (id)
import qualified Data.Map.Strict as Map

data Result
    = Pass
    | Fail
    deriving (Show, Eq)

data SuiteEventPath = SuiteEventPath
    { path :: Path
    , suiteEvent :: SuiteEvent
    }
    deriving (Show, Eq, Ord)

expectedPriorPath :: [Template] -> Map SuiteEventPath SuiteEventPath
expectedPriorPath = uu
  where
    priorMap :: (Maybe SuiteEventPath, Map SuiteEventPath SuiteEventPath) -> Template -> (Maybe SuiteEventPath, Map SuiteEventPath SuiteEventPath) 
    priorMap pm@(mParent, _) t = 
        case t of
        FullSuiteTestTemplate.Test{testItems} -> 
            mParent & maybe pm (\parent -> 
            foldl' (\(_, accMap') testItem -> 
                    let 
                     thisEvtPath = SuiteEventPath (testItemPath testItem) TE.Test
                    in
                     (mParent, Map.insert thisEvtPath parent accMap')
             ) pm testItems
            )
        _ -> 
         let 
            b4Evnt = templateBeforeEvnt t <|> mParent
            chldEvnts = t.subNodes 
         in 
            case t of 
             OnceAfter{} -> uu
         b4Evnt & maybe pm (\parent -> 
            foldl' (\(_, accMap') child -> 
                    let 
                     thisEvtPath = SuiteEventPath child.path (suiteEvent child)
                    in
                     (Just thisEvtPath, Map.insert thisEvtPath parent accMap')
             ) pm chldEvnts
            )   
         where 
            b4Evnt = templateBeforeEvnt t
            chldEvnts = t.subNodes

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
