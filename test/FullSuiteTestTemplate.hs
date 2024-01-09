module FullSuiteTestTemplate where

import DSL.Internal.ApEvent (Path (..))
import qualified Data.Map.Strict as Map
import Internal.ThreadEvent (HookPos (..), Hz (..), SuiteEvent (..))
import qualified Internal.ThreadEvent as TE
import PyrethrumExtras (uu)
import Prelude hiding (id)

data Result
    = Pass
    | Fail
    deriving (Show, Eq)

data SuiteEventPath = SuiteEventPath
    { path :: Path
    , suiteEvent :: SuiteEvent
    }
    deriving (Show, Eq, Ord)

data ParentAcc = ParentAcc
    { mB4Parent :: Maybe SuiteEventPath
    , accMap :: Map SuiteEventPath SuiteEventPath
    }
    deriving (Show, Eq)

-- | Given a list of templates, return a map of each event path to its expected preceeding parent event path
expectedParentPrecedingEvents :: [Template] -> Map SuiteEventPath SuiteEventPath
expectedParentPrecedingEvents ts = 
    (foldl' priorMap (ParentAcc Nothing Map.empty) ts).accMap
  where
    priorMap :: ParentAcc -> Template -> ParentAcc
    priorMap pm@ParentAcc{mB4Parent, accMap} t =
        case t of
            FullSuiteTestTemplate.Test{testItems} ->
                mB4Parent
                    & maybe
                        pm
                        ( \parent ->
                            foldl'
                                ( \(ParentAcc mParent' accMap') testItem ->
                                    let
                                        thisEvtPath = SuiteEventPath (testItemPath testItem) TE.Test
                                     in
                                        ParentAcc mParent' (Map.insert thisEvtPath parent accMap')
                                )
                                pm
                                testItems
                        )
            _ ->
                foldl' priorMap nxtParentAcc t.subNodes
              where
                thisTemplateEvntPaths = SuiteEventPath t.path <$> emittedHooks t
                nxtB4Evnt = (SuiteEventPath t.path <$> templateBeforeEvnt t) <|> mB4Parent
                nxtMap =
                    mB4Parent
                        & maybe
                            accMap
                            ( \p ->
                                foldl' (\accMap' thisEvtPath -> Map.insert thisEvtPath p accMap') accMap thisTemplateEvntPaths
                            )
                nxtParentAcc = ParentAcc nxtB4Evnt nxtMap

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