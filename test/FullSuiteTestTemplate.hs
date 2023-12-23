module FullSuiteTestTemplate where

import DSL.Internal.ApEvent (Path (..))
import Internal.ThreadEvent (SuiteEvent (..), Hz (..), HookPos (..))
import qualified Internal.ThreadEvent as TE


data Result
  = Pass
  | Fail
  deriving (Show, Eq)

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

eventPaths :: Template -> [EventPath]
eventPaths t = case t of
    FullSuiteTestTemplate.Test{testItems} ->
        let
            mkEvnt i = EventPath (TestPath i.id i.title) TE.Test
         in
            map mkEvnt testItems
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
