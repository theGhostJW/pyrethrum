{-# OPTIONS_GHC -Wdeferred-type-errors #-} 

module SuiteRuntimeTest where

import Check (Checks)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import DSL.Interpreter
import Data.Aeson.Encoding (quarter)
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Set as ST
import Data.Yaml
import GHC.Records
import Internal.PreNode (PreNode (hookChild))
import Internal.PreNode as PN
import qualified Internal.SuiteRuntime as S
import Polysemy
import Pyrelude as P
  ( Bool (..),
    Either,
    Eq (..),
    IO,
    Int,
    ListLike (foldl', head, null, unsafeHead, unsafeLast),
    Maybe (Just, Nothing),
    Num ((+)),
    Ord (..),
    Show,
    SomeException,
    Text,
    Traversable (sequenceA, traverse),
    catMaybes,
    const,
    count,
    debug_,
    debugf,
    dropWhile,
    dropWhileEnd,
    error,
    filter,
    find,
    for_,
    fromJust,
    groupBy,
    isPrefixOf,
    last,
    length,
    maybe,
    maybef,
    not,
    nub,
    pure,
    replicateM_,
    reverse,
    singleton,
    toS,
    traverse_,
    txt,
    txtPretty,
    uncurry,
    uu,
    zip,
    ($),
    (&),
    (&&),
    (-),
    (.),
    (<$>),
    (<>),
    (>>),
    (>>=),
    (?),
    (\\),
    (||),
  )
import Pyrelude.Test (chk', chkFail)
import Pyrelude.Test as T hiding (filter, maybe, singleton)
import TempUtils (debugLines)
import Text.Show.Pretty (PrettyVal (prettyVal), pPrint, pPrintList, ppShow, ppShowList)
import UnliftIO.Concurrent as C
  ( ThreadId,
    forkFinally,
    forkIO,
    myThreadId,
    threadDelay,
  )
import UnliftIO.STM
import Prelude (Ord, putStrLn, String)
import qualified Data.Map.Strict as M 
import Internal.RunTimeLogging (Loc (Loc), unLoc)


data BoundaryType
  = Start
  | End
  deriving (Show, Eq, Ord)

data BranchType
  = Hook
  | Fixture
  deriving (Show, Eq, Ord)

data NodeStats
  = HookStats
      { id :: Text,
        parent :: Text,
        fixtureCount :: Int,
        hookCount :: Int
      }
  | FixtureStats
      { id :: Text,
        parent :: Text,
        iterationCount :: Int
      }
  deriving (Show)

isHookStats :: NodeStats -> Bool
isHookStats = \case
  HookStats {} -> True
  FixtureStats {} -> False

isFixtureStats :: NodeStats -> Bool
isFixtureStats = \case
  HookStats {} -> False
  FixtureStats {} -> True

countSubNodes :: (forall a b c d e f. PreNode a b c d e f -> Bool) -> PreNode g h i j k l -> Int
countSubNodes pred node =
  let countSubNodes' :: forall h i j k l m. Int -> PreNode h i j k l m -> Int
      countSubNodes' accum = \case
        b@Branch {subElms} ->
          foldl' countSubNodes' (pred b ? accum + 1 $ accum) subElms
        ah@OnceHook {} -> pred ah ? accum + 1 $ accum
        ThreadHook {} -> uu
        TestHook {} -> uu
        fx@PN.Fixture {} -> pred fx ? accum + 1 $ accum
   in countSubNodes' 0 node

getStats :: PreNodeRoot -> IO [NodeStats]
getStats PreNodeRoot {rootNode} =
  getStats' "Root" 0 <$> rootNode
  where
    nonEmptyFixture :: PreNode a b c d e f -> Bool
    nonEmptyFixture = \case
      PN.OnceHook {} -> False
      PN.Branch {} -> False
      ThreadHook {} -> False
      TestHook {} -> False
      f@PN.Fixture {} -> not $ nodeEmpty f

    nonEmptyHook :: PreNode a b c d e f -> Bool
    nonEmptyHook = \case
      h@PN.OnceHook {} -> not $ nodeEmpty h
      PN.Branch {} -> False
      ThreadHook {} -> False
      TestHook {} -> False
      PN.Fixture {} -> False

    getStats' :: Text -> Int -> PreNode a b c d e f -> [NodeStats]
    getStats' parentId subIndex =
      \case
        PN.Branch {subElms} ->
          let thisId = parentId <> ".Branch " <> txt subIndex
           in (zip [0 ..] subElms >>= uncurry (getStats' thisId))
        PN.OnceHook {hookChild} ->
          let thisId = parentId <> ".Hook " <> txt subIndex
              thisNode =
                HookStats
                  { id = thisId,
                    parent = parentId,
                    fixtureCount = countSubNodes nonEmptyFixture hookChild,
                    hookCount = countSubNodes nonEmptyHook hookChild
                  }
           in thisNode : getStats' thisId 0 hookChild
        ThreadHook {} -> uu
        TestHook {} -> uu
        PN.Fixture {iterations} ->
          [ FixtureStats
              { id = parentId <> ".Fixture " <> txt subIndex,
                parent = parentId,
                iterationCount = M.size iterations
              }
          ]


data BoundaryInfo = BoundaryInfo
  { id :: Loc,
    childCount :: Int
  }

data RunEvent
  = Boundary
      { branchType :: BranchType,
        boundaryType :: BoundaryType,
        threadId :: ThreadId,
        id :: Loc
      }
  | IterationMessage
      { 
        index :: Int,
        message :: Maybe Text,
        threadId :: ThreadId
      }
  | Message Text ThreadId
  deriving (Show, Eq, Ord)

logEvent :: TQueue RunEvent -> (ThreadId -> RunEvent) -> IO ()
logEvent q ev = do
  i <- myThreadId
  atomically . writeTQueue q $ ev i

logBoundary :: TQueue RunEvent -> BranchType -> BoundaryType -> Loc -> IO ()
logBoundary q brt bdt loc =
  logEvent q $ \thrd ->
    Boundary
      { branchType = brt,
        boundaryType = bdt,
        threadId = thrd,
        id = loc
      }

hook :: TQueue RunEvent -> BoundaryType -> Loc -> o -> IO o
hook q bdt loc hko = logBoundary q SuiteRuntimeTest.Hook bdt loc >> pure hko

hookStart :: TQueue RunEvent -> Loc -> o -> IO o
hookStart q = SuiteRuntimeTest.hook q Start

hookEnd :: TQueue RunEvent -> Loc -> IO ()
hookEnd q loc = SuiteRuntimeTest.hook q End loc ()

fixture :: TQueue RunEvent -> BoundaryType -> Loc -> IO ()
fixture q = logBoundary q SuiteRuntimeTest.Fixture

fixtureStart :: TQueue RunEvent -> Loc -> IO ()
fixtureStart q = SuiteRuntimeTest.fixture q Start

fixtureEnd :: TQueue RunEvent -> Loc -> IO ()
fixtureEnd q = SuiteRuntimeTest.fixture q End

logIteration :: TQueue RunEvent -> Maybe Text -> Int -> IO ()
logIteration q itMsg iidx =
  logEvent q (IterationMessage iidx itMsg)

logMessage :: TQueue RunEvent -> Text -> IO ()
logMessage q txt' = logEvent q (Message txt')

-- remove when pyrelude updated
chkEq' t = assertEqual (toS t)

mkBranch :: TQueue RunEvent -> [IO (PreNode si so ti to ii io)] -> IO (PreNode si () ti ()  ii ())
mkBranch q subElms = PN.Branch Nothing <$> sequenceA subElms

mkFixture :: TQueue RunEvent -> Int -> IO (PreNode i () ti () ii ())
mkFixture q itCount = do
  pure $
    PN.Fixture
      { 
        fxTag = Nothing,
        logStart = fixtureStart q,
        iterations = M.fromList $ zip (txt <$> [1..]) $ mkIterations q itCount,
        logEnd = fixtureEnd q
      }


-- mkHook :: TQueue RunEvent -> so -> IO (PreNode so so2 ti to) -> IO (PreNode si so ti to)
mkHook q hko nodeChild =
  do
    status <- atomically $ newTVar S.Pending
    rslt <- (newEmptyTMVarIO :: IO (TMVar (Either SomeException o)))
    nc <- nodeChild
    pure
      PN.OnceHook
        { 
          hookTag = Nothing,
          hookResult = rslt,
          hook = \loc _ -> hookStart q loc hko,
          hookChild = nc,
          hookRelease = \loc _ -> hookEnd q loc
        }

iterationMessage :: Int -> Text
iterationMessage i = "iteration " <> txt i

mkIterations :: TQueue RunEvent -> Int -> [oi -> ti -> ii -> IO ()]
mkIterations q size' =
  let mkIt :: Int -> oi' -> ti' -> ii' -> IO ()
      mkIt idx oi ti ii = logIteration q Nothing idx 
   in mkIt <$> [0 .. size' - 1]

root :: IO (PreNode () () () () () ()) -> IO PreNodeRoot
root = pure . PreNodeRoot

superSimplSuite :: TQueue RunEvent -> IO PreNodeRoot
superSimplSuite q =
  root $ mkFixture q 1

simpleSuiteWithHook :: TQueue RunEvent -> IO PreNodeRoot
simpleSuiteWithHook q = do
  root
    . mkHook q ()
    $ mkFixture q 1

-- simpleBranchedSuiteWithHook :: TQueue RunEvent -> IO PreNodeRoot
-- simpleBranchedSuiteWithHook q = do
--   fx <- mkFixture q "Root.Hook 0" "Fixture 0" 1
--   root $ (: []) <$> mkHook q "Root" "Hook 0" fx

tQToList :: TQueue a -> IO [a]
tQToList q =
  reverse <$> recurse []
  where
    recurse l =
      atomically (tryReadTQueue q)
        >>= maybe (pure l) (\a -> recurse (a : l))

boundaryId :: BranchType -> BoundaryType -> RunEvent -> Maybe Text
boundaryId brt bnt = \case
  Boundary {branchType, boundaryType, id} -> branchType == brt && boundaryType == bnt ? (Just . unLoc $ id) $ Nothing
  IterationMessage {} -> Nothing
  Message {} -> Nothing

boundaryId' :: RunEvent -> Maybe Text
boundaryId' = \case
  Boundary {id} -> Just . unLoc $ id
  IterationMessage {} -> Nothing
  Message {} -> Nothing

isBoundary :: RunEvent -> Bool
isBoundary = \case
  Boundary {} -> True
  IterationMessage {} -> False
  Message {} -> False

isHook :: RunEvent -> Bool
isHook re = isBoundary re && branchType re == SuiteRuntimeTest.Hook

mboundaryType :: RunEvent -> Maybe BoundaryType
mboundaryType = \case
  Boundary {boundaryType = bt} -> Just bt
  IterationMessage {} -> Nothing
  Message {} -> Nothing

chkHooks :: [NodeStats] -> [RunEvent] -> IO ()
chkHooks stats evntLst =
  let hks = filter isHookStats stats
      expectedHkIds = (id :: NodeStats -> Text) <$> hks
      bndrys = filter isBoundary evntLst
      actualHooks = filter isHook bndrys
      actualHkIds = nub . catMaybes $ boundaryId' <$> actualHooks

      chkStartAndEnd :: Text -> [RunEvent] -> IO ()
      chkStartAndEnd msg = \case
        [] -> chkFail $ msg <> " - expected start and end boundary but no boundarry found"
        [bnd] -> chkFail $ msg <> " - expected start and end boundary but only one boundarry found " <> txt bnd
        [start, end] -> do
          -- hook should log a start before it ends
          chkEq' (msg <> " " <> txt start) (Just Start) (mboundaryType start)
          chkEq' (msg <> " " <> txt end) (Just End) (mboundaryType end)
        a -> chkFail $ msg <> " too many boundary events" <> txt a

      chkHkExists :: Text -> IO ()
      chkHkExists id' =
        let hkbds =
              filter
                ( \n ->
                    boundaryId SuiteRuntimeTest.Hook Start n == Just id'
                      || boundaryId SuiteRuntimeTest.Hook End n == Just id'
                )
                bndrys
         in chkStartAndEnd ("hook: " <> id') hkbds

      chkUnexpectedHooks :: IO ()
      chkUnexpectedHooks = chkEq' "Unexpected or empty hooks in actual" [] (actualHkIds \\ expectedHkIds)

      chkSubElements :: Text -> IO ()
      chkSubElements nId =
        let hooksStats = fromJust $ find (\s -> nId == (id :: NodeStats -> Text) s) hks
            actualSubElms =
              groupBy (\e1 e2 -> boundaryId' e1 == boundaryId' e2)
                . filter (\re -> ((\id' -> isPrefixOf nId id' && nId /= id') <$> boundaryId' re) == Just True)
                . dropWhileEnd (\re -> boundaryId' re /= Just nId)
                . dropWhile (\re -> boundaryId' re /= Just nId)
                $ bndrys
            actualSubHooks = filter (\g -> Just True == ((SuiteRuntimeTest.Hook ==) . branchType <$> head g)) actualSubElms
            actualSubFixtures = filter (\g -> Just True == ((SuiteRuntimeTest.Fixture ==) . branchType <$> head g)) actualSubElms
         in do
              chkEq' "expected hook count" (hookCount hooksStats) (length actualSubHooks)
              chkEq' "expected fixture count" (fixtureCount hooksStats) (length actualSubFixtures)
              traverse_ (chkStartAndEnd "sub-hooks") actualSubHooks
              traverse_ (chkStartAndEnd "sub-fixture") actualSubFixtures
   in do
        traverse_ chkHkExists expectedHkIds
        chkUnexpectedHooks
        traverse_ chkSubElements expectedHkIds

chkFixtures :: [NodeStats] -> [RunEvent] -> IO ()
chkFixtures stats evntLst =
  let fixIds bt = catMaybes $ boundaryId SuiteRuntimeTest.Fixture bt <$> evntLst
      fixStarts = fixIds Start
      fixEnds = fixIds End

      fixStatCount = count isFixtureStats stats

      chkFixture :: NodeStats -> IO ()
      chkFixture = \case
        HookStats {} -> pure ()
        FixtureStats {id, parent, iterationCount} ->
          let matchesFix :: RunEvent -> Bool
              matchesFix = \case
                Boundary {branchType, id = id'} -> branchType == SuiteRuntimeTest.Fixture && id' == Loc id
                -- IterationMessage {parentFix} -> parentFix == Loc id
                IterationMessage {} -> uu
                Message {} -> False

              evntsToChk :: [RunEvent]
              evntsToChk = P.filter matchesFix evntLst

              emptyFix = iterationCount == 0

              iternIdx :: RunEvent -> Maybe Int
              iternIdx = \case
                Boundary {} -> Nothing
                IterationMessage {index} -> Just index
                Message {} -> Nothing

              itrLst :: [Int]
              itrLst = catMaybes $ iternIdx <$> evntsToChk

              itrIdxs :: ST.Set Int
              itrIdxs = ST.fromList itrLst

              expectedIterationIdxs :: ST.Set Int
              expectedIterationIdxs = emptyFix ? ST.empty $ ST.fromList [0 .. iterationCount - 1]

              isIteration :: RunEvent -> Bool
              isIteration = \case
                Boundary {} -> False
                IterationMessage {} -> True
                Message {} -> False

              firstEv = head evntsToChk >>= mboundaryType
              lastEv = last evntsToChk >>= mboundaryType
           in emptyFix
                ? chk' ("Fixture: " <> id <> " has no iterations - no related run events should be logged") (null evntsToChk)
                $ do
                  null evntsToChk
                    ?
                    -- run events for fixture must exist
                    chkFail ("Fixture: " <> id <> " has " <> txt iterationCount <> " iterations but no run events ")
                    $ do
                      -- the first event must be
                      chkEq' "first event should be fixture start" (Just Start) firstEv
                      chkEq' "last event should be fixture end" (Just End) lastEv
                      -- iteration counts should match
                      chkEq' "iteration count" iterationCount $ length itrLst
                      chkEq' "iteration indexes" expectedIterationIdxs itrIdxs
   in do
        chkEq' "expected fixture start count" fixStatCount $ length fixStarts
        chkEq' "expected fixture end count" fixStatCount $ length fixEnds
        for_ stats chkFixture

exeSuiteTests :: (TQueue RunEvent -> IO PreNodeRoot) -> Int -> IO ()
exeSuiteTests preSuite threadCount = do
  q <- atomically newTQueue
  preSuite' <- preSuite q
  stats <- getStats preSuite'
  S.execute threadCount uu preSuite'
  l <- tQToList q
  putStrLn ""
  putStrLn "============ Stats ============"
  pPrint stats
  putStrLn ""
  putStrLn "============ Logs ============"
  pPrint l
  chkFixtures stats l
  chkHooks stats l

-- $> unit_simple_single

unit_simple_single :: IO ()
unit_simple_single = do
  exeSuiteTests superSimplSuite 1

-- $> unit_simple_with_hook

unit_simple_with_hook :: IO ()
unit_simple_with_hook =
  replicateM_ 1 $
    exeSuiteTests simpleSuiteWithHook 1

-- ~ simple branch
simpleSuiteWithBranch :: TQueue RunEvent -> IO PreNodeRoot
simpleSuiteWithBranch q =
  let subElms =
        [ mkFixture q 2 :: IO (PreNode () () () () () ()),
          mkFixture q 2 :: IO (PreNode () () () () () ())
        ]
   in root $ mkBranch q subElms

simpleSuiteBranchInHook :: TQueue RunEvent -> IO PreNodeRoot
simpleSuiteBranchInHook q =
  root $
    mkBranch
      q
      [ mkHook q 7 $
          mkBranch
            q
            [ mkFixture q 2,
              mkFixture q 2
            ]
      ]

deeplyNested :: TQueue RunEvent -> IO PreNodeRoot
deeplyNested q =
  let 
      hk :: o -> IO (PreNode o o2 to to2 io io2) -> IO (PreNode i o to to2 io io2)
      hk = mkHook q

      branch :: [IO (PreNode si so ti to ii io)] -> IO (PreNode si () ti () ii ())
      branch = mkBranch q

      fx :: Int -> IO (PreNode i () ti () ii ())
      fx = mkFixture q
   in root $
        branch
          [ hk "hkOut" $
              hk 88 $
                branch
                  [ fx 2,
                    fx 2,
                    branch
                      [ hk "Out Str" $
                          fx 2
                      ],
                    branch
                      [ fx 2
                      ]
                  ]
          ]

{-
ISSUE

3 threads

shk
  thk
    shk
      fx
      fx
      fx
    thk
      fx
      fx
      fx

3 threads

shk - t1
  thk - t1 t2 t3
    shk - t1
      fx - t1
      fx - t2
      fx - t3
      thk
       fx
       fx
       fx
  thk
    fx
    fx
    fx

3 threads

shk - t1
  thk - t1 t2 t3
    fx
    shk - t1
      fx - t1
      fx - t2
      fx - t3
      thk
       thk
        fx
        fx
        fx
  thk'
    fx
    fx
    fx

- has fixtures
 - shk - thk
 - shk - thk'
 - shk - thk - shk
 - shk - thk - shk - thk - thk

 exeBranch:
    [
      sk 2 v0 s [] [
        thk [fx] [
          shk [fx fx fx] [
            thk [] [
                thk [fx fx fx] [],
                thk [fx fx fx] [],
                thk [fx fx fx] []
              ]
          ]
        ],
        thk [
          fx fx fx
        ]
      ]
    ]

    RuntimeElement

   1. run fixture set
    - thread will not escape fixture set until all fixtures run
    - thread will not escape sub hooks until all fixtures run
   2. atomically
    - find subelem not running
    - find sub-elem partially not running
    - next index
   3. run thread

- issues
  % initial distribution
  % tracking state - pending / running / complete
  %

-- allow for no thread hooks
shk - t1
      fx - t1
      fx - t2
      fx - t3
       fx
       fx
       fx
      shk
    fx
    fx
    fx

1. thread must live longer than one fixture
2. prioritise adjacent fixtures when continuing thread
3. hook finalise when crossing thread hook boundary
4. prioritise distant fixtures when launching new thread
  % constuct breath first list of thread hooks
  % ensure wait for thread hooks to finish
  % test with no thread hooks
  % test fixtures before thread hooks
  % test run time of singleton hooks vs thread hooks
    - will not follow tree structure
    - n threads

-}

{- TODO
  ~ DONE: chkHks

  ~ branching / thread level
    ~ new constructors
      ~ DONE: add branch constructor
      ~ DONE: add thread level hook constructor
    ~ structure
      ~ nested structures - construct
        ~ DONE: simple branch
        ~ DONE: nested hook
        ~ DONE: nested branch hook
        ~ DONE: nested hook - type changing
        ~ DONE: nested branch hook - type changing
        ~ DONE: nested hooks multiple - type changing
        ~ DONE: nested branches multiple - type changing
        ~ DONE: check for simplifications
    ~ !!!!! MISDESIGN FIX
    ~ implemntation
      ~ update runtime ~ remove uu completing implementation of thread level hooks
      ~ add thread level hooks to deep nested structure


      ~ thread through logging
        ~ start by deleting logEnd and logStart on fixture 
        ~ add a sink parameter 
        ~ log start end of everything ~ include thread Id
        ~ update validation

      ~ reinstate existing tests
      ~ update validation to include thread level hooks
      ~ add thread level tests

    ~ update prepare to not add empty nodes
      ~ update tests to include empty nodes

  ~ add iteration level
    ~ constructors
    ~ structure
    ~ implementation
    ~ reinstate existing tests

    ~ update validation
    ~ tests

  ~ advnced tests
  
  ~ hook timeout
  ~ test with empty:
    ~ fixtures
    ~ hooks
      ~ singleton
      ~ thread-level
      ~ iteration level
  ~ simple multiple hooks / iterations / threads

  ~ simple exceptions
    ~ update stats expectations
    ~ iteration
    ~ singleton hook
    ~ thread-level hook
  ~ simple multi threaded

  ~ property based - inc differening times / hook types / hook fixture counts
   ~ import validation
   ~ write generators
   ~ write shrinkers

  ~ killing run
    ~ implement
    ~ test
      ~ make sure tests killed in finalisation don't keep running
      ~ must not handle async exceptions

  ~ fixture thread limits
    ~ update constructors
    ~ runtime
    ~ update validations
-}
