--  should be able to remove this in later versions of GHC
-- https://gitlab.haskell.org/ghc/ghc/-/issues/21443
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SuiteRuntimeTest where

import Check (Checks)

-- -- TODO Add to Pyrelude
-- -- TODO Add to Pyrelude

-- import Control.Monad (Functor ((<$)), void, foldM_)
-- import Control.Monad.Reader (ReaderT (runReaderT), ask)
-- import DSL.Interpreter
-- import Data.Aeson.Encoding (quarter)
-- import Data.Aeson.TH
-- import Data.Aeson.Types
-- import qualified Data.IntMap.Merge.Lazy as ST
import qualified Data.Map.Strict as M

-- import qualified Data.Set as ST
-- import qualified Data.Text as Txt
-- import Data.Yaml
-- import GHC.Records
-- import Hedgehog.Internal.State (Action (actionExecute))
-- import Internal.PreNode
-- import qualified Internal.PreNode as PN
import Internal.RunTimeLogging as L (
  -- ExeEvent (..),
  -- ExeEventType (..),
  -- Loc (..),

  -- PException,
  -- SThreadId (..),
  -- endIsTerminal,
  -- isFixtureChild,
  -- isGrouping,
  -- isOnceEvent,
  -- isThreadedEvent,
  -- mkLogger,
  ExeEvent (..),
  ExeLog,
  Log (..),
  LogControls (..),
  testLogControls,
 )

-- import Internal.SuiteRuntime
-- import qualified Internal.SuiteRuntime as S
import qualified PyrethrumExtras.Test as T

-- import TempUtils (debugLines)
import Text.Show.Pretty (PrettyVal (prettyVal), pPrint, pPrintList, ppDocList, ppShow, ppShowList)

-- import UnliftIO.Concurrent as C
--   ( ThreadId,
--     forkFinally,
--     forkIO,
--     myThreadId,
--     threadDelay,
--   )
-- import UnliftIO.STM
import Prelude as P hiding (atomically, fail, head, last, newTVarIO)

import Data.List.NonEmpty (singleton)
import Data.Traversable (for)
import GHC.Show (Show (..))
import Internal.PreNode (
  Fixture (..),
  PreNode (..),
  Test (..),
  TestHook (..),
  ThreadHook (ThreadAfter, ThreadAround, ThreadBefore, ThreadNone),
 )
import qualified Internal.PreNode as P (
  OnceHook (..),
 )
import Internal.SuiteRuntime (execute)
import List.Extra as L (head, last, zipWithIndex)
import PyrethrumExtras
import UnliftIO (newTChanIO)
import qualified UnliftIO.Concurrent as C
import UnliftIO.STM (
  TQueue,
  atomically,
  newTQueue,
  newTQueueIO,
  tryReadTQueue,
  writeTQueue,
 )

data DocFunc a = DocFunc
  { doc :: Text
  , func :: IO a
  }

instance Show (DocFunc a) where
  show :: DocFunc a -> String
  show = toS . (.doc)

data ExeOutcome = Pass | Fail deriving (Show, Eq)

data IOProps = IOProps
  { delayms :: Int
  , outcome :: ExeOutcome
  }
  deriving (Show)

data THook
  = None
  | Before (NonEmpty IOProps)
  | After (NonEmpty IOProps)
  | Around
      { hook :: NonEmpty IOProps
      , release :: NonEmpty IOProps
      }
  deriving (Show)

data TOnceHook
  = OnceNone
  | OnceBefore IOProps
  | OnceAfter IOProps
  | OnceAround
      { hook :: IOProps
      , release :: IOProps
      }
  deriving (Show)

data Template
  = TGroup
      { threadLimit :: Maybe Int
      , onceHook :: TOnceHook
      , threadHook :: THook
      , subNodes :: NonEmpty Template
      }
  | TFixtures
      { threadLimit :: Maybe Int
      , testHook :: THook
      , fixtures :: NonEmpty TFixture
      }
  deriving (Show)

name :: Template -> Text
name = \case
  TGroup{} -> "Group"
  TFixtures{} -> "Fixture"

data TFixture = TFixture
  { maxThreads :: Maybe Int
  , onceHook :: TOnceHook
  , threadHook :: THook
  , testHook :: THook
  , tests :: NonEmpty IOProps
  }
  deriving (Show)

ioAction :: IOProps -> Text -> IO ()
ioAction IOProps{delayms, outcome} erMsg =
  do
    C.threadDelay delayms
    when (outcome == Fail)
      . error
      . toS
      $ "exception thrown " <> erMsg

loadProps :: NonEmpty IOProps -> STM (TQueue (Int, IOProps))
loadProps ip = do
  q <- newTQueue
  traverse_ (writeTQueue q) (zipWithIndex ip)
  pure q

ioActions :: TQueue (Int, IOProps) -> Text -> IO ()
ioActions q ttl =
  atomically (tryReadTQueue q)
    >>= maybe
      (error $ "queue empty bad test setup: " <> ttl)
      (\(idx, ip') -> ioAction ip' (ttl <> " [" <> txt idx <> "]"))

prepare :: forall a. Int -> Template -> STM (PreNode a () ())
prepare threadCount = prepare' 0 0
 where
  prepare' :: Int -> Int -> Template -> STM (PreNode a () ())
  prepare' depth idx tp =
    let title = name tp <> " [" <> txt depth <> "." <> txt idx <> "]"
     in tp & \case
          TGroup{threadLimit, onceHook, threadHook = th, subNodes = sn} ->
            do
              threadHook <- thrdHk title th
              subNodesl <- traverse (uncurry (prepare' (succ depth))) $ zipWithIndex sn
              pure $
                Group
                  { title
                  , threadLimit
                  , onceHook = onceHk title onceHook
                  , threadHook
                  , subNodes = fromList subNodesl
                  }
          TFixtures
            { threadLimit
            , testHook = th
            , fixtures = fx
            } ->
              do
                testHook <- tstHk title th
                fixtures <- mkFixtures title fx
                pure $
                  Fixtures
                    { title
                    , threadLimit
                    , testHook
                    , fixtures
                    }

  mkp1 t q c p1 = ioActions q t

  mkp2 t q c p1 p2 = ioActions q t
  mkp3 t q c p1 p2 p3 = ioActions q t
  mkp1Singleton t q c p1 = ioAction q t

  tstHk :: Text -> THook -> STM (TestHook a () () () ())
  tstHk ttl hk =
    hk & \case
      None -> pure TestNone
      Before ip -> TestBefore . mkp3 ttl <$> loadProps ip
      After ip -> TestAfter . mkp1 ttl <$> loadProps ip
      Around ipb ipa -> TestAround . mkp3 ttl <$> loadProps ipb <*> (mkp1 ttl <$> loadProps ipa)

  thrdHk :: Text -> THook -> STM (ThreadHook a () () ())
  thrdHk ttl hk =
    hk & \case
      None -> pure ThreadNone
      Before ip -> ThreadBefore . mkp2 ttl <$> loadProps ip
      After ip -> ThreadAfter . mkp1 ttl <$> loadProps ip
      Around ipb ipa -> ThreadAround . mkp2 ttl <$> loadProps ipb <*> (mkp1 ttl <$> loadProps ipa)

  onceHk :: Text -> TOnceHook -> P.OnceHook a () ()
  onceHk ttl =
    \case
      OnceNone -> P.OnceNone
      OnceBefore ip -> P.OnceBefore (mkp1Singleton ttl ip)
      OnceAfter ip -> P.OnceAfter (mkp1Singleton ttl ip)
      OnceAround ipb ipa -> P.OnceAround (mkp1Singleton ttl ipb) (mkp1Singleton ttl ipa)

  mkFixtures :: Text -> NonEmpty TFixture -> STM (NonEmpty (Fixture a () () ()))
  mkFixtures title fxs =
    traverse (uncurry mkFixture) $ fromList $ zipWithIndex fxs
   where
    mkFixture :: Int -> TFixture -> STM (Fixture a () () ())
    mkFixture idx TFixture{maxThreads, onceHook = oh, threadHook = th, testHook = tsth, tests = tsts} = do
      threadHook <- thrdHk title th
      testHook <- tstHk title tsth
      tests <- loadProps tsts
      pure
        Fixture
          { title = title <> " [" <> txt idx <> "]"
          , maxThreads
          , onceHook = onceHk title oh
          , threadHook
          , testHook
          , tests = mkTest title <$> tsts
          }
    mkTest :: Text -> IOProps -> Test a () () ()
    mkTest t p =
      Test
        { id = t
        , test = \_ctx _oi _ti _tsti -> ioAction p t
        }

q2List :: TQueue a -> STM [a]
q2List qu = reverse <$> recurse [] qu
 where
  recurse :: [a] -> TQueue a -> STM [a]
  recurse l q =
    tryReadTQueue q
      >>= maybe (pure l) (\e -> recurse (e : l) q)

runTest :: Int -> Template -> IO ()
runTest maxThreads template = do
  -- validateTemplate template
  putStrLn ""
  pPrint template
  putStrLn "========="
  chan <- newTChanIO
  q <- newTQueueIO
  ior <- newIORef 0
  tid <- C.myThreadId
  lc@LogControls{sink, log} <- testLogControls chan q
  pn <- atomically $ prepare maxThreads template
  execute maxThreads lc pn
  log
    & maybe
      (T.chkFail "No Events Log")
      (\evts -> atomically (q2List evts) >>= chkProperties maxThreads template)

chkProperties :: (Show a, Show l) => Int -> Template -> [ExeLog l a] -> IO ()
chkProperties mxThrds t evts =
  do
    traverse_
      (evts &)
      [ chkThreadLogsInOrder
      , chkStartEndExecution
      -- chkSingletonLeafEventsStartEnd,
      -- chkOnceEventsAreBlocking,
      --         chkEventCounts t,
      --         chkOnceHksReleased,
      --         chkErrorPropagation
      ]

-- chkProperties :: Int -> Template -> [ExeEvent] -> IO ()
-- chkProperties mxThrds t evts =
--   do
--     traverse_
--       (evts &)
--       [ chkThreadLogsInOrder,
--         chkStartEndExecution,
--         chkSingletonLeafEventsStartEnd,
--         chkOnceEventsAreBlocking,
--         chkEventCounts t,
--         chkOnceHksReleased,
--         chkErrorPropagation
--       ]
--     traverse_
--       (threadedEvents &)
--       [ chkStartEndIntegrity,
--         chkFixtureChildren,
--         chkFixturesContainTests t,
--         traverse_ chkTestEvtsConsecutive,
--         chkThreadLeafEventsStartEnd,
--         traverse_ (chkParentOrder t),
--         traverse_ chkThreadHksReleased
--       ]
--     T.chk'
--       ( "max execution threads + 2: "
--           <> txt (mxThrds + 1)
--           <> " exceeded: "
--           <> txt (length threadedEvents)
--           <> "\n"
--           <> txt (ppShow (((.threadId) <$>) <$> threadedEvents))
--       )
--       $ length threadedEvents <= mxThrds + 2
--   where
--     threadedEvents = groupOn (.threadId) evts
{-
 * happy path test
 * happy path test with hook
 * simple effefectful
 * complete tests
-}

-- type TextLogger = Text -> IO ()

-- foldTemplate :: forall a. a -> (a -> Template -> a) -> Template -> a
-- foldTemplate seed f t =
--   let tval = f seed t
--       recurse = foldTemplate tval f
--    in t & \case
--         TGroup {tChilds} -> foldl' f tval tChilds
--         TOnceHook {tChild} -> recurse tChild
--         TThreadHook {tChild} -> recurse tChild
--         TFixture {} -> tval

-- templateList :: Template -> [Template]
-- templateList = foldTemplate [] (flip (:))

-- childToParentMap :: Template -> M.Map Loc Loc
-- childToParentMap =
--   snd . cpm Root M.empty
--   where
--     cpm :: Loc -> M.Map Loc Loc -> Template -> (Loc, M.Map Loc Loc)
--     cpm pLoc accMap =
--       foldTemplate
--         (pLoc, accMap)
--         ( \(pl, m) t ->
--             let cl = Node pl t.tag
--                 accm = M.insert cl pl m

--                 mkLoc loc et = (Node loc $ txt et)
--                 mkLocIdxed loc et idx = (Node loc $ txt et <> " :: " <> txt idx)

--                 subMap :: ExeEventType -> M.Map Loc Loc -> M.Map Loc Loc
--                 subMap et = M.insert (mkLoc cl et) cl

--                 subMap' et = subMap et accm
--                 accm' =
--                   t & \case
--                     TGroup {} -> accm
--                     TOnceHook {} ->
--                       subMap' L.OnceHookRelease
--                     TThreadHook {} ->
--                       subMap' L.ThreadHookRelease
--                     TFixture {tag, tTests} ->
--                       let fxloc = cl
--                           fxOnceHkloc = mkLoc fxloc L.FixtureOnceHook
--                           fxOnceHkReleaseloc = mkLoc fxOnceHkloc L.FixtureOnceHookRelease
--                           fxThrdHkloc = mkLoc fxOnceHkloc L.FixtureThreadHook
--                           fxThrdhkReleaseloc = mkLoc fxThrdHkloc L.FixtureThreadHookRelease

--                           insertTstTstHkRelease mp =
--                             foldl'
--                               ( \mp' i ->
--                                   let tsthkLoc = mkLocIdxed fxThrdHkloc L.TestHook i
--                                       tstLoc = mkLocIdxed tsthkLoc L.Test i
--                                       tsthkRloc = mkLocIdxed tsthkLoc L.TestHookRelease i
--                                    in M.insert tstLoc tsthkLoc
--                                         . M.insert tsthkRloc tsthkLoc
--                                         $ M.insert tsthkLoc fxThrdHkloc mp'
--                               )
--                               mp
--                               [0 .. (pred $ length tTests)]

--                           testhkloc = mkLoc fxThrdHkloc L.TestHook
--                           testhkReleaseLoc = mkLoc fxThrdHkloc L.TestHook
--                        in -- M.insert child parent testhkloc .
--                           insertTstTstHkRelease
--                             . M.insert fxThrdhkReleaseloc fxThrdHkloc
--                             . M.insert fxThrdHkloc fxOnceHkloc
--                             . M.insert fxOnceHkReleaseloc fxOnceHkloc
--                             . M.insert fxOnceHkloc fxloc
--                             $ accm {--- fixture loc added-}
--              in (cl, accm')
--         )

fail :: (Show a) => a -> c
fail = error . txt

-- lookupThrow :: (Ord k, Show k, Show v) => Text -> M.Map k v -> k -> v
-- lookupThrow msg m k =
--   (m M.!? k)
--     & fromMaybe
--       (errorS $ toS msg <> "\n" <> ppShow k <> " not found in " <> ppShow m)

-- getTag :: Loc -> Text
-- getTag = \case
--   Root -> "ROOT"
--   Node {tag = t} -> t

-- data EvInfo = EvInfo
--   { eiTag :: Text,
--     eiEventType :: ExeEventType,
--     eiStartEnd :: StartEnd,
--     eiLoc :: Loc
--   }
--   deriving (Show)

-- testTags :: [IOProps] -> [Text]
-- testTags tTests = (\idx -> "Test :: " <> txt idx) <$> take (length tTests) [0 ..]

-- chkFixturesContainTests :: Template -> [[ExeEvent]] -> IO ()
-- chkFixturesContainTests root tevts =
--   chkEq'
--     "fixtures do not contain all expected tests:"
--     expected
--     (actual tevts)
--   where
--     expected =
--       foldTemplate
--         M.empty
--         ( \acc -> \case
--             TGroup {} -> acc
--             TOnceHook {} -> acc
--             TThreadHook {} -> acc
--             TFixture {tag, tTests} ->
--               M.insert
--                 tag
--                 (ST.fromList $ testTags tTests)
--                 acc
--         )
--         root

--     actual :: [[ExeEvent]] -> M.Map Text (ST.Set Text)
--     actual evts =
--       snd $
--         foldl'
--           ( \(mFxTag, fxTstMap) EvInfo {eiEventType = et, eiTag, eiStartEnd, eiLoc} ->
--               et
--                 & ( \case
--                       L.Fixture ->
--                         eiStartEnd & \case
--                           IsStart ->
--                             mFxTag
--                               & maybe
--                                 ( Just eiTag,
--                                   M.member eiTag fxTstMap
--                                     ? fxTstMap
--                                     $ M.insert eiTag ST.empty fxTstMap
--                                 )
--                                 ( \title ->
--                                     errorS $
--                                       "overlapping fixtures in same thread. Starting fixture with loc: "
--                                         <> ppShow eiLoc
--                                         <> " while fixture with tag is still running"
--                                         <> toS title
--                                 )
--                           IsEnd ->
--                             mFxTag
--                               & maybe
--                                 (errorS $ "fixture end found when no fixture is open - fixture end with loc: " <> ppShow eiLoc)
--                                 ( \startTag' ->
--                                     (startTag' == eiTag)
--                                       ? (Nothing, fxTstMap)
--                                       $ errorS ( "fixture start and end tags don't match for start tag: "
--                                             <> toS startTag'
--                                             <> "and end tag: "
--                                             <> toS eiTag
--                                             <> "at loc:\n"
--                                             <> ppShow eiLoc
--                                         )
--                                         -- check parent contains tag
--                                         --  (Nothing, fxTstMap)
--                                 )
--                       L.Test ->
--                         mFxTag
--                           & maybe
--                             (errorS "test encountered when fuixture not open")
--                             ( \title ->
--                                 (mFxTag, M.insert title (ST.insert eiTag (fxTstMap M.! title)) fxTstMap)
--                             )
--                       _ -> errorS "this event should have been filtered out"
--                   )
--           )
--           (Nothing, M.empty)
--           serialisedTestFixInfo

--     serialisedTestFixInfo =
--       -- filter for fixture start & ends and test starts
--       filter
--         ( \EvInfo {eiEventType = et, eiStartEnd} ->
--             et `elem` [L.Test, L.Fixture]
--               && not (et == L.Test && eiStartEnd == IsEnd)
--         )
--         (boundaryEvents Nothing $ join tevts)

-- actualChildParentMap :: [ExeEvent] -> SThreadId -> M.Map Loc Loc
-- actualChildParentMap evs tid =
--   let (_, _, result) =
--         foldl'
--           ( \(openParents, openEvents, mp) (i, e) ->
--               e
--                 & ( \case
--                       se@(Start eet childLoc n sti) ->
--                         let etLoc = (eet, childLoc)
--                             oevts = etLoc : openEvents
--                             nxtparents =
--                               releaseEventType eet
--                                 & maybe
--                                   openParents
--                                   (const $ etLoc : openParents)
--                             parentLoc =
--                               openParents & \case
--                                 [] -> Root
--                                 (_, ploc) : ps -> ploc
--                          in ( nxtparents,
--                               oevts,
--                               M.insert childLoc parentLoc mp
--                             )
--                       --
--                       ev@End
--                         { eventType,
--                           loc = endLoc,
--                           idx,
--                           threadId
--                         } ->
--                           let nxtOpenParents =
--                                 openEventType eventType
--                                   & maybe
--                                     openParents
--                                     ( \oet ->
--                                         openParents
--                                           & \case
--                                             [] -> errorS $ "event does not have parent:\n" <> ppShow ev
--                                             p@(pet, ploc) : ps ->
--                                               (oet == pet)
--                                                 ? ps
--                                                 $ errorS
--                                                   ( "child event type does not match parent:\nchild loc is:\n "
--                                                       <> ppShow endLoc
--                                                       <> "\nparent loc is\n"
--                                                       <> ppShow ploc
--                                                   )
--                                     )
--                            in openEvents
--                                 & \case
--                                   [] -> errorS $ "event ended without start:\n" <> ppShow e
--                                   (evtt, evLoc) : oevs ->
--                                     (endLoc == evLoc)
--                                       ? ( nxtOpenParents,
--                                           oevs,
--                                           mp
--                                         )
--                                       $ errorS
--                                         ( "end event out of order - end event was\n"
--                                             <> ppShow e
--                                             <> "\nbut start event loc was:\n"
--                                             <> ppShow evLoc
--                                         )
--                       e' -> errorS $ "this event should be a Start or End threadStartEnds hasn't worked: " <> P.show e'
--                   )
--           )
--           ([], [], M.empty)
--           (zip [0 ..] threadStartEnds)
--    in result
--   where
--     threadStartEnds =
--       filter
--         ( \case
--             StartExecution {} -> False
--             Start {threadId, eventType} -> include' threadId eventType
--             End {threadId, eventType} -> include' threadId eventType
--             Failure {} -> False
--             ParentFailure {} -> False
--             ApLog {} -> False
--             EndExecution n sti -> False
--         )
--         evs
--     include' = eventBelongsToThread tid

--     openEventType = \case
--       L.FixtureThreadHookRelease -> Just L.FixtureThreadHook
--       L.FixtureOnceHookRelease -> Just L.FixtureOnceHook
--       L.OnceHookRelease -> Just L.OnceHook
--       L.ThreadHookRelease -> Just L.ThreadHook
--       L.TestHookRelease -> Just L.TestHook
--       L.Fixture -> Just L.Fixture
--       L.Group -> Just L.Group
--       L.OnceHook -> Nothing
--       L.ThreadHook -> Nothing
--       L.TestHook -> Nothing
--       L.FixtureThreadHook -> Nothing
--       L.FixtureOnceHook -> Nothing
--       L.Test -> Nothing

--     releaseEventType = \case
--       L.OnceHook -> Just L.OnceHookRelease
--       L.ThreadHook -> Just L.ThreadHookRelease
--       L.TestHook -> Just L.TestHookRelease
--       L.FixtureThreadHook -> Just L.FixtureThreadHookRelease
--       L.FixtureOnceHook -> Just L.OnceHookRelease
--       L.Fixture -> Just L.Fixture
--       L.Group -> Just L.Group
--       L.FixtureThreadHookRelease -> Nothing
--       L.FixtureOnceHookRelease -> Nothing
--       L.OnceHookRelease -> Nothing
--       L.ThreadHookRelease -> Nothing
--       L.TestHookRelease -> Nothing
--       L.Test -> Nothing

-- data StartEnd = IsStart | IsEnd deriving (Show, Eq)

-- boundaryEvents :: Maybe StartEnd -> [ExeEvent] -> [EvInfo]
-- boundaryEvents startEndFilter = mapMaybe (boundaryInfo startEndFilter)

-- boundaryInfo :: Maybe StartEnd -> ExeEvent -> Maybe EvInfo
-- boundaryInfo startEndFilter = \case
--   StartExecution {} -> Nothing
--   Start {loc, eventType} ->
--     (fromMaybe IsStart startEndFilter == IsStart)
--       ? Just (EvInfo (getTag loc) eventType IsStart loc)
--       $ Nothing
--   End {loc, eventType} ->
--     (fromMaybe IsEnd startEndFilter == IsEnd)
--       ? Just (EvInfo (getTag loc) eventType IsEnd loc)
--       $ Nothing
--   Failure {} -> Nothing
--   ParentFailure {} -> Nothing
--   ApLog {} -> Nothing
--   EndExecution {} -> Nothing

-- threadIds :: [ExeEvent] -> [SThreadId]
-- threadIds thrdEvts = nub $ (.threadId) <$> thrdEvts

-- -- check immediate parent (preceeding start or following end) of each thread element
-- -- ignoring non threaded events when checking tests ignore other test start / ends
-- chkParentOrder :: Template -> [ExeEvent] -> IO ()
-- chkParentOrder rootTpl thrdEvts =
--   traverse_
--     ( \threadid ->
--         let actual = actualCPMap threadid
--          in M.traverseWithKey
--               ( \c p ->
--                   chkEq'
--                     ("actual parent child differs for child loc: " <> toS (ppShow c))
--                     (lookupThrow "actual child not found in expected map" expectedCPMap c)
--                     p
--               )
--               actual
--     )
--     (threadIds thrdEvts)
--   where
--     actualCPMap = actualChildParentMap thrdEvts
--     expectedCPMap = childToParentMap rootTpl

-- templateCount :: Template -> (Template -> Int) -> Int
-- templateCount t templateInc = foldTemplate 0 (\c t' -> c + templateInc t') t

-- mkPrenode :: Int -> Template -> IO (PreNode oi ti)
-- mkPrenode maxThreads =
--   let runThreaded lggr propsLst = do
--         prps <- atomically $ do
--           plst <- readTVar propsLst
--           case plst of
--             [] -> errorS "test config or test utils wrong - more calls to tHook or tRelease function than configured"
--             x : xs -> writeTVar propsLst xs >> pure x
--         ioAction lggr prps
--    in \case
--         TGroup
--           { tag,
--             tChilds
--           } -> uu
--         TOnceHook
--           { tag,
--             sHook,
--             sRelease,
--             tChild
--           } -> do
--             chld <- mkPrenode maxThreads tChild
--             pure $
--               PN.OnceHook
--                 { title = tag,
--                   hook = \_loc lg _in -> ioAction lg sHook,
--                   onceSubNodes = chld,
--                   hookRelease = \_loc lg _in -> ioAction lg sHook
--                 }
--         TThreadHook
--           { tag,
--             tHook,
--             tRelease,
--             tChild
--           } -> do
--             chld <- mkPrenode maxThreads tChild
--             thrdHks <- newTVarIO tHook
--             thrdHkRs <- newTVarIO tRelease
--             pure $
--               PN.ThreadHook
--                 { title = tag,
--                   threadHook = \_loc lg _in _ti -> runThreaded lg thrdHks, -- :: Loc -> ApLogger -> oi -> ti -> IO to,
--                   threadSubNodes = chld,
--                   threadHookRelease = \_loc lg _tsto -> runThreaded lg thrdHkRs
--                 }
--         TFixture
--           { tag,
--             sHook,
--             sRelease,
--             tHook,
--             tRelease,
--             tTestHook,
--             tTestRelease,
--             tTests
--           } ->
--             do
--               thrdHks <- newTVarIO tHook
--               thrdHkRs <- newTVarIO tRelease
--               tstHks <- newTVarIO tHook
--               tstHkRs <- newTVarIO tRelease
--               pure
--                 PN.Fixtures
--                   { onceFxHook = \_loc lg _in -> ioAction lg sHook,
--                     onceFxHookRelease = \_loc lg _in -> ioAction lg sRelease,
--                     threadFxHook = \_loc lg _oo _ti -> runThreaded lg thrdHks,
--                     threadFxHookRelease = \_loc lg _to -> runThreaded lg thrdHkRs,
--                     testHook = \_loc lg _oo _to -> runThreaded lg tstHks,
--                     testHookRelease = \_loc lg _tsto -> runThreaded lg tstHkRs,
--                     title = tag,
--                     fixtures = mkTest <$> tTests
--                   }

-- TODO - add tests add to pyrelude
groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f =
  M.elems . foldl' fld M.empty . reverse
 where
  fld m a =
    M.lookup (f a) m
      & maybe
        (M.insert (f a) [a] m)
        (\as -> M.insert (f a) (a : as) m)

-- -- TODO - better formatting chkEq pyrelude
chkEqfmt' :: (Eq a, Show a) => a -> a -> Text -> IO ()
chkEqfmt' e a msg = chkEq' msg e a

chkEq' :: (Eq a, Show a) => Text -> a -> a -> IO ()
chkEq' msg e a =
  when (e /= a) $
    fail $
      "\n"
        <> toS msg
        <> "\n"
        <> "equality check failed:\n"
        <> "Expected:\n  "
        <> ppShow e
        <> "\nDoes not Equal:\n  "
        <> ppShow a
        <> "\n"

chkThreadLogsInOrder :: (Show a, Show l) => [ExeLog l a] -> IO ()
chkThreadLogsInOrder evts =
  do
    T.chk' "Nothing found in heads - groupOn error this should not happen" (all isJust heads)
    traverse_ (chkEq' "first index of thread should be 0" 0 . (.idx)) $ catMaybes heads
    traverse_ chkIds threads
 where
  threads = groupOn (.threadId) evts
  heads = head <$> threads
  chkIds evts' =
    for_
      (zip evts' $ drop 1 evts')
      ( \(ev1, ev2) ->
          let idx1 = ev1.idx
              idx2 = ev2.idx
           in chkEqfmt' (succ idx1) idx2 $
                "event idx not consecutive\n"
                  <> toS (ppShow ev1)
                  <> "\n"
                  <> toS (ppShow ev2)
      )

chkStartEndExecution :: (Show a, Show l) => [ExeLog l a] -> IO ()
chkStartEndExecution evts =
  (,) <$> L.head evts <*> L.last evts
    & maybe
      (fail "no events")
      ( \(s, e) -> do
          s.event & \case
            StartExecution{} -> pure ()
            _ -> fail $ "first event is not StartExecution:\n " <> toS (ppShow s)
          e.event & \case
            EndExecution{} -> pure ()
            _ -> fail $ "last event is not EndExecution:\n " <> toS (ppShow e)
      )

{-
TODO ::
  - remove end events
  - fix naming of nested test hooks (duplicate names)
  - flag to turn off in place logging
  - make loc a list
  - map to different object for logging
  - add test for thread hook
    - threadid back to int
    - redisplay loc
  - does a log sink have toalways be io?
-}

-- $> unit_group_fixture_with_hooks
unit_group_fixture_with_hooks :: IO ()
unit_group_fixture_with_hooks = runTest 1 simpleGroupWithHooks

-- $ > unit_single_fixture
unit_single_fixture :: IO ()
unit_single_fixture = runTest 1 singleFixture

passPropsSingleton :: NonEmpty IOProps
passPropsSingleton = singleton $ IOProps 0 Pass

passProps :: IOProps
passProps = IOProps 0 Pass

baseFx :: TFixture
baseFx =
  TFixture
    { maxThreads = Just 1
    , onceHook = OnceNone
    , threadHook = None
    , testHook = None
    , tests = passPropsSingleton
    }

fxWithHooks :: TFixture
fxWithHooks =
  baseFx
    { onceHook = OnceAround passProps passProps
    , threadHook = Around passPropsSingleton passPropsSingleton
    , testHook = Around passPropsSingleton passPropsSingleton
    }

singleFixture :: Template
singleFixture =
  TFixtures
    { threadLimit = Just 1
    , testHook = None
    , fixtures = singleton baseFx
    }

simpleGroupWithHooks :: Template
simpleGroupWithHooks =
  TGroup
    { threadLimit = Just 1
    , onceHook = OnceAround passProps passProps
    , threadHook = Around passPropsSingleton passPropsSingleton
    , subNodes =
        singleton $
          TFixtures
            { threadLimit = Just 1
            , testHook = Around passPropsSingleton passPropsSingleton
            , fixtures = singleton fxWithHooks
            }
    }

-- isStart :: ExeEventType -> ExeEvent -> Bool
-- isStart et = \case
--   StartExecution {} -> False
--   Start {eventType} -> eventType == et
--   End {} -> False
--   Failure {} -> False
--   ParentFailure {} -> False
--   ApLog {} -> False
--   EndExecution {} -> False

-- chkMaxThreads :: Int -> [[ExeEvent]] -> IO ()
-- chkMaxThreads mxThrds threadedEvents =
--   -- TODO: this should be 1 but is a workaround for debug using the base thread when set up in the test
--   -- fix when logging is fully integrated with test
--   let baseThrds = 2 -- should be 1
--       allowed = mxThrds + baseThrds
--    in T.chk' -- TODO: chk' formatting
--         ( "max execution threads + "
--             <> txt baseThrds
--             <> ": "
--             <> txt allowed
--             <> " exceeded: "
--             <> txt (length threadedEvents)
--             <> "\n"
--             <> txt (ppShow (((.threadId) <$>) <$> threadedEvents))
--         )
--         $ length threadedEvents <= allowed -- baseThrds + exe threads

-- partialLoc :: ExeEvent -> Loc
-- partialLoc = \case
--   StartExecution {} -> boom "StartExecution"
--   Start {loc} -> loc
--   End {loc} -> loc
--   Failure {loc} -> loc
--   ParentFailure {loc} -> loc
--   ApLog {} -> boom "ApLog"
--   EndExecution {} -> boom "EndExecution"
--   where
--     boom msg = errorS $ "BOOM - partialLoc called on: " <> msg <> " which does not have a loc property"

-- startTag :: ExeEvent -> Text
-- startTag =
--   ( \case
--       Root -> errorS "BOOM - Root loc Passed to startTag"
--       Node {tag} -> tag
--   )
--     . partialLoc

-- chkTestEvtsConsecutive :: [ExeEvent] -> IO ()
-- chkTestEvtsConsecutive evs =
--   let tsmevts = Unsafe.read . toS . Txt.takeWhileEnd (/= ':') . startTag <$> filter (isStart L.Test) evs
--       chkidxless :: Int -> Int -> IO Int
--       chkidxless i1 i2 = i1 >= i2 ? errorS "test index out of order" $ pure i2
--    in null tsmevts
--         ? pure ()
--         $ foldM_ chkidxless (-1) tsmevts

-- chkLeafEventsStartEnd :: ExeEventType -> [[ExeEvent]] -> IO ()
-- chkLeafEventsStartEnd targetEventType =
--   traverse_ chkEvents
--   where
--     chkEvents :: [ExeEvent] -> IO ()
--     chkEvents evts' = do
--       foldl' chkEvent Nothing evts'
--         & maybe
--           (pure ())
--           (\fx -> errorS $ targStr <> " started not ended in same thread\n" <> ppShow fx)
--       chkTestEvtsConsecutive evts'

--     targStr = P.show targetEventType
--     matchesTarg = (targetEventType ==)

--     chkEvent :: Maybe Loc -> ExeEvent -> Maybe Loc
--     chkEvent mTstLoc evt =
--       mTstLoc
--         & maybe
--           ( -- outside fixture
--             case evt of
--               StartExecution {} -> Nothing
--               Start {eventType, loc} -> chkOutEventStartEnd True loc eventType
--               End {eventType, loc} -> chkOutEventStartEnd False loc eventType
--               Failure {} -> Nothing
--               ParentFailure {} -> Nothing
--               ApLog {} -> Nothing
--               EndExecution {} -> Nothing
--           )
--           ( -- within fixture
--             \tstLoc ->
--               case evt of
--                 StartExecution {} -> failIn "StartExecution"
--                 Start {eventType, loc} -> chkInEventStartEnd True tstLoc loc eventType
--                 End {eventType, loc} -> chkInEventStartEnd False tstLoc loc eventType
--                 Failure {} -> mTstLoc
--                 ParentFailure {} -> mTstLoc
--                 ApLog {} -> mTstLoc
--                 EndExecution {} -> failIn "EndExecution"
--           )
--       where
--         chkOutEventStartEnd :: Bool -> Loc -> ExeEventType -> Maybe Loc
--         chkOutEventStartEnd isStart' evtLoc eiEventType =
--           matchesTarg eiEventType
--             ? ( isStart'
--                   ? Just evtLoc
--                   $ fail ("End " <> strTrg <> " when not started")
--               )
--             $ Nothing

--         chkInEventStartEnd :: Bool -> Loc -> Loc -> ExeEventType -> Maybe Loc
--         chkInEventStartEnd isStart' activeFxLoc evtLoc evt' =
--           let sevt = P.show evt'
--            in if
--                   | matchesTarg evt' ->
--                       ( isStart'
--                           ? fail ("Nested " <> sevt <> " - " <> sevt <> " started when a " <> sevt <> " is already running in the same thread")
--                           $ activeFxLoc
--                             == evtLoc
--                             ? Nothing
--                           $ fail (strTrg <> " end loc does not match " <> strTrg <> " start loc")
--                       )
--                   | isFixtureChild evt' -> Just activeFxLoc
--                   | otherwise -> failIn sevt

--         fail' msg = errorS $ msg <> "\n" <> ppShow evt
--         strTrg = P.show targetEventType
--         failIn s = fail' $ s <> " must only occur outside a " <> strTrg <> " in same thread"

-- onceEventTypes :: [ExeEventType]
-- onceEventTypes = filter L.isOnceEvent enumList

-- threadedEventTypes :: [ExeEventType]
-- threadedEventTypes = filter L.isThreadedEvent enumList

-- chkSingletonLeafEventsStartEnd :: [ExeEvent] -> IO ()
-- chkSingletonLeafEventsStartEnd evts =
--   traverse_ (`chkLeafEventsStartEnd` [evts]) onceEventTypes

-- chkThreadLeafEventsStartEnd :: [[ExeEvent]] -> IO ()
-- chkThreadLeafEventsStartEnd evts =
--   traverse_ (`chkLeafEventsStartEnd` evts) threadedEventTypes

-- chkFixtureChildren :: [[ExeEvent]] -> IO ()
-- chkFixtureChildren =
--   traverse_ chkEvents
--   where
--     chkEvents :: [ExeEvent] -> IO ()
--     chkEvents evts' =
--       foldl' chkEvent Nothing evts'
--         & maybe
--           (pure ())
--           (\fx -> errorS $ "Fixture started not ended in same thread\n" <> ppShow fx)

--     chkEvent :: Maybe Loc -> ExeEvent -> Maybe Loc
--     chkEvent mActiveFixLoc evt =
--       mActiveFixLoc
--         & maybe
--           ( -- outside fixture
--             evt & \case
--               StartExecution {} -> Nothing
--               Start {eventType, loc} -> chkOutOfFixtureStartEnd True loc eventType
--               End {eventType, loc} -> chkOutOfFixtureStartEnd False loc eventType
--               Failure {} -> Nothing
--               ParentFailure {} -> Nothing
--               ApLog {} -> Nothing
--               EndExecution {} -> Nothing
--           )
--           ( -- within fixture
--             \activeFxLoc ->
--               evt & \case
--                 StartExecution {} -> failIn "StartExecution"
--                 Start {eventType, loc} -> chkInFixtureStartEnd True activeFxLoc loc eventType
--                 End {eventType, loc} -> chkInFixtureStartEnd False activeFxLoc loc eventType
--                 Failure {} -> mActiveFixLoc
--                 ParentFailure {} -> mActiveFixLoc
--                 ApLog {} -> mActiveFixLoc
--                 EndExecution {} -> failIn "EndExecution"
--           )
--       where
--         fail' msg = errorS $ msg <> "\n" <> ppShow evt
--         failOut et = fail' $ P.show et <> " must occur within start / end of fixture in same thread"

--         failIn :: Show a => a -> Maybe Loc
--         failIn et = fail' $ P.show et <> " must only occur outside a fixture in same thread"

--         chkOutOfFixtureStartEnd :: Bool -> Loc -> ExeEventType -> Maybe Loc
--         chkOutOfFixtureStartEnd isStart' evtLoc et =
--           -- the err events should not occur unless a fixture is running in the same thread
--           et & \case
--             L.OnceHook -> Nothing
--             OnceHookRelease -> Nothing
--             L.ThreadHook -> Nothing
--             ThreadHookRelease -> Nothing
--             L.Group -> Nothing
--             L.TestHook -> err
--             TestHookRelease -> err
--             L.Fixture ->
--               isStart'
--                 ? Just evtLoc
--                 $ fail "End fixture when fixture not started"
--             L.Test -> err
--             L.FixtureOnceHook -> err
--             L.FixtureOnceHookRelease -> err
--             L.FixtureThreadHook -> err
--             L.FixtureThreadHookRelease -> err
--           where
--             err = failOut et

--         chkInFixtureStartEnd :: Bool -> Loc -> Loc -> ExeEventType -> Maybe Loc
--         chkInFixtureStartEnd isStart' activeFxLoc evtLoc et =
--           -- the err events should not occur when a fixture is running in the same thread
--           et & \case
--             L.OnceHook -> err
--             OnceHookRelease -> err
--             L.ThreadHook -> err
--             ThreadHookRelease -> err
--             L.TestHook -> mActiveFixLoc
--             TestHookRelease -> mActiveFixLoc
--             L.Group -> err
--             L.Fixture ->
--               if isStart'
--                 then fail "Nested fixtures - fixture started when a fixture is already running in the same thread"
--                 else
--                   (activeFxLoc == evtLoc)
--                     ? Nothing
--                     $ fail "fixture end loc does not match fixture start loc"
--             L.Test {} -> mActiveFixLoc
--             L.FixtureOnceHook -> mActiveFixLoc
--             L.FixtureOnceHookRelease -> mActiveFixLoc
--             L.FixtureThreadHook -> mActiveFixLoc
--             L.FixtureThreadHookRelease -> mActiveFixLoc
--           where
--             err = failIn et

-- countStarts :: ExeEventType -> [ExeEvent] -> Int
-- countStarts et = count (isStart et)

-- countLocSets :: [ExeEvent] -> ExeEventType -> Int
-- countLocSets evs et =
--   -- for threaded events ie. not Once* events the number of occurances
--   -- is non-determiistic because it may occur on 1..n threads (e.g. the same fixture starts
--   -- on 3 threads) but we can count the number of different locs and compare that to
--   -- the run template
--   ST.size $
--     foldl'
--       (\s ev -> isStart et ev ? ST.insert ev.loc s $ s)
--       ST.empty
--       evs

-- chkLocCount :: Template -> [ExeEvent] -> (Template -> Int) -> ExeEventType -> IO ()
-- chkLocCount t evs templateCounter et =
--   chkEq' (txt et) (templateCount t templateCounter) (countLocSets evs et)

-- threadedBoundary :: ExeEventType -> Bool
-- threadedBoundary = \case
--   L.OnceHook -> False
--   OnceHookRelease -> False
--   L.ThreadHook -> True
--   ThreadHookRelease -> True
--   L.TestHook -> True
--   TestHookRelease -> True
--   L.Group -> True
--   L.Fixture -> True
--   L.Test -> True
--   L.FixtureOnceHook -> False
--   L.FixtureOnceHookRelease -> False
--   L.FixtureThreadHook -> True
--   L.FixtureThreadHookRelease -> True

-- chkStartEndIntegrity :: [[ExeEvent]] -> IO ()
-- chkStartEndIntegrity =
--   traverse_ chkThread
--   where
--     chkThread :: [ExeEvent] -> IO ()
--     chkThread evts =
--       unless (M.null r)
--         . errorS
--         $ "events still open when thread finalised\n" <> ppShow r
--       where
--         r = foldl' chkEvent M.empty evts

--     chkEvent :: M.Map Loc (ST.Set Loc) -> ExeEvent -> M.Map Loc (ST.Set Loc)
--     chkEvent acc evt =
--       evt & \case
--         StartExecution {} -> acc
--         Start {eventType} ->
--           threadedBoundary eventType
--             ? chkStart acc evt
--             $ acc
--         End {eventType} ->
--           threadedBoundary eventType
--             ? chkEnd acc evt
--             $ acc
--         Failure {} -> acc
--         ParentFailure {} -> acc
--         ApLog {} -> acc
--         EndExecution {} -> acc

--     chkStart :: M.Map Loc (ST.Set Loc) -> ExeEvent -> M.Map Loc (ST.Set Loc)
--     chkStart m e =
--       M.member eLoc m
--         ? errorS ("Duplicate start events in same thread\n " <> ppShow e)
--         $ M.insert eLoc ST.empty (ST.insert eLoc <$> m)
--       where
--         eLoc = partialLoc e

--     chkEnd :: M.Map Loc (ST.Set Loc) -> ExeEvent -> M.Map Loc (ST.Set Loc)
--     chkEnd m e =
--       M.lookup eLoc m
--         & maybe
--           (errorS $ "Event end where event not started in same thread\n " <> ppShow e)
--           ( \s ->
--               ST.null s
--                 & P.bool
--                   ( errorS $
--                       "Event ended before child events finished\n Event: \n"
--                         <> ppShow e
--                         <> "\nOpen child locs\n"
--                         <> ppShow s
--                   )
--                   (M.delete eLoc $ ST.delete eLoc <$> m)
--           )
--       where
--         eLoc = partialLoc e

-- data ChkErrAccum = ChkErrAccum
--   { initialised :: Bool,
--     lastStart :: Maybe (Loc, ExeEventType),
--     lastFailure :: Maybe (Loc, PException, ExeEventType),
--     matchedFails :: ST.Set Loc
--   }

-- chkHkReleased :: [ExeEvent] -> ExeEventType -> ExeEventType -> IO ()
-- chkHkReleased evs hkType relType =
--   {-
--     * integrity of start / end and overlapping events tested in other tests
--     * this just tests that every star hook has a corresponding release
--   -}
--   ST.null openHooks
--     ? pure ()
--     $ errorS ("Hooks executed without release: " <> ppShow openHooks)
--   where
--     openHooks = foldl' step ST.empty evs

--     step :: ST.Set Loc -> ExeEvent -> ST.Set Loc
--     step openHks ev =
--       ev
--         & ( \case
--               StartExecution {} -> openHks
--               Start {eventType, loc} ->
--                 if
--                     | eventType == hkType ->
--                         ST.member loc openHks
--                           ? errorS ("the same hook is openned twice" <> ppShow loc)
--                           $ ST.insert loc openHks
--                     | eventType == relType ->
--                         let -- hook releases are always parented by hook
--                             parent' = loc.parent
--                          in ST.member parent' openHks
--                               ? ST.delete parent' openHks
--                               $ errorS ("hook is released that has not been run" <> ppShow loc)
--                     | otherwise -> openHks
--               End {eventType, loc} -> openHks
--               Failure {} -> openHks
--               ParentFailure {} -> openHks
--               ApLog {} -> openHks
--               EndExecution {} -> openHks
--           )

-- chkThreadHksReleased :: [ExeEvent] -> IO ()
-- chkThreadHksReleased evs = do
--   let ckr = chkHkReleased evs
--   ckr L.ThreadHook ThreadHookRelease
--   ckr FixtureThreadHook FixtureThreadHookRelease
--   ckr TestHook TestHookRelease

-- chkOnceHksReleased :: [ExeEvent] -> IO ()
-- chkOnceHksReleased evs = do
--   let ckr = chkHkReleased evs
--   ckr L.OnceHook OnceHookRelease
--   ckr FixtureOnceHook FixtureOnceHookRelease

-- data Fail = Fail {floc :: Loc, exception :: PException} | ParentFail {floc :: Loc, ploc :: Loc, exception :: PException} deriving (Show, Eq, Ord)

-- eventBelongsToThread :: SThreadId -> SThreadId -> ExeEventType -> Bool
-- eventBelongsToThread targetId eventThrdId = (||) (eventThrdId == targetId) . isOnceEvent

-- chkErrorPropagation :: [ExeEvent] -> IO ()
-- chkErrorPropagation evts =
--   traverse_ reconcileParents $ threadIds evts
--   where
--     reconcileParents :: SThreadId -> IO ()
--     reconcileParents tid =
--       let cpMap' = cpMap tid
--           failMap = fails $ Just tid
--           failsAllThreads = fails Nothing
--           -- get the parent or if it is a grouping event
--           -- get it's paraent Grouping events (Groups and Fixtures)
--           --  are effectively ignored in the errorS propagation as they
--           -- themsleves neither Pass or fail
--           truParent :: Loc -> Loc
--           truParent parentLoc =
--             let nxtParent = truParent $ lookupThrow "parent not found in child parent map" cpMap' parentLoc
--                 parentIsGrouping = isGrouping $ lookupThrow "loc not found in event map" evtTypeMap parentLoc
--              in (parentLoc == Root)
--                   ? Root
--                   $ (parentIsGrouping ? nxtParent $ parentLoc)
--        in traverse_
--             ( \(chldLoc, pLoc) ->
--                 let trueParentFailure = failMap M.!? truParent pLoc
--                  in isGrouping (lookupThrow "loc not found in event map" evtTypeMap chldLoc)
--                       ? pure ()
--                       $ failMap M.!? chldLoc
--                         & maybe
--                           ( -- the child event Passed so parent must have Passed
--                             trueParentFailure
--                               & maybe
--                                 (pure ())
--                                 ( errorS $
--                                     "Child event Passed when parent failed - errorS should have propagated\nchild\n"
--                                       <> ppShow chldLoc
--                                       <> "\nparent\n"
--                                       <> ppShow (truParent pLoc)
--                                 )
--                           )
--                           ( -- the child event failed
--                             \case
--                               Fail {floc = childLoc, exception = childExcption} ->
--                                 trueParentFailure
--                                   & maybe
--                                     (pure ())
--                                     (errorS $ "Child event failed (not propagated parent failure) when parent failed - parent errorS should have propagated\n" <> ppShow childLoc)
--                               ParentFail {floc = childloc, ploc, exception = childException} ->
--                                 let prntFail = trueParentFailure
--                                  in prntFail
--                                       & maybe
--                                         ( {-
--                                             allow for special condition where onceHook has been failed due to failure in a
--                                             concurrent thread which means onceHook will have propagated failure but parent
--                                             in this thread wont
--                                           -}
--                                           isOnceEvent (lookupThrow "eventType not found" evtTypeMap childloc)
--                                             ? errorS "Not implemented"
--                                             $ errorS ("Child event has propagated parent failure when parent has not failed\n" <> ppShow childloc)
--                                         )
--                                         ( \p ->
--                                             let pexcpt = p.exception
--                                              in chkEq'
--                                                   ( toS $
--                                                       "Propagated excption does not equal parent exception for loc:\n"
--                                                         <> ppShow childloc
--                                                         <> "\nchild exception\n"
--                                                         <> ppShow childException
--                                                         <> "\nparent exception\n"
--                                                         <> ppShow pexcpt
--                                                   )
--                                                   childException
--                                                   pexcpt
--                                         )
--                           )
--             )
--             (M.toList cpMap')

--     thrdIds = threadIds evts
--     evtTypeMap =
--       foldl'
--         ( \acc ->
--             \case
--               Start eet loc _idx sti -> M.insert loc eet acc
--               _ -> acc
--         )
--         M.empty
--         evts
--     cpMap = actualChildParentMap evts

--     fails :: Maybe SThreadId -> M.Map Loc Fail
--     fails targThread =
--       foldl'
--         ( \acc ->
--             let rslt thistid' eventType newAcc =
--                   targThread & maybe
--                     newAcc
--                     \targThread' -> eventBelongsToThread targThread' thistid' eventType ? newAcc $ acc
--              in \case
--                   StartExecution {} -> acc
--                   Start {} -> acc
--                   End {} -> acc
--                   Failure {loc, exception, threadId, parentEventType} ->
--                     rslt threadId parentEventType $ M.insert loc (Fail loc exception) acc
--                   ParentFailure {parentLoc, exception, loc, threadId, parentEventType} ->
--                     rslt
--                       threadId
--                       parentEventType
--                       $ M.insert loc (ParentFail loc parentLoc exception) acc
--                   ApLog {} -> acc
--                   EndExecution {} -> acc
--         )
--         M.empty
--         evts

-- chkOnceEventsAreBlocking :: [ExeEvent] -> IO ()
-- chkOnceEventsAreBlocking evts =
--   T.chk' ("event started not closed:\n" <> toS (ppShow openEvnt)) $ isNothing openEvnt
--   where
--     openEvnt :: Maybe (Loc, ExeEventType)
--     openEvnt = foldl' chkStartEndIntegrity' Nothing evts

--     chkStartEndIntegrity' :: Maybe (Loc, ExeEventType) -> ExeEvent -> Maybe (Loc, ExeEventType)
--     chkStartEndIntegrity' mLcEt evt =
--       evt & \case
--         StartExecution {} -> mLcEt
--         Start {eventType, loc} ->
--           mLcEt
--             & maybe
--               (isOnceEvent eventType ? Just (loc, eventType) $ mLcEt)
--               ( \activeEvt ->
--                   errorS $
--                     "event started while once event still open\nevent:\n"
--                       <> ppShow evt
--                       <> "open once event"
--                       <> ppShow activeEvt
--               )
--         End {eventType, loc} ->
--           if isOnceEvent eventType
--             then
--               mLcEt
--                 & maybe
--                   ( errorS $
--                       "once event ended when once event not open\nevent:\n"
--                         <> ppShow evt
--                   )
--                   ( \locEt ->
--                       locEt
--                         == (loc, eventType)
--                         ? Nothing
--                         $ errorS
--                         $ "end once event does not correspond to start loc / event type\nstart:\n"
--                           <> ppShow locEt
--                           <> "\n"
--                           <> "end:\n"
--                           <> ppShow (loc, eventType)
--                   )
--             else
--               mLcEt
--                 & maybe
--                   mLcEt
--                   ( \activeEvt ->
--                       errorS $
--                         "event ended while once event still open\nevent:\n"
--                           <> ppShow evt
--                           <> "open once event"
--                           <> ppShow activeEvt
--                   )
--         Failure {} -> mLcEt
--         ParentFailure {} -> mLcEt
--         ApLog {} -> mLcEt
--         EndExecution {} -> mLcEt

-- -- T.chk' "" uu
-- -- after start only oe should end
-- -- no event parented by oe should occur beefore oe

-- chkEventCounts :: Template -> [ExeEvent] -> IO ()
-- chkEventCounts t evs = do
--   -- another check checks hook release matches hook so only checking hook starts
--   -- once event check number of start events
--   chkEq' "Once Hook Count" expectedOnceHkCount $ countStarts L.OnceHook evs
--   chkEq' "Fixture Once Hook Count" expectedFixtureCount $ countStarts L.FixtureOnceHook evs
--   -- tests number of start events
--   chkEq' "Test Count" expectedTestCount $ countStarts L.Test evs
--   chkEq' "Test Hooks" expectedTestCount $ countStarts L.TestHook evs

--   -- threaded events check nuber of unique locs of type
--   -- one event could occur n times due to running in more than
--   -- one thread
--   -- fixture
--   chkLocCount' fixtureCounter L.Fixture

--   -- group
--   chkLocCount'
--     ( \case
--         TGroup {} -> 1
--         _ -> 0
--     )
--     L.Group

--   -- threadHook
--   chkLocCount'
--     ( \case
--         TThreadHook {} -> 1
--         _ -> 0
--     )
--     L.ThreadHook

--   -- FixtureThreadHook
--   chkLocCount' fixtureCounter L.FixtureThreadHook
--   where
--     chkLocCount' = chkLocCount t evs
--     templateCount' = templateCount t
--     fixtureCounter = \case
--       TFixture {} -> 1
--       _ -> 0
--     expectedTestCount =
--       templateCount'
--         ( \case
--             TFixture {tTests} -> length tTests
--             _ -> 0
--         )
--     expectedFixtureCount = templateCount' fixtureCounter
--     expectedOnceHkCount =
--       templateCount'
--         ( \case
--             TOnceHook {} -> 1
--             _ -> 0
--         )

-- validateTemplate :: Template -> IO ()
-- validateTemplate t =
--   let tl = (.tag) <$> templateList t
--       utl = nub tl
--    in when (length tl /= length utl) $
--         errorS $
--           "template must be such that tags are unique: \n" <> ppShow tl

-- runTest :: Int -> Template -> IO ()
-- runTest maxThreads template = do
--   validateTemplate template
--   putStrLn ""
--   pPrint template
--   putStrLn "========="
--   chan <- newTChanIO
--   q <- newTQueueIO
--   ior <- newIORef 0
--   tid <- C.myThreadId
--   lc@LogControls {sink, log} <- testLogControls chan q
--   pn <- mkPrenode maxThreads template
--   execute maxThreads lc pn
--   log
--     & maybe
--       (T.chkFail "No Events Log")
--       (\evts -> atomically (q2List evts) >>= chkProperties maxThreads template)

-- mkTest :: IOProps -> PN.Test si ti ii
-- mkTest iop@IOProps {message} = PN.Test message \log a b c -> ioAction log iop

-- noDelay :: DocFunc Int
-- noDelay = DocFunc "No Delay" $ pure 0

-- neverFail :: DocFunc Bool
-- neverFail = DocFunc "Never Fail" $ pure False

-- alwaysFail :: DocFunc Bool
-- alwaysFail = DocFunc "Always Fail" $ pure True

-- superSimplSuite :: Template
-- superSimplSuite =
--   TThreadHook
--     { tag = "ThreadHook",
--       tHook =
--         [ IOProps
--             { message = "ThreadHook",
--               delayms = 1,
--               outcome = Fail
--             }
--         ],
--       tRelease =
--         [ IOProps
--             { message = "ThreadHookRelease",
--               delayms = 1,
--               outcome = Pass
--             }
--         ],
--       tChild =
--         TOnceHook
--           { tag = "OnceHook",
--             sHook =
--               IOProps
--                 { message = "Once Hook",
--                   delayms = 1,
--                   outcome = Pass
--                 },
--             sRelease =
--               IOProps
--                 { message = "Once Hook Release",
--                   delayms = 1,
--                   outcome = Pass
--                 },
--             tChild =
--               TFixture
--                 { tag = "Fixture 0",
--                   --
--                   sHook = testProps "Fixture 0 - Fixture Once Hook" 0 0 Pass,
--                   sRelease = testProps "Fixture 0 - Fixture Once Hook Release" 0 0 Pass,
--                   --
--                   tHook = [testProps "Fixture 0 - Fixture Thread Hook" 0 0 Pass],
--                   tRelease = [testProps "Fixture 0 - Fixture Hook Release" 0 0 Pass],
--                   --
--                   tTestHook = [testProps "Fixture 0 - Test Hook" 0 0 Pass],
--                   tTestRelease = [testProps "Fixture 0 - Test Hook Release" 0 0 Pass],
--                   tTests = [testProps "" 0 0 Pass]
--                 }
--           }
--     }

-- -- $ > unit_simple_single_failure

-- unit_simple_single_failure :: IO ()
-- unit_simple_single_failure =
--   runTest 1 $
--     TFixture
--       { tag = "FX 0",
--         sHook = testProps "Fx 0 - SH" 0 0 Pass,
--         sRelease = testProps "Fx 0 - SHR" 0 0 Pass,
--         tHook = [testProps "Fx 0" 0 0 Pass],
--         tRelease = [testProps "Fx 0" 0 0 Pass],
--         tTestHook = [testProps "Fx 0" 0 0 Pass],
--         tTestRelease = [testProps "Fx 0" 0 0 Pass],
--         tTests = [testProps "" 0 0 Fail]
--       }

-- {-
-- ISSUE

-- 3 threads

-- shk
--   thk
--     shk
--       fx
--       fx
--       fx
--     thk
--       fx
--       fx
--       fx

-- 3 threads

-- shk - t1
--   thk - t1 t2 t3
--     shk - t1
--       fx - t1
--       fx - t2
--       fx - t3
--       thk
--        fx
--        fx
--        fx
--   thk
--     fx
--     fx
--     fx

-- 3 threads

-- shk - t1
--   thk - t1 t2 t3
--     fx
--     shk - t1
--       fx - t1
--       fx - t2
--       fx - t3
--       thk
--        thk
--         fx
--         fx
--         fx
--   thk'
--     fx
--     fx
--     fx

-- - has fixtures
--  - shk - thk
--  - shk - thk'
--  - shk - thk - shk
--  - shk - thk - shk - thk - thk

--  exeBranch:
--     [
--       sk 2 v0 s [] [
--         thk [fx] [
--           shk [fx fx fx] [
--             thk [] [
--                 thk [fx fx fx] [],
--                 thk [fx fx fx] [],
--                 thk [fx fx fx] []
--               ]
--           ]
--         ],
--         thk [
--           fx fx fx
--         ]
--       ]
--     ]

--     RuntimeElement

--    1. run fixture set
--     - thread will not escape fixture set until all fixtures run
--     - thread will not escape sub hooks until all fixtures run
--    2. atomically
--     - find subelem not running
--     - find sub-elem partially not running
--     - nxt index
--    3. run thread

-- - issues
--   % initial distribution
--   % tracking state - pending / running / complete
--   %

-- -- allow for no thread hooks
-- shk - t1
--       fx - t1
--       fx - t2
--       fx - t3
--        fx
--        fx
--        fx
--       shk
--     fx
--     fx
--     fx

-- 1. thread must live longer than one fixture
-- 2. prioritise adjacent fixtures when continuing thread
-- 3. hook finalise when crossing thread hook boundary
-- 4. prioritise distant fixtures when launching new thread
--   % constuct breath first list of thread hooks
--   % ensure wait for thread hooks to finish
--   % test with no thread hooks
--   % test fixtures before thread hooks
--   % test run time of singleton hooks vs thread hooks
--     - will not follow tree structure
--     - n threads

-- -}

-- {- TODO
--   ~ DONE: chkHks

--   ~ branching / thread level
--     ~ new constructors
--       ~ DONE: add branch constructor
--       ~ DONE: add thread level hook constructor
--     ~ structure
--       ~ nested structures - construct
--         ~ DONE: simple branch
--         ~ DONE: nested hook
--         ~ DONE: nested branch hook
--         ~ DONE: nested hook - type changing
--         ~ DONE: nested branch hook - type changing
--         ~ DONE: nested hooks multiple - type changing
--         ~ DONE: nested branches multiple - type changing
--         ~ DONE: check for simplifications
--     ~ !!!!! MISDESIGN FIX
--     ~ implemntation
--       ~ update runtime ~ remove uu completing implementation of thread level hooks
--       ~ add thread level hooks to deep nested structure

--       ~ thread through logging
--         ~ start by deleting logEnd and logStart on fixture
--         ~ add a sink parameter
--         ~ log start end of everything ~ include thread Id
--         ~ update validation

--       ~ reinstate existing tests
--       ~ update validation to include thread level hooks
--       ~ add thread level tests

--     ~ update prepare to not add empty nodes
--       ~ update tests to include empty nodes

--   ~ add iteration level
--     ~ constructors
--     ~ structure
--     ~ implementation
--     ~ reinstate existing tests

--     ~ update validation
--     ~ tests

--   ~ advnced tests

--   ~ hook timeout
--   ~ test with empty:
--     ~ fixtures
--     ~ hooks
--       ~ singleton
--       ~ thread-level
--       ~ iteration level
--   ~ simple multiple hooks / iterations / threads

--   ~ simple exceptions
--     ~ update stats expectations
--     ~ iteration
--     ~ singleton hook
--     ~ thread-level hook
--   ~ simple multi threaded

--   ~ property based - inc differening times / hook types / hook fixture counts
--    ~ import validation
--    ~ write generators
--    ~ write shrinkers

--   ~ killing run
--     ~ implement
--     ~ test
--       ~ make sure tests killed in finalisation don't keep running
--       ~ must not handle async exceptions

--   ~ fixture thread limits
--     ~ update constructors
--     ~ runtime
--     ~ update validations
-- -}
