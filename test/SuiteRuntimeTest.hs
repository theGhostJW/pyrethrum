module SuiteRuntimeTest where

import Check (Checks)
-- TODO Add to Pyrelude
-- TODO Add to Pyrelude

import Control.Monad (Functor ((<$)), void)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import DSL.Interpreter
import Data.Aeson.Encoding (quarter)
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.IntMap.Merge.Lazy as ST
import Data.List.Extra (lookup, notElem, snoc)
import qualified Data.Map.Strict as M
import qualified Data.Set as ST
import qualified Data.Text as Txt
import Data.Yaml
import GHC.Records
import Internal.PreNode
import qualified Internal.PreNode as PN
import Internal.RunTimeLogging as L
  ( ExeEvent (..),
    ExeEventType (..),
    Loc (..),
    LogControls (..),
    PException,
    SThreadId (..),
    endIsTerminal,
    isGrouping,
    isOnceEvent,
    isThreadedEvent,
    mkLogger,
    testLogControls,
  )
import Internal.SuiteRuntime
import qualified Internal.SuiteRuntime as S
import Polysemy
import Pyrelude (Any, ListLike (..), enumList)
import Pyrelude as P
  ( Alternative ((<|>)),
    Applicative ((<*>)),
    Bool (..),
    Category (id),
    Either (Left, Right),
    Enum (succ),
    Eq (..),
    Foldable (foldl, sum),
    IO,
    Int,
    ListLike (all, drop, elem, foldl', foldl1, head, null, takeWhileEnd, unsafeHead, unsafeLast),
    Maybe (Just, Nothing),
    Num ((+)),
    Ord (..),
    Show (show),
    SomeException,
    Text,
    Traversable (sequenceA, traverse),
    bool,
    catMaybes,
    catchAll,
    const,
    count,
    debug,
    debug',
    debug_,
    debugf,
    displayException,
    dropWhile,
    dropWhileEnd,
    either,
    error,
    filter,
    find,
    first,
    flip,
    fmap,
    foldM_,
    foldl1',
    foldr',
    for_,
    fromJust,
    fromMaybe,
    fst,
    groupBy,
    isNothing,
    isPrefixOf,
    join,
    last,
    length,
    lines,
    maybe,
    maybef,
    myThreadId,
    newIORef,
    not,
    nub,
    otherwise,
    pure,
    replace,
    replicateM_,
    reverse,
    sequence,
    sequenceA_,
    singleton,
    snd,
    sortOn,
    threadDelay,
    throw,
    toS,
    traverse_,
    txt,
    txtPretty,
    uncurry,
    unless,
    uu,
    when,
    whenJust,
    zip,
    ($),
    (&),
    (&&),
    (-),
    (.),
    (<$>),
    (<&>),
    (<>),
    (>>),
    (>>=),
    (?),
    (\\),
    (||),
  )
import qualified Pyrelude.Test as T
import TempUtils (debugLines)
import Text.Show.Pretty (PrettyVal (prettyVal), pPrint, pPrintList, ppDocList, ppShow, ppShowList)
import UnliftIO.Concurrent as C
  ( ThreadId,
    forkFinally,
    forkIO,
    myThreadId,
    threadDelay,
  )
import UnliftIO.STM
import Prelude (Ord, String, putStrLn, read)


{-
1. create logger pretty print + list :: Done
2. run most basic :: Done* * - need to fix index in debbug logging - deferred
3. stats gather
4. laws
5. generate
-}

data DocFunc a = DocFunc
  { doc :: Text,
    func :: IO a
  }

instance Show (DocFunc a) where
  show = toS . doc

data IOProps = IOProps
  { message :: Text,
    delayms :: Int,
    fail :: Bool
  }
  deriving (Show)

testProps :: Text -> Int -> Int -> Bool -> IOProps
testProps prefix idx = IOProps (prefix == "" ? txt idx $ prefix <> "." <> txt idx)

data Template
  = TGroup
      { tTag :: Text,
        tChilds :: [Template]
      }
  | TOnceHook
      { tTag :: Text,
        sHook :: IOProps,
        sRelease :: IOProps,
        tChild :: Template
      }
  | TThreadHook
      { tTag :: Text,
        tHook :: [IOProps],
        tRelease :: [IOProps],
        tChild :: Template
      }
  | TFixture
      { tTag :: Text,
        sHook :: IOProps,
        sRelease :: IOProps,
        tHook :: [IOProps],
        tRelease :: [IOProps],
        tTestHook :: [IOProps],
        tTestRelease :: [IOProps],
        tTests :: [IOProps]
      }
  deriving (Show)

type TextLogger = Text -> IO ()

foldTemplate :: forall a. a -> (a -> Template -> a) -> Template -> a
foldTemplate seed f t =
  let tval = f seed t
      recurse = foldTemplate tval f
   in t & \case
        TGroup {tChilds} -> foldl' f tval tChilds
        TOnceHook {tChild} -> recurse tChild
        TThreadHook {tChild} -> recurse tChild
        TFixture {} -> tval

parentMap :: Template -> M.Map Text (Maybe Template)
parentMap t =
  foldTemplate
    (M.singleton (tTag t) Nothing)
    ( \m p ->
        p & \case
          TGroup {tChilds} -> foldl' (\m' c -> M.insert (tTag c) (Just p) m') m tChilds
          TOnceHook {tChild} -> M.insert (tTag tChild) (Just p) m
          TThreadHook {tChild} -> M.insert (tTag tChild) (Just p) m
          TFixture {} -> m
    )
    t

templateList :: Template -> [Template]
templateList = foldTemplate [] (flip (:))

testTags :: Text -> [IOProps] -> [Text]
testTags fxTag tsts =
  (\it -> fxTag <> "." <> txt (fst it)) <$> zip [0 ..] tsts

threadParentMap :: Template -> M.Map Text (Maybe Text)
threadParentMap root =
  (tTag <$>) <$> foldl' insertTests ((>>= threadParent) <$> rootMap) (templateList root)
  where
    rootMap :: M.Map Text (Maybe Template)
    rootMap = parentMap root

    insertTests :: M.Map Text (Maybe Template) -> Template -> M.Map Text (Maybe Template)
    insertTests m = \case
      TGroup {} -> m
      TOnceHook {} -> m
      TThreadHook {} -> m
      -- add an element for each test
      fx@TFixture {tTests, tTag} ->
        foldl'
          (\m' tsttg -> M.insert tsttg (Just fx) m')
          m
          (testTags tTag tTests)

    threadParent :: Template -> Maybe Template
    threadParent tmp =
      fstPrnt isTstHk tmp <|> fstPrnt isThrdHkorGrp tmp
      where
        fstPrnt :: (Template -> Bool) -> Template -> Maybe Template
        fstPrnt pred t =
          pred t
            ? Just t
            $ lookupThrow rootMap (tTag t) >>= fstPrnt pred

        isTstHk :: Template -> Bool
        isTstHk = \case
          TGroup {} -> False
          TOnceHook {} -> False
          TThreadHook {} -> False
          TFixture {} -> False

        isThrdHkorGrp :: Template -> Bool
        isThrdHkorGrp = \case
          TGroup {} -> True
          TOnceHook {} -> False
          TThreadHook {} -> True
          TFixture {} -> False

lookupThrow :: (Ord k, Show k, Show v) => M.Map k v -> k -> v
lookupThrow m k =
  (m M.!? k)
    & maybe
      (error $ show k <> " not found in " <> ppShow m)
      id

getTag :: Loc -> Text
getTag = \case
  Root -> "ROOT"
  Node {tag = t} -> t

data EvInfo = EvInfo
  { eiTag :: Text,
    et :: ExeEventType
  }
  deriving (Show)

chkFixturesContainTests :: Template -> [Template] -> [[ExeEvent]] -> IO ()
chkFixturesContainTests root tList tevts =
  chkEq'
    ( "fixtures do not contain all expected tests\nExpected:\n"
        <> toS (ppShow expected)
        <> "\nActual:\n"
        <> toS (ppShow actual)
    )
    expected
    actual
  where
    pm = threadParentMap root
    templateFixTags = M.fromList $ (,ST.empty) <$> catMaybes (fixTag <$> tList)
    expected =
      foldl'
        ( \m (c, mp) ->
            mp
              & maybe
                m
                ( \p -> M.alter (maybe (Just $ ST.singleton c) (Just . ST.insert c)) p m
                )
        )
        templateFixTags
        $ M.toList pm

    actual = threadActual $ join tevts

    threadActual :: [ExeEvent] -> M.Map Text (ST.Set Text)
    threadActual evts =
      let (openTests, fixAccum) = foldl' step (ST.empty, M.empty) (revStartEvents evts)

          step acc'@(st, mp) EvInfo {eiTag, et} =
            et & \case
              L.OnceHook -> acc'
              L.OnceHookRelease -> acc'
              L.ThreadHook -> acc'
              L.ThreadHookRelease -> acc'
              L.TestHook -> acc'
              L.TestHookRelease -> acc'
              L.Group -> acc'
              L.Fixture ->
                let updateSet = Just . maybe st (ST.union st)
                 in (ST.empty, M.alter updateSet eiTag mp)
              L.Test -> (ST.insert eiTag st, mp)
              L.FixtureOnceHook -> acc'
              L.FixtureOnceHookRelease -> acc'
              L.FixtureThreadHook -> acc'
              L.FixtureThreadHookRelease -> acc'
       in ST.null openTests
            ? fixAccum
            $ error
            $ "tests still open at end of thread" <> ppShow openTests

    fixTag = \case
      TGroup {} -> Nothing
      TOnceHook {} -> Nothing
      TThreadHook {} -> Nothing
      TFixture {tTag} -> Just tTag

actualParentIgnoreTests :: ListLike m EvInfo i => m -> Maybe Text
actualParentIgnoreTests evs = eiTag <$> find (\EvInfo {et = et'} -> et' /= L.Test) evs

revStartEvents :: [ExeEvent] -> [EvInfo]
revStartEvents thrdEvts = catMaybes $ boundryLoc True <$> reverse thrdEvts

boundryLoc :: Bool -> ExeEvent -> Maybe EvInfo
boundryLoc useStart = \case
  StartExecution {} -> Nothing
  Start {loc, eventType} -> useStart ? Just (EvInfo (getTag loc) eventType) $ Nothing
  End {loc, eventType} -> useStart ? Nothing $ Just (EvInfo (getTag loc) eventType)
  Failure {} -> Nothing
  ParentFailure {} -> Nothing
  ApLog {} -> Nothing
  EndExecution {} -> Nothing

-- check immediate parent (preceeding start or following end) of each thread element
-- ignoring non threaded events when checking tests ignore other test start / ends
chkParentOrder :: Template -> [ExeEvent] -> IO ()
chkParentOrder rootTpl thrdEvts =
  chkParents "Parent start event (working back from child)" revStartEvents'
    >> chkParents "Parent end event (working forward from child)" evntEndLocs
  where
    tpm = threadParentMap rootTpl
    revStartEvents' = revStartEvents thrdEvts
    evntEndLocs = catMaybes $ boundryLoc False <$> thrdEvts

    chkParents :: Text -> [EvInfo] -> IO ()
    chkParents errPrefix =
      \case
        [] -> pure ()
        EvInfo {eiTag = tg, et} : evs ->
          let expected = lookupThrow tpm tg
              next = chkParents errPrefix evs
              actualParentIgnoreTests' = actualParentIgnoreTests evs
              actualParent = eiTag <$> head evs
              fail =
                error . toS $
                  toS errPrefix <> "\n  expected (tag): " <> txt expected <> "  \ngot (tag): " <> txt actualParent
              chkParentIncTests = expected == actualParent ? next $ fail
              chkParentExclTests = expected == actualParentIgnoreTests' ? next $ fail
           in case et of
                L.OnceHook -> next
                L.OnceHookRelease -> next
                L.ThreadHook -> chkParentIncTests
                L.ThreadHookRelease -> chkParentIncTests
                L.TestHook -> chkParentIncTests
                L.TestHookRelease -> chkParentIncTests
                L.Group -> chkParentIncTests
                L.Fixture -> chkParentIncTests
                L.Test -> chkParentExclTests
                L.FixtureOnceHook -> next
                L.FixtureOnceHookRelease -> next
                L.FixtureThreadHook -> chkParentIncTests
                L.FixtureThreadHookRelease -> chkParentIncTests

templateCount :: Template -> (Template -> Int) -> Int
templateCount t templateInc = foldTemplate 0 (\c t' -> c + templateInc t') t

mkPrenode :: Int -> Template -> IO (PreNode oi () ti ())
mkPrenode maxThreads =
  let runThreaded lggr propsLst = do
        prps <- atomically $ do
          plst <- readTVar propsLst
          case plst of
            [] -> error "test config or test utils wrong - more calls to tHook or tRelease function than configured"
            x : xs -> writeTVar propsLst xs >> pure x
        ioAction lggr prps
   in \case
        TGroup
          { tTag,
            tChilds
          } -> uu
        TOnceHook
          { tTag,
            sHook,
            sRelease,
            tChild
          } -> do
            chld <- mkPrenode maxThreads tChild
            pure $
              PN.OnceHook
                { hookTag = Just tTag,
                  hook = \_loc lg _in -> ioAction lg sHook,
                  hookChild = chld,
                  hookRelease = \_loc lg _in -> ioAction lg sHook
                }
        TThreadHook
          { tTag,
            tHook,
            tRelease,
            tChild
          } -> do
            chld <- mkPrenode maxThreads tChild
            thrdHks <- newTVarIO tHook
            thrdHkRs <- newTVarIO tRelease
            pure $
              PN.ThreadHook
                { threadTag = Just tTag,
                  threadHook = \_loc lg _in _ti -> runThreaded lg thrdHks, -- :: Loc -> ApLogger -> oi -> ti -> IO to,
                  threadHookChild = chld,
                  threadHookRelease = \_loc lg _tsto -> runThreaded lg thrdHkRs
                }
        TFixture
          { tTag,
            sHook,
            sRelease,
            tHook,
            tRelease,
            tTestHook,
            tTestRelease,
            tTests
          } ->
            do
              thrdHks <- newTVarIO tHook
              thrdHkRs <- newTVarIO tRelease
              tstHks <- newTVarIO tHook
              tstHkRs <- newTVarIO tRelease
              -- let runThreaded lggr propsLst = do
              --       prps <- atomically $ do
              --         plst <- readTVar propsLst
              --         case plst of
              --           [] -> error "test config or test utils wrong - more calls to tHook or tRelease function than configured"
              --           x : xs -> writeTVar propsLst xs >> pure x
              --       ioAction lggr prps
              --  in
              pure
                PN.Fixture
                  { onceFxHook = \_loc lg _in -> ioAction lg sHook,
                    onceFxHookRelease = \_loc lg _in -> ioAction lg sRelease,
                    threadFxHook = \_loc lg _oo _ti -> runThreaded lg thrdHks,
                    threadFxHookRelease = \_loc lg _to -> runThreaded lg thrdHkRs,
                    testHook = \_loc lg _oo _to -> runThreaded lg tstHks,
                    testHookRelease = \_loc lg _tsto -> runThreaded lg tstHkRs,
                    fxTag = Just tTag,
                    iterations = mkTest <$> tTests
                  }

q2List :: TQueue ExeEvent -> STM [ExeEvent]
q2List qu = reverse <$> recurse [] qu
  where
    recurse :: [ExeEvent] -> TQueue ExeEvent -> STM [ExeEvent]
    recurse l q =
      tryReadTQueue q
        >>= P.maybe (pure l) (\e -> recurse (e : l) q)

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

-- TODO - better formatting chkEq pyrelude
chkEqfmt' :: (Eq a, Show a) => a -> a -> Text -> IO ()
chkEqfmt' e a msg = chkEq' msg e a

chkEq' :: (Eq a, Show a) => Text -> a -> a -> IO ()
chkEq' msg e a =
  when (e /= a) $
    error $
      "\n"
        <> toS msg
        <> "\n"
        <> "equality check failed:\n"
        <> "Expected:\n  "
        <> ppShow e
        <> "\nDoes not Equal:\n  "
        <> ppShow a
        <> "\n"

chkThreadLogsInOrder :: [ExeEvent] -> IO ()
chkThreadLogsInOrder evts =
  do
    eachThread
      ( \l ->
          let ck = chkEq' "first index of thread should be 0" 0 . idx
              ev = unsafeHead l
           in ck ev
      )
    eachThread chkIds
  where
    eachThread = for_ threads
    threads =
      groupOn
        threadId
        evts

    chkIds evts' =
      for_
        (zip evts' $ drop 1 evts')
        ( \(ev1, ev2) ->
            let idx1 = idx ev1
                idx2 = idx ev2
             in -- TODO - better formatting chkEq pyrelude
                chkEqfmt' (succ idx1) idx2 $
                  "event idx not consecutive\n"
                    <> toS (ppShow ev1)
                    <> "\n"
                    <> toS (ppShow ev2)
        )

chkStartEndExecution :: [ExeEvent] -> IO ()
chkStartEndExecution evts =
  se
    & maybe
      (error "no events")
      ( \(s, e) -> do
          case s of
            StartExecution {} -> pure ()
            _ -> error "first event is not StartExecution"
          case e of
            EndExecution {} -> pure ()
            _ -> error "last event is not EndExecution"
      )
  where
    se = do
      s <- head evts
      e <- last evts
      pure (s, e)

isStart :: ExeEventType -> ExeEvent -> Bool
isStart et = \case
  StartExecution {} -> False
  Start {eventType} -> eventType == et
  End {} -> False
  Failure {} -> False
  ParentFailure {} -> False
  ApLog {} -> False
  EndExecution {} -> False

chkMaxThreads :: Int -> [[ExeEvent]] -> IO ()
chkMaxThreads mxThrds threadedEvents =
  -- TODO: this should be 1 but is a workaround for debug using the base thread when set up in the test
  -- fix when logging is fully integrated with test
  let baseThrds = 2 -- should be 1
      allowed = mxThrds + baseThrds
   in T.chk' -- TODO: chk' formatting
        ( "max execution threads + "
            <> txt baseThrds
            <> ": "
            <> txt allowed
            <> " exceeded: "
            <> txt (length threadedEvents)
            <> "\n"
            <> txt (ppShow ((threadId <$>) <$> threadedEvents))
        )
        $ length threadedEvents <= allowed -- baseThrds + exe threads

partialLoc :: ExeEvent -> Loc
partialLoc = \case
  StartExecution {} -> boom "StartExecution"
  Start {loc} -> loc
  End {loc} -> loc
  Failure {loc} -> loc
  ParentFailure {loc} -> loc
  ApLog {} -> boom "ApLog"
  EndExecution {} -> boom "EndExecution"
  where
    boom msg = error $ "BOOM - partialLoc called on: " <> msg <> " which does not have a loc property"

startTag :: ExeEvent -> Text
startTag =
  ( \case
      Root -> error "BOOM - Root loc passed to startTag"
      Node {tag} -> tag
  )
    . partialLoc

chkTestEvtsConsecutive :: [ExeEvent] -> IO ()
chkTestEvtsConsecutive evs =
  let tsmevts = read . toS . Txt.takeWhileEnd (/= ':') . startTag <$> filter (isStart L.Test) evs
      chkidxless :: Int -> Int -> IO Int
      chkidxless i1 i2 = i1 >= i2 ? error "test index out of order" $ pure i2
   in null tsmevts
        ? pure ()
        $ foldM_ chkidxless (-1) tsmevts

chkLeafEventsStartEnd :: ExeEventType -> [[ExeEvent]] -> IO ()
chkLeafEventsStartEnd targetEventType =
  traverse_ chkEvents
  where
    chkEvents :: [ExeEvent] -> IO ()
    chkEvents evts' = do
      foldl' chkEvent Nothing evts'
        & maybe
          (pure ())
          (\fx -> error $ targStr <> " started not ended in same thread\n" <> ppShow fx)
      chkTestEvtsConsecutive evts'

    targStr = show targetEventType
    matchesTarg = (targetEventType ==)

    chkEvent :: Maybe Loc -> ExeEvent -> Maybe Loc
    chkEvent mTstLoc evt =
      -- TODO: format on debug'
      mTstLoc
        & maybe
          ( -- outside fixture
            case evt of
              StartExecution {} -> Nothing
              Start {eventType, loc} -> chkOutEventStartEnd True loc eventType
              End {eventType, loc} -> chkOutEventStartEnd False loc eventType
              Failure {} -> Nothing
              ParentFailure {} -> Nothing
              ApLog {} -> Nothing
              EndExecution {} -> Nothing
          )
          ( -- within fixture
            \tstLoc ->
              case evt of
                StartExecution {} -> failIn "StartExecution"
                Start {eventType, loc} -> chkInEventStartEnd True tstLoc loc eventType
                End {eventType, loc} -> chkInEventStartEnd False tstLoc loc eventType
                Failure {} -> mTstLoc
                ParentFailure {} -> mTstLoc
                ApLog {} -> mTstLoc
                EndExecution {} -> failIn "EndExecution"
          )
      where
        chkOutEventStartEnd :: Bool -> Loc -> ExeEventType -> Maybe Loc
        chkOutEventStartEnd isStart' evtLoc evtp =
          matchesTarg evtp
            ? ( isStart'
                  ? Just evtLoc
                  $ fail ("End " <> strTrg <> " when not started")
              )
            $ Nothing

        chkInEventStartEnd :: Bool -> Loc -> Loc -> ExeEventType -> Maybe Loc
        chkInEventStartEnd isStart' activeFxLoc evtLoc evt' =
          let sevt = show evt'
           in matchesTarg evt'
                ? ( isStart'
                      ? fail ("Nested " <> sevt <> " - " <> sevt <> " started when a " <> sevt <> " is alreeady running in the same thread")
                      $ activeFxLoc
                        == evtLoc
                        ? Nothing
                      $ fail (strTrg <> " end loc does not match " <> strTrg <> " start loc")
                  )
                $ failIn sevt

        fail msg = error $ msg <> "\n" <> ppShow evt
        strTrg = show targetEventType
        failIn s = fail $ s <> " must only occur outside a " <> strTrg <> " in same thread"

onceEventTypes :: [ExeEventType]
onceEventTypes = filter L.isOnceEvent enumList

threadedEventTypes :: [ExeEventType]
threadedEventTypes = filter L.isThreadedEvent enumList

chkSingletonLeafEvents :: [ExeEvent] -> IO ()
chkSingletonLeafEvents evts =
  traverse_ (`chkLeafEventsStartEnd` [evts]) onceEventTypes

chkThreadLeafEvents :: [[ExeEvent]] -> IO ()
chkThreadLeafEvents evts =
  traverse_ (`chkLeafEventsStartEnd` evts) threadedEventTypes

-- # TODO replace prelude: https://github.com/dnikolovv/practical-haskell/blob/main/replace-prelude/README.md
chkFixtureChildren :: [[ExeEvent]] -> IO ()
chkFixtureChildren =
  traverse_ chkEvents
  where
    chkEvents :: [ExeEvent] -> IO ()
    chkEvents evts' =
      foldl' chkEvent Nothing evts'
        & maybe
          (pure ())
          (\fx -> error $ "Fixture started not ended in same thread\n" <> ppShow fx)

    chkEvent :: Maybe Loc -> ExeEvent -> Maybe Loc
    chkEvent mActiveFixLoc evt =
      mActiveFixLoc
        & maybe
          ( -- outside fixture
            evt & \case
              StartExecution {} -> Nothing
              Start {eventType, loc} -> chkOutOfFixtureStartEnd True loc eventType
              End {eventType, loc} -> chkOutOfFixtureStartEnd False loc eventType
              Failure {} -> Nothing
              ParentFailure {} -> Nothing
              ApLog {} -> Nothing
              EndExecution {} -> Nothing
          )
          ( -- within fixture
            \activeFxLoc ->
              evt & \case
                StartExecution {} -> failIn "StartExecution"
                Start {eventType, loc} -> chkInFixtureStartEnd True activeFxLoc loc eventType
                End {eventType, loc} -> chkInFixtureStartEnd False activeFxLoc loc eventType
                Failure {} -> mActiveFixLoc
                ParentFailure {} -> mActiveFixLoc
                ApLog {} -> mActiveFixLoc
                EndExecution {} -> failIn "EndExecution"
          )
      where
        fail msg = error $ msg <> "\n" <> ppShow evt
        failOut et = fail $ show et <> " must occur within start / end of fixture in same thread"

        failIn :: Show a => a -> Maybe Loc
        failIn et = fail $ show et <> " must only occur outside a fixture in same thread"

        chkOutOfFixtureStartEnd :: Bool -> Loc -> ExeEventType -> Maybe Loc
        chkOutOfFixtureStartEnd isStart' evtLoc et =
          -- the err events should not occur unless a fixture is running in the same thread
          et & \case
            L.OnceHook -> Nothing
            OnceHookRelease -> Nothing
            L.ThreadHook -> Nothing
            ThreadHookRelease -> Nothing
            L.Group -> Nothing
            L.TestHook -> err
            TestHookRelease -> err
            L.Fixture ->
              isStart'
                ? Just evtLoc
                $ fail "End fixture when fixture not started"
            L.Test -> err
            L.FixtureOnceHook -> err
            L.FixtureOnceHookRelease -> err
            L.FixtureThreadHook -> err
            L.FixtureThreadHookRelease -> err
          where
            err = failOut et

        chkInFixtureStartEnd :: Bool -> Loc -> Loc -> ExeEventType -> Maybe Loc
        chkInFixtureStartEnd isStart' activeFxLoc evtLoc et =
          -- the err events should not occur when a fixture is running in the same thread
          et & \case
            L.OnceHook -> err
            OnceHookRelease -> err
            L.ThreadHook -> err
            ThreadHookRelease -> err
            L.TestHook -> mActiveFixLoc
            TestHookRelease -> mActiveFixLoc
            L.Group -> err
            L.Fixture ->
              if isStart'
                then fail "Nested fixtures - fixture started when a fixture is alreeady running in the same thread"
                else
                  (activeFxLoc == evtLoc)
                    ? Nothing
                    $ fail "fixture end loc does not match fixture start loc"
            L.Test {} -> mActiveFixLoc
            L.FixtureOnceHook -> mActiveFixLoc
            L.FixtureOnceHookRelease -> mActiveFixLoc
            L.FixtureThreadHook -> mActiveFixLoc
            L.FixtureThreadHookRelease -> mActiveFixLoc
          where
            err = failIn et

countStarts :: ExeEventType -> [ExeEvent] -> Int
countStarts et = count (isStart et)


countLocSets :: [ExeEvent] -> ExeEventType -> Int
countLocSets evs et =
  -- for threaded events ie. not Once* events the number of occurances
  -- is non-determiistic because it may occur on 1..n threads (e.g. the same fixture starts
  -- on 3 threads) but we can count the number of different locs and compare that to
  -- the run template
  ST.size $
    foldl'
      (\s ev -> isStart et ev ? ST.insert (L.loc ev) s $ s)
      ST.empty
      evs

chkLocCount :: Template -> [ExeEvent] -> (Template -> Int) -> ExeEventType -> IO ()
chkLocCount t evs templateCounter et =
  chkEq' (txt et) (templateCount t templateCounter) (countLocSets evs et)

threadedBoundary :: ExeEventType -> Bool
threadedBoundary = \case
  L.OnceHook -> False
  OnceHookRelease -> False
  L.ThreadHook -> True
  ThreadHookRelease -> True
  L.TestHook -> True
  TestHookRelease -> True
  L.Group -> True
  L.Fixture -> True
  L.Test -> True
  L.FixtureOnceHook -> False
  L.FixtureOnceHookRelease -> False
  L.FixtureThreadHook -> True
  L.FixtureThreadHookRelease -> True


chkStartEndIntegrity :: [[ExeEvent]] -> IO ()
chkStartEndIntegrity =
  traverse_ chkThread
  where
    chkThread :: [ExeEvent] -> IO ()
    chkThread evts =
      unless (M.null r)
        . error
        $ "events still open when thread finalised\n" <> ppShow r
      where
        r = foldl' chkEvent M.empty evts

    chkEvent :: M.Map Loc (ST.Set Loc) -> ExeEvent -> M.Map Loc (ST.Set Loc)
    chkEvent acc evt =
      evt & \case
        StartExecution {} -> acc
        Start {eventType} ->
          threadedBoundary eventType
            ? chkStart acc evt
            $ acc
        End {eventType} ->
          threadedBoundary eventType
            ? chkEnd acc evt
            $ acc
        Failure {} -> acc
        ParentFailure {} -> acc
        ApLog {} -> acc
        EndExecution {} -> acc

    chkStart :: M.Map Loc (ST.Set Loc) -> ExeEvent -> M.Map Loc (ST.Set Loc)
    chkStart m e =
      M.member eLoc m
        ? error ("Duplicate start events in same thread\n " <> ppShow e)
        $ M.insert eLoc ST.empty (ST.insert eLoc <$> m)
      where
        eLoc = partialLoc e

    chkEnd :: M.Map Loc (ST.Set Loc) -> ExeEvent -> M.Map Loc (ST.Set Loc)
    chkEnd m e =
      M.lookup eLoc m
        & maybe
          (error $ "Event end where event not started in same thread\n " <> ppShow e)
          ( \s ->
              ST.null s
                & P.bool
                  ( error $
                      "Event ended before child events finished\n Event: \n"
                        <> ppShow e
                        <> "\nOpen child locs\n"
                        <> ppShow s
                  )
                  (M.delete eLoc $ ST.delete eLoc <$> m)
          )
      where
        eLoc = partialLoc e

chkLaws :: Int -> Template -> [ExeEvent] -> IO ()
chkLaws mxThrds t evts =
  do
    traverse_
      (evts &)
      [ chkThreadLogsInOrder,
        chkStartEndExecution,
        chkSingletonLeafEvents,
        chkOnceEventsAreBlocking,
        chkEventCounts t,
        chkErrorPropagation,
        chkOnceHksReleased
      ]
    traverse_
      (threadedEvents &)
      [ chkStartEndIntegrity,
        chkFixtureChildren,
        chkFixturesContainTests t templateAsList,
        traverse_ chkTestEvtsConsecutive,
        chkThreadLeafEvents,
        traverse_ (chkParentOrder t),
        chkThreadErrorPropagation,
        chkThreadHksReleased
      ]
    T.chk'
      ( "max execution threads + 2: "
          <> txt (mxThrds + 1)
          <> " exceeded: "
          <> txt (length threadedEvents)
          <> "\n"
          <> txt (ppShow ((threadId <$>) <$> threadedEvents))
      )
      $ length threadedEvents <= mxThrds + 2
  where
    threadedEvents = groupOn threadId evts
    templateAsList = templateList t

data ChkErrAccum = ChkErrAccum
  { initialised :: Bool,
    lastStart :: Maybe (Loc, ExeEventType),
    lastFailure :: Maybe (Loc, PException, ExeEventType),
    matchedFails :: ST.Set Loc
  }

errorPropagationStep :: ChkErrAccum -> ExeEvent -> ChkErrAccum
errorPropagationStep accum@ChkErrAccum {initialised, lastStart, lastFailure, matchedFails} =
  \case
    StartExecution {} -> accum
    Start {loc, eventType} -> accum {lastStart = Just (loc, eventType)}
    ed@End {loc, eventType} ->
      lastFailure
        & maybe
          accum
          ( \lf@(lfLoc, _e, _et) ->
              ST.member loc matchedFails
                ? accum
                  { matchedFails = ST.delete loc matchedFails,
                    lastFailure =
                      (lfLoc == loc && endIsTerminal eventType)
                        ? Nothing
                        $ lastFailure
                  }
                $ error
                  ( "parent failure was not logged as expected within event ending with:\n"
                      <> ppShow ed
                      <> "\nexpected last failure"
                      <> ppShow lf
                  )
          )
    f@Failure
      { loc,
        msg,
        exception,
        idx,
        threadId
      } ->
        lastFailure
          & maybe
            ( lastStart
                & maybe
                  (error ("failure logged when outside of any event\n" <> ppShow f))
                  ( \(loc', et) ->
                      (==) loc loc'
                        ? accum
                          { lastFailure = Just (loc, exception, et),
                            matchedFails = ST.insert loc matchedFails
                          }
                        $ error
                          ( "failure loc does not equal loc of parent event:\nfailure loc\n"
                              <> ppShow loc'
                              <> "\nparent event:\n"
                              <> ppShow loc
                          )
                  )
            )
            ( \parentFail ->
                error $
                  "failure logged when expected a parent failure (ie. in the child of a partent event that failed):\nfailure:\n"
                    <> ppShow f
                    <> "\nunder parent failure:\n"
                    <> ppShow parentFail
            )
    pf@ParentFailure
      { loc,
        parentLoc,
        parentEventType,
        exception,
        idx,
        threadId
      } ->
        lastFailure
          & maybe
            ( initialised
                ? error ("parent failure logged when parent hasn't failed:\n" <> ppShow pf)
                $ accum
                  { lastFailure = Just (parentLoc, exception, parentEventType),
                    matchedFails = ST.insert loc matchedFails
                  }
            )
            ( \lf@(ploc, ex, _et) ->
                parentLoc
                  == ploc
                  ? accum {matchedFails = ST.insert loc matchedFails}
                  $ error
                    ( "parent failure logged in child does not match parent failure in parent:\nparent failure logged:\n"
                        <> ppShow lf
                        <> "actual parent failure:\n"
                        <> ppShow pf
                    )
            )
    ApLog {} -> accum
    EndExecution {} -> accum

chkThreadErrs :: [ExeEvent] -> IO ()
chkThreadErrs evts =
  -- assumes nesting correct - all nodes have unique locs
  lastFailure (foldl' errorPropagationStep (ChkErrAccum False Nothing Nothing ST.empty) evts)
    & maybe
      (pure ())
      ( \(l, _ex, et) ->
          et
            `elem` [
              -- once errors could occur 
              L.OnceHook, 
              L.FixtureOnceHook
            ]
            ? pure ()
            $ error ("chkThreadErrs error loc still open at end of thread: " <> show l)
      )

chkThreadErrorPropagation :: [[ExeEvent]] -> IO ()
chkThreadErrorPropagation = traverse_ chkThreadErrs

chkHkReleased :: [ExeEvent] -> ExeEventType -> ExeEventType -> IO ()
chkHkReleased evs hkType relType = 
  -- hook releases are always parented by hook
  ST.null openHooks ? 
    pure () $
    error ("Hooks executed without release: " <> ppShow openHooks)
  where
    openHooks = foldl' step ST.empty evs 
    step ev openHks = uu --ev & \case

chkThreadHksReleased = error "not implemented chkThreadHksReleased"
  -- | ThreadHook
  -- | ThreadHookRelease

  -- | FixtureThreadHook
  -- | FixtureThreadHookRelease

  -- | TestHook
  -- | TestHookRelease

chkOnceHksReleased = error "not implemented chkOnceHksReleased"
  -- data ExeEventType
  -- = OnceHook
  -- | OnceHookRelease

  -- | FixtureOnceHook
  -- | FixtureOnceHookRelease





-- data OEAccum = OEAccum
--   { oOpenSHk :: Maybe Loc,
--     oLastSHkFailure :: Maybe (Loc, PException),
--     oOpenChldElms :: ST.Set ThrdLoc
--   }

-- data FixtureSubComponent =
--   FxOnceHook |
--   FxThreadHook |
--   FxTest |
--   NotFxChild deriving (Show, Eq, Ord)
-- data ElemId
--   = ThreadedId
--       { rLoc :: Loc,
--         rThreadId :: SThreadId,
--         rEventType :: ExeEventType,
--         fxChild :: FixtureSubComponent
--       }
--   | OnceId
--       { rLoc :: Loc,
--         rEventType :: ExeEventType,
--         fxChild :: FixtureSubComponent
--       }
--   deriving (Show, Eq, Ord)

-- data ElmResult
--   = Pass
--       { rId :: ElemId
--       }
--   | Fail
--       { rId :: ElemId
--       }
--   | Abondonned
--       { rId :: ElemId,
--         parentId :: ElemId
--       }
--   | Grouping
--       { rId :: ElemId
--       }
--   deriving (Show, Eq, Ord)

data NodeType
  = Singleton
  | Threaded SThreadId
  deriving (Show, Eq, Ord)

data RTLoc = RTLoc
  { loc :: Loc,
    eventType :: ExeEventType,
    nodeType :: NodeType
  }
  deriving (Show, Eq, Ord)

data Fail = Fail {floc :: Loc} | ParentFail {floc :: Loc, ploc :: Loc} deriving (Show, Eq, Ord)

chkErrorPropagation :: [ExeEvent] -> IO ()
chkErrorPropagation evts =
  traverse_
    ( \(p, cs) ->
        fails M.!? p
          & maybe
            (chkChildrenOfPassedParent p cs)
            (chkChildrenOfFailedParent p cs)
    )
    (M.toList parentChildMapGroupsRemoved)
  where
    chkChildrenOfPassedParent :: RTLoc -> [RTLoc] -> IO ()
    chkChildrenOfPassedParent p cs =
      let mErr =
            find
              ( \c ->
                  fails M.!? c
                    & maybe
                      False
                      ( \case
                          Fail {} -> False
                          ParentFail {} -> True
                      )
              )
              cs
       in mErr
            & maybe
              (pure ())
              ( \c ->
                  error $
                    "child loc is parent fail but parent loc is a pass:\n"
                      <> "\nchild:\n"
                      <> ppShow c
                      <> "\nparent:\n"
                      <> ppShow p
              )

    chkChildrenOfFailedParent :: RTLoc -> [RTLoc] -> Fail -> IO ()
    chkChildrenOfFailedParent p cs pf =
      find (not . childFailHasParent) cs
        & maybe
          (pure ())
          ( \c ->
              error $
                "expect child to be parent fail staus with same parent fail loc as parent (ie loc of error source):\n"
                  <> "\nchild loc:\n"
                  <> ppShow c
                  <> "\nchild failure:\n"
                  <> ppShow (fails M.!? c)
                  <> "\nparent loc:\n"
                  <> ppShow p
                  <> "\nparent fail:\n"
                  <> ppShow pf
          )
      where
        pFailLoc =
          pf & \case
            Fail loc -> loc
            ParentFail {ploc} -> ploc
        childFailHasParent c =
          fails M.!? c
            & maybe
              False
              ( \case
                  Fail {} -> False
                  ParentFail {ploc} -> pFailLoc == ploc
              )

    fails :: M.Map RTLoc Fail
    fails =
      foldl'
        ( \acc -> \case
            StartExecution {} -> acc
            Start {} -> acc
            End {} -> acc
            Failure {loc, parentEventType, threadId} -> M.insert (mkRTLoc loc parentEventType threadId) (Fail loc) acc
            ParentFailure {parentLoc = ploc, fEventType, parentEventType, loc = floc, threadId} -> M.insert (mkRTLoc floc fEventType threadId) (ParentFail {floc, ploc}) acc
            ApLog {} -> acc
            EndExecution {} -> acc
        )
        M.empty
        evts

    mkRTLoc :: Loc -> ExeEventType -> SThreadId -> RTLoc
    mkRTLoc l et t =
      RTLoc l et
        $ isOnceEvent et
          ? Singleton
        $ Threaded t

    initMap :: M.Map RTLoc [a]
    initMap =
      foldl'
        ( \accum ->
            \case
              StartExecution {} -> accum
              End {} -> accum
              Failure {} -> accum
              ParentFailure {} -> accum
              ApLog {} -> accum
              EndExecution {} -> accum
              Start {eventType, loc, threadId} -> M.insert (mkRTLoc loc eventType threadId) [] accum
        )
        M.empty
        evts

    lookUpMsg msg m k =
      m M.!? k
        & fromMaybe
          ( error $
              toS msg
                <> ": key not found for\n"
                <> ppShow k
                <> "\nin map\n"
                <> ppShow m
          )

    evntType = lookUpMsg "event type" evntTypeMap

    evntTypeMap :: M.Map Loc ExeEventType
    evntTypeMap =
      foldl'
        ( \accum ->
            \case
              StartExecution {} -> accum
              End {} -> accum
              Failure {} -> accum
              ParentFailure {} -> accum
              ApLog {} -> accum
              EndExecution {} -> accum
              Start {eventType, loc, threadId} ->
                M.alter
                  ( maybe
                      (Just eventType)
                      ( \et ->
                          et
                            == eventType
                            ? Just et
                            $ error
                            $ "same loc encountered with different event type\nloc:\n"
                              <> ppShow loc
                              <> "\nevent types\n"
                              <> ppShow et
                              <> "\n"
                              <> ppShow eventType
                      )
                  )
                  loc
                  accum
        )
        M.empty
        evts

    childParentMap :: M.Map RTLoc RTLoc
    childParentMap =
      M.foldlWithKey
        ( \accum p cs ->
            let childMap = M.fromList $ (,p) <$> cs
                common = M.intersection accum childMap
             in M.null common
                  & bool
                    ( error $
                        "child has more than one parent:\n" <> ppShow common <> "\nin parent map:\n" <> ppShow accum
                    )
                    (M.union accum childMap)
        )
        M.empty
        parentChildMap

    parentChildMapGroupsRemoved :: M.Map RTLoc [RTLoc]
    parentChildMapGroupsRemoved =
      M.foldlWithKey
        ( \acc p cs ->
            let step acc' p'@RTLoc {loc} cs' =
                  let csNoGrps = filter (\RTLoc {eventType} -> notElem eventType [L.Group, L.Fixture]) cs'
                      simple = M.insert p' csNoGrps acc'
                      merged =
                        let p'' = lookUpMsg "parent's parent" childParentMap p'
                            cs'' = lookUpMsg "parent's children" parentChildMap p'' <> cs'
                         in step acc' p'' csNoGrps
                   in isGrouping (evntType loc)
                        ? merged
                        $ simple
             in step acc p cs
        )
        M.empty
        parentChildMap

    parentChildMap :: M.Map RTLoc [RTLoc]
    parentChildMap =
      foldl'
        ( \accum ->
            \case
              StartExecution {} -> accum
              End {} -> accum
              Failure {} -> accum
              ParentFailure {} -> accum
              ApLog {} -> accum
              EndExecution {} -> accum
              ev@Start {eventType, loc, threadId} ->
                loc & \case
                  Root -> accum
                  Node {parent = prnt} ->
                    let pt =
                          RTLoc
                            { loc = prnt,
                              eventType = evntType prnt,
                              nodeType = Threaded threadId
                            }
                        ps = pt {nodeType = Singleton}
                        p =
                          if
                              | M.member pt accum -> pt
                              | M.member ps accum -> ps
                              | otherwise ->
                                  error $
                                    "neither threaded or singleton parent in map\n"
                                      <> ppShow pt
                                      <> "\n\n"
                                      <> ppShow ps
                                      <> "\nnot found in\n"
                                      <> ppShow accum
                        c = mkRTLoc loc eventType threadId
                     in prnt & \case
                          Root -> accum
                          Node {} ->
                            M.member p accum
                              ? M.alter ((c :) <$>) p accum
                              $ error
                                ( "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\nparent not found for child event:\n"
                                    <> ppShow ev
                                    <> "\nlooking for\n"
                                    <> ppShow p
                                    <> "\nin\n"
                                    <> ppShow accum
                                )
        )
        initMap
        evts

chkOnceEventsAreBlocking :: [ExeEvent] -> IO ()
chkOnceEventsAreBlocking evts =
  T.chk' ("event started not closed:\n" <> toS (ppShow openEvnt)) $ isNothing openEvnt
  where
    openEvnt :: Maybe (Loc, ExeEventType)
    openEvnt = foldl' chkStartEndIntegrity' Nothing evts

    chkStartEndIntegrity' :: Maybe (Loc, ExeEventType) -> ExeEvent -> Maybe (Loc, ExeEventType)
    chkStartEndIntegrity' mLcEt evt =
      evt & \case
        StartExecution {} -> mLcEt
        Start {eventType, loc} ->
          mLcEt
            & maybe
              (isOnceEvent eventType ? Just (loc, eventType) $ mLcEt)
              ( \activeEvt ->
                  error $
                    "event started while once event still open\nevent:\n"
                      <> ppShow evt
                      <> "open once event"
                      <> ppShow activeEvt
              )
        End {eventType, loc} ->
          if isOnceEvent eventType
            then
              mLcEt
                & maybe
                  ( error $
                      "once event ended when once event not open\nevent:\n"
                        <> ppShow evt
                  )
                  ( \locEt ->
                      locEt
                        == (loc, eventType)
                        ? Nothing
                        $ error
                        $ "end once event does not correspond to start loc / event type\nstart:\n"
                          <> ppShow locEt
                          <> "\n"
                          <> "end:\n"
                          <> ppShow (loc, eventType)
                  )
            else
              mLcEt
                & maybe
                  mLcEt
                  ( \activeEvt ->
                      error $
                        "event ended while once event still open\nevent:\n"
                          <> ppShow evt
                          <> "open once event"
                          <> ppShow activeEvt
                  )
        Failure {} -> mLcEt
        ParentFailure {} -> mLcEt
        ApLog {} -> mLcEt
        EndExecution {} -> mLcEt

-- T.chk' "" uu
-- after start only oe should end
-- no event parented by oe should occur beefore oe

chkEventCounts :: Template -> [ExeEvent] -> IO ()
chkEventCounts t evs = do
  -- another check checks hook release matches hook so only checking hook starts
  -- once event check number of start events
  chkEq' "Once Hook Count" expectedOnceHkCount $ countStarts L.OnceHook evs
  chkEq' "Fixture Once Hook Count" expectedFixtureCount $ countStarts L.FixtureOnceHook evs
  -- tests number of start events
  chkEq' "Test Count" expectedTestCount $ countStarts L.Test evs
  chkEq' "Test Hooks" expectedTestCount $ countStarts L.TestHook evs

  -- threaded events check nuber of unique locs of type
  -- one event could occur n times due to running in more than
  -- one thread
  -- fixture
  chkLocCount' fixtureCounter L.Fixture

   -- group
  chkLocCount'( \case
        TGroup {} -> 1
        _ -> 0
     )
     L.Group

   -- threadHook
  chkLocCount'( \case
        TThreadHook {} -> 1
        _ -> 0
     )
     L.ThreadHook

   -- FixtureThreadHook
  chkLocCount' fixtureCounter L.FixtureThreadHook

  where
    chkLocCount' = chkLocCount t evs
    templateCount' = templateCount t
    fixtureCounter = \case
        TFixture {} -> 1
        _ -> 0
    expectedTestCount =
      templateCount'
        ( \case
            TFixture {tTests} -> length tTests
            _ -> 0
        )
    expectedFixtureCount = templateCount' fixtureCounter
    expectedOnceHkCount =
      templateCount'
        ( \case
            TOnceHook {} -> 1
            _ -> 0
        )

validateTemplate :: Template -> IO ()
validateTemplate t =
  let tl = (tTag <$> templateList t)
      utl = nub tl
   in when (length tl /= length utl) $
        error $
          "template must be such that tags are unique: \n" <> ppShow tl

runTest :: Int -> Template -> IO ()
runTest maxThreads template = do
  validateTemplate template
  putStrLn ""
  pPrint template
  putStrLn "========="
  chan <- newTChanIO
  q <- newTQueueIO
  ior <- newIORef 0
  tid <- C.myThreadId
  lc@LogControls {sink, log} <- testLogControls chan q
  pn <- mkPrenode maxThreads template
  execute maxThreads lc pn
  log
    & maybe
      (T.chkFail "No Events Log")
      (\evts -> atomically (q2List evts) >>= chkLaws maxThreads template)

ioAction :: TextLogger -> IOProps -> IO ()
ioAction log (IOProps {message, delayms, fail}) =
  do
    log message
    P.threadDelay delayms
    when fail
      . error
      . toS
      $ "exception thrown " <> message

mkTest :: IOProps -> PN.Test si ti ii
mkTest iop@IOProps {message, delayms, fail} = PN.Test message \log a b c -> ioAction log iop

noDelay :: DocFunc Int
noDelay = DocFunc "No Delay" $ pure 0

neverFail :: DocFunc Bool
neverFail = DocFunc "Never Fail" $ pure False

alwaysFail :: DocFunc Bool
alwaysFail = DocFunc "Always Fail" $ pure True

superSimplSuite :: Template
superSimplSuite =
  TThreadHook
    { tTag = "ThreadHook",
      tHook =
        [ IOProps
            { message = "ThreadHook",
              delayms = 1,
              fail = True
            }
        ],
      tRelease =
        [ IOProps
            { message = "ThreadHookRelease",
              delayms = 1,
              fail = False
            }
        ],
      tChild =
        TOnceHook
          { tTag = "OnceHook",
            sHook =
              IOProps
                { message = "Once Hook",
                  delayms = 1,
                  fail = False
                },
            sRelease =
              IOProps
                { message = "Once Hook Release",
                  delayms = 1,
                  fail = False
                },
            tChild =
              TFixture
                { tTag = "Fixture 0",
                  --
                  sHook = testProps "Fixture 0 - Fixture Once Hook" 0 0 False,
                  sRelease = testProps "Fixture 0 - Fixture Once Hook Release" 0 0 False,
                  --
                  tHook = [testProps "Fixture 0 - Fixture Thread Hook" 0 0 False],
                  tRelease = [testProps "Fixture 0 - Fixture Hook Release" 0 0 False],
                  --
                  tTestHook = [testProps "Fixture 0 - Test Hook" 0 0 False],
                  tTestRelease = [testProps "Fixture 0 - Test Hook Release" 0 0 False],
                  tTests = [testProps "" 0 0 False]
                }
          }
    }

-- $> unit_simple_single

unit_simple_single :: IO ()
unit_simple_single = runTest 1 superSimplSuite

-- $ > unit_simple_single_failure

unit_simple_single_failure :: IO ()
unit_simple_single_failure =
  runTest 1 $
    TFixture
      { tTag = "FX 0",
        sHook = testProps "Fx 0 - SH" 0 0 False,
        sRelease = testProps "Fx 0 - SHR" 0 0 False,
        tHook = [testProps "Fx 0" 0 0 False],
        tRelease = [testProps "Fx 0" 0 0 False],
        tTestHook = [testProps "Fx 0" 0 0 False],
        tTestRelease = [testProps "Fx 0" 0 0 False],
        tTests = [testProps "" 0 0 True]
      }

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
    - nxt index
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
