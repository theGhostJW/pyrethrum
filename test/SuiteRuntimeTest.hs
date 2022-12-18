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
import Data.List.Extra (lookup, snoc)
import qualified Data.Map.Strict as M
import qualified Data.Set as ST
import qualified Data.Text as Txt
import Data.Yaml
import GHC.Records
import Internal.PreNode (PreNode (hookChild))
import Internal.PreNode as PN
import Internal.RunTimeLogging as L
  ( ExeEvent (..),
    ExeEventType (..),
    Loc (..),
    LogControls (..),
    PException,
    SThreadId (..),
    endIsTerminal,
    mkLogger,
    testLogControls,
  )
import Internal.SuiteRuntime
import qualified Internal.SuiteRuntime as S
import Polysemy
import Pyrelude (ListLike (..))
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

foldTemplate :: forall a. a -> (Template -> Template -> a -> a) -> Template -> a
foldTemplate seed combine =
  pm seed
  where
    pm :: a -> Template -> a
    pm acc t =
      let recurse :: Template -> a
          recurse child = combine t child acc
       in t & \case
            TGroup {tChilds} -> foldl' pm acc tChilds
            TOnceHook {tChild} -> recurse tChild
            TThreadHook {tChild} -> recurse tChild
            TFixture {} -> acc

parentMap :: Template -> M.Map Text (Maybe Template)
parentMap t =
  foldTemplate
    (M.singleton (tTag t) Nothing)
    (\p c m -> M.insert (tTag c) (Just p) m)
    t

templateList :: Template -> [Template]
templateList t = foldTemplate [t] (\_p c l -> c : l) t

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
            case et of
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

templateTestCount :: [Template] -> Int
templateTestCount ts =
  length . catMaybes $ tests <$> ts
  where
    tests = \case
      TFixture {tTests} -> Just tTests
      _ -> Nothing

templateFixCount :: Template -> Int
templateFixCount =
  fxCount' 0
  where
    fxCount' :: Int -> Template -> Int
    fxCount' ac = \case
      TGroup {tChilds} -> ac + foldl' fxCount' 0 tChilds
      TOnceHook {tChild} -> fxCount' ac tChild
      TThreadHook {tChild} -> fxCount' ac tChild
      TFixture {tTests} -> ac + 1

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
  unpack . foldl' fld M.empty . reverse
  where
    unpack = (snd <$>) . M.toList
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
      "Equality check failed\n"
        <> "Expected:\n  "
        <> ppShow e
        <> "\nDoes not Equal:\n  "
        <> ppShow a
        <> "\n"
        <> toS msg

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

chkTesmevtsConsecutive :: [ExeEvent] -> IO ()
chkTesmevtsConsecutive evs =
  let tsmevts = read . toS . Txt.takeWhileEnd (/= '.') . startTag <$> filter (isStart L.Test) evs
      chkidxless :: Int -> Int -> IO Int
      chkidxless i1 i2 = i1 >= i2 ? error "test index out of order" $ pure i2
   in null tsmevts
        ? pure ()
        $ foldM_ chkidxless (-1) tsmevts

chkLeafEvents :: ExeEventType -> [[ExeEvent]] -> IO ()
chkLeafEvents targetEventType =
  traverse_ chkEvents
  where
    targStr = show targetEventType
    matchesTarg = (targetEventType ==)

    chkEvents :: [ExeEvent] -> IO ()
    chkEvents evts' = do
      foldl' chkEvent Nothing evts'
        & maybe
          (pure ())
          (\fx -> error $ targStr <> " started not ended in same thread\n" <> ppShow fx)
      chkTesmevtsConsecutive evts'

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
        fail msg = error $ msg <> "\n" <> ppShow evt
        strTrg = show targetEventType
        failIn s = fail $ s <> " must only occur outside a " <> strTrg <> " in same thread"

        chkOutEventStartEnd :: Bool -> Loc -> ExeEventType -> Maybe Loc
        chkOutEventStartEnd isStart' evtLoc =
          P.bool
            Nothing
            ( isStart'
                ? Just evtLoc
                $ fail
                $ "End " <> strTrg <> " when not started"
            )
            . matchesTarg

        chkInEventStartEnd :: Bool -> Loc -> Loc -> ExeEventType -> Maybe Loc
        chkInEventStartEnd isStart' fxLoc evtLoc evt' =
          let sevt = show evt'
           in matchesTarg evt'
                ? ( isStart'
                      ? fail ("Nested " <> sevt <> " - " <> sevt <> " started when a " <> sevt <> " is alreeady running in the same thread")
                      $ fxLoc
                        == evtLoc
                        ? Nothing
                      $ fail (strTrg <> " end loc does not match " <> strTrg <> " start loc")
                  )
                $ failIn sevt

chkSingletonLeafEvents :: [ExeEvent] -> IO ()
chkSingletonLeafEvents evts =
  traverse_
    (`chkLeafEvents` [evts])
    [ L.OnceHook,
      L.OnceHookRelease
    ]

chkThreadLeafEvents :: [[ExeEvent]] -> IO ()
chkThreadLeafEvents evts =
  traverse_
    (`chkLeafEvents` evts)
    [ L.ThreadHook,
      L.ThreadHookRelease,
      L.TestHook,
      L.TestHookRelease,
      L.Test
    ]

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
    chkEvent mfixLoc evt =
      mfixLoc
        & maybe
          ( -- outside fixture
            case evt of
              StartExecution {} -> Nothing
              Start {eventType, loc} -> chkOutOfFixtureStartEnd True loc eventType
              End {eventType, loc} -> chkOutOfFixtureStartEnd False loc eventType
              Failure {} -> Nothing
              ParentFailure {} -> Nothing
              ApLog {} -> Nothing
              EndExecution {} -> Nothing
          )
          ( -- within fixture
            \fxLoc ->
              case evt of
                StartExecution {} -> failIn "StartExecution"
                Start {eventType, loc} -> chkInFixtureStartEnd True fxLoc loc eventType
                End {eventType, loc} -> chkInFixtureStartEnd False fxLoc loc eventType
                Failure {} -> mfixLoc
                ParentFailure {} -> mfixLoc
                ApLog {} -> mfixLoc
                EndExecution {} -> failIn "EndExecution"
          )
      where
        fail msg = error $ msg <> "\n" <> ppShow evt
        failOut et = fail $ et <> " must occur within start / end of fixture in same thread"
        failIn et = fail $ et <> " must only occur outside a fixture in same thread"

        chkOutOfFixtureStartEnd :: Bool -> Loc -> ExeEventType -> Maybe Loc
        chkOutOfFixtureStartEnd isStart' evtLoc = \case
          L.OnceHook -> Nothing
          OnceHookRelease -> Nothing
          L.ThreadHook -> Nothing
          ThreadHookRelease -> Nothing
          L.Group -> Nothing
          L.TestHook -> failOut "TestHook"
          TestHookRelease -> failOut "TestHookTestHookRelease"
          L.Fixture ->
            isStart'
              ? Just evtLoc
              $ fail "End fixture when fixture not started"
          L.Test -> failOut "Test"
          L.FixtureOnceHook -> uu
          L.FixtureOnceHookRelease -> uu
          L.FixtureThreadHook -> uu
          L.FixtureThreadHookRelease -> uu

        chkInFixtureStartEnd :: Bool -> Loc -> Loc -> ExeEventType -> Maybe Loc
        chkInFixtureStartEnd isStart' fxLoc evtLoc = \case
          L.OnceHook -> failIn "OnceHook"
          OnceHookRelease -> failIn "OnceHookRelease"
          L.ThreadHook -> failIn "ThreadHook"
          ThreadHookRelease -> failIn "ThreadHookRelease"
          L.TestHook -> mfixLoc
          TestHookRelease -> mfixLoc
          L.Group -> failIn "Group"
          L.Fixture ->
            isStart'
              ? fail "Nested fixtures - fixture started when a fixture is alreeady running in the same thread"
              $ (fxLoc == evtLoc) ? Nothing
              $ fail "fixture end loc does not match fixture start loc"
          L.Test {} -> mfixLoc
          L.FixtureOnceHook -> uu
          L.FixtureOnceHookRelease -> uu
          L.FixtureThreadHook -> uu
          L.FixtureThreadHookRelease -> uu

chkTestCount :: [Template] -> [ExeEvent] -> IO ()
chkTestCount t evs =
  chkEq'
    "test count not as expected"
    (templateTestCount t)
    $ count (isStart L.Test) evs

chkFxtrCount :: [Template] -> [ExeEvent] -> IO ()
chkFxtrCount t =
  chkEq'
    "test count not as expected"
    (templateTestCount t)
    . length
    -- in concurrent runs the same fixture can be
    -- triggered in multiple threads so
    . groupOn partialLoc
    . filter (isStart L.Fixture)

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
  L.FixtureOnceHook -> uu
  L.FixtureOnceHookRelease -> uu
  L.FixtureThreadHook -> uu
  L.FixtureThreadHookRelease -> uu

-- TODO: Error -> txt
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
    chkEvent acc evt = case evt of
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
        $ M.insert eLoc ST.empty
        $ ST.insert eLoc <$> m
      where
        eLoc = partialLoc e

    -- TODO - change uu to error
    -- TODO chkFalse'
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
    -- TODO check there is a chk for fix contains all tests
    -- TODO OnceFixtureHook, ThreadFixtureHook -- don't use
    traverse_
      (evts &)
      [ -- chkThreadLogsInOrder,
        -- chkStartEndExecution,
        -- chkTestCount templateAsList,
        -- chkFxtrCount templateAsList,
        -- chkSingletonLeafEvents,
        -- chkOnceEventCount,
        -- chkOnceEventParentOrder,
        chkErrorPropagation
      ]
    traverse_
      (threadedEvents &)
      [ chkStartEndIntegrity,
        chkFixtureChildren,
        chkFixturesContainTests t templateAsList,
        traverse_ chkTesmevtsConsecutive,
        chkThreadLeafEvents,
        traverse_ (chkParentOrder t),
        chkThreadErrorPropagation
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

errorStep :: ChkErrAccum -> ExeEvent -> ChkErrAccum
errorStep accum@ChkErrAccum {initialised, lastStart, lastFailure, matchedFails} =
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
                (==) parentLoc ploc
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
  lastFailure (foldl' errorStep (ChkErrAccum False Nothing Nothing ST.empty) evts)
    & maybe
      (pure ())
      ( \(l, _ex, et) ->
          et
            `elem` [L.OnceHook, L.FixtureOnceHook]
            ? pure ()
            $ error ("chkThreadErrs error loc still open at end of thread: " <> show l)
      )

chkThreadErrorPropagation :: [[ExeEvent]] -> IO ()
chkThreadErrorPropagation = traverse_ chkThreadErrs

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
    nodeType :: NodeType
  }
  deriving (Show, Eq, Ord)

data RTLocEvt = RTLocEvt
  { rtLoc :: RTLoc,
    eventType :: ExeEventType
  }
  deriving (Show, Eq, Ord)

chkErrorPropagation :: [ExeEvent] -> IO ()
chkErrorPropagation evts =
  M.null parentChildMap ? error "NULL" $ error "NOT NULL!!"
  where
    -- do
    --   when (M.null pmap) $
    --     putStrLn "Template Map Empty"
    --   for_
    --     (M.toList childList)
    --     ( \(p, c) ->
    --         if
    --             | isFail p ->
    --                 T.chk'
    --                   ("parent has failed but not all child nodes are parent failures\nparent:\n " <> txt p <> "\nchild nodes:\n " <> txt c)
    --                   $ all isParentFail c
    --             | isParentFail p ->
    --                 T.chk'
    --                   ("parent is parent failure but not all child nodes are parent failures\nparent:\n " <> txt p <> "\nchild nodes:\n " <> txt c)
    --                   $ all isParentFail c
    --             | otherwise ->
    --                 T.chk'
    --                   ("parent is not a parent failure or failure but child node(s) exist that are parent failures\nparent:\n " <> txt p <> "\nchild nodes:\n " <> txt c)
    --                   . not
    --                   $ any isParentFail c
    --     )
    isSingleton :: ExeEventType -> Bool
    isSingleton = \case
      L.OnceHook -> True
      L.OnceHookRelease -> True
      L.ThreadHook -> False
      L.ThreadHookRelease -> False
      L.FixtureOnceHook -> True
      L.FixtureOnceHookRelease -> True
      L.FixtureThreadHook -> False
      L.FixtureThreadHookRelease -> False
      L.TestHook -> False
      L.TestHookRelease -> False
      L.Group -> False
      L.Fixture -> False
      L.Test -> False

    mkRTLoc :: Loc -> ExeEventType -> SThreadId -> RTLoc
    mkRTLoc l et t =
      RTLoc l
        $ isSingleton et
          ? Singleton
        $ Threaded t

    initMap :: M.Map RTLoc [RTLocEvt]
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

    childParentMap :: M.Map RTLoc RTLoc
    childParentMap =
      foldl'
        ( \accum (p, cs) ->
            let childMap = M.fromList $ (\c -> (rtLoc c, p)) <$> cs
                common = M.intersection accum childMap
             in M.null common
                  & bool
                    ( error $
                        "child has more than one parent:\n" <> ppShow common <> "\nin parent map:\n" <> ppShow accum
                    )
                    (M.union accum common)
        )
        M.empty
        $ M.toList parentChildMap

    parentChildMap :: M.Map RTLoc [RTLocEvt]
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
                              nodeType = Threaded threadId
                            }
                        ps = pt {nodeType = Singleton}
                        p = M.member pt accum ? pt $ ps
                        c = RTLocEvt (mkRTLoc loc eventType threadId) eventType
                     in prnt & \case
                          Root -> accum
                          Node {} ->
                            accum M.!? p
                              & maybe
                                ( error $
                                    "parent not found for\n"
                                      <> ppShow ev
                                      <> "\nin\n"
                                      <> ppShow p
                                )
                                (\clst -> M.alter (fmap (c :)) p accum)
        )
        initMap
        evts
        & debug' "########~ PARENT MAP ~#######"

chkOnceEventsAreBlocking :: [ExeEvent] -> IO ()
chkOnceEventsAreBlocking _ = putStrLn "!!!!!!!!!!!!!! TODO !!!!!!!!!"

chkOnceEventParentOrder :: [ExeEvent] -> IO ()
chkOnceEventParentOrder _ = putStrLn "!!!!!!!!!!!!!! TODO !!!!!!!!!"

chkOnceEventCount :: [ExeEvent] -> IO ()
chkOnceEventCount _ = putStrLn "!!!!!!!!!!!!!! TODO !!!!!!!!!"

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

-- superSimplSuite :: TQueue RunEvent -> IO PreNodeRoot
-- superSimplSuite q =
--   root $ mkFixture q 1

-- data BoundaryType
--   = Start
--   | End
--   deriving (Show, Eq, Ord)

-- data BranchType
--   = Hook
--   | Fixture
--   deriving (Show, Eq, Ord)

-- data NodeStats
--   = HookStats
--       { id :: Text,
--         parent :: Text,
--         fixtureCount :: Int,
--         hookCount :: Int
--       }
--   | FixtureStats
--       { id :: Text,
--         parent :: Text,
--         iterationCount :: Int
--       }
--   deriving (Show)

-- isHookStats :: NodeStats -> Bool
-- isHookStats = \case
--   HookStats {} -> True
--   FixtureStats {} -> False

-- isFixtureStats :: NodeStats -> Bool
-- isFixtureStats = \case
--   HookStats {} -> False
--   FixtureStats {} -> True

-- countSubNodes :: (forall a b c d e f. PreNode a b c d e f -> Bool) -> PreNode g h i j k l -> Int
-- countSubNodes pred node =
--   let countSubNodes' :: forall h i j k l m. Int -> PreNode h i j k l m -> Int
--       countSubNodes' accum = \case
--         b@Branch {subElms} ->
--           foldl' countSubNodes' (pred b ? accum + 1 $ accum) subElms
--         ah@OnceHook {} -> pred ah ? accum + 1 $ accum
--         ThreadHook {} -> uu
--         TestHook {} -> uu
--         fx@PN.Fixture {} -> pred fx ? accum + 1 $ accum
--    in countSubNodes' 0 node

-- getStats :: PreNodeRoot -> IO [NodeStats]
-- getStats PreNodeRoot {rootNode} =
--   getStats' "Root" 0 <$> rootNode
--   where
--     nonEmptyFixture :: PreNode a b c d e f -> Bool
--     nonEmptyFixture = \case
--       PN.OnceHook {} -> False
--       PN.Branch {} -> False
--       ThreadHook {} -> False
--       TestHook {} -> False
--       f@PN.Fixture {} -> not $ nodeEmpty f

--     nonEmptyHook :: PreNode a b c d e f -> Bool
--     nonEmptyHook = \case
--       h@PN.OnceHook {} -> not $ nodeEmpty h
--       PN.Branch {} -> False
--       ThreadHook {} -> False
--       TestHook {} -> False
--       PN.Fixture {} -> False

--     getStats' :: Text -> Int -> PreNode a b c d e f -> [NodeStats]
--     getStats' parentId subIndex =
--       \case
--         PN.Branch {subElms} ->
--           let thisId = parentId <> ".Branch " <> txt subIndex
--            in (zip [0 ..] subElms >>= uncurry (getStats' thisId))
--         PN.OnceHook {hookChild} ->
--           let thisId = parentId <> ".Hook " <> txt subIndex
--               thisNode =
--                 HookStats
--                   { id = thisId,
--                     parent = parentId,
--                     fixtureCount = countSubNodes nonEmptyFixture hookChild,
--                     hookCount = countSubNodes nonEmptyHook hookChild
--                   }
--            in thisNode : getStats' thisId 0 hookChild
--         ThreadHook {} -> uu
--         TestHook {} -> uu
--         PN.Fixture {iterations} ->
--           [ FixtureStats
--               { id = parentId <> ".Fixture " <> txt subIndex,
--                 parent = parentId,
--                 iterationCount = length iterations
--               }
--           ]

-- -- data BoundaryInfo = BoundaryInfo
-- --   { id :: Loc,
-- --     childCount :: Int
-- --   }

-- -- data RunEvent
-- --   = Boundary
-- --       { branchType :: BranchType,
-- --         boundaryType :: BoundaryType,
-- --         threadId :: ThreadId,
-- --         id :: Loc
-- --       }
-- --   | IterationMessage
-- --       {
-- --         index :: Int,
-- --         message :: Maybe Text,
-- --         threadId :: ThreadId
-- --       }
-- --   | Message Text ThreadId
-- --   deriving (Show, Eq, Ord)

-- logEvent :: TQueue RunEvent -> (ThreadId -> RunEvent) -> IO ()
-- logEvent q ev = do
--   i <- myThreadId
--   atomically . writeTQueue q $ ev i

-- logBoundary :: TQueue RunEvent -> BranchType -> BoundaryType -> Loc -> IO ()
-- logBoundary q brt bdt loc =
--   logEvent q $ \thrd ->
--     Boundary
--       { branchType = brt,
--         boundaryType = bdt,
--         threadId = thrd,
--         id = loc
--       }

-- hook :: TQueue RunEvent -> BoundaryType -> Loc -> o -> IO o
-- hook q bdt loc hko = logBoundary q SuiteRuntimeTest.Hook bdt loc >> pure hko

-- hookStart :: TQueue RunEvent -> Loc -> o -> IO o
-- hookStart q = SuiteRuntimeTest.hook q Start

-- hookEnd :: TQueue RunEvent -> Loc -> IO ()
-- hookEnd q loc = SuiteRuntimeTest.hook q End loc ()

-- fixture :: TQueue RunEvent -> BoundaryType -> Loc -> IO ()
-- fixture q = logBoundary q SuiteRuntimeTest.Fixture

-- fixtureStart :: TQueue RunEvent -> Loc -> IO ()
-- fixtureStart q = SuiteRuntimeTest.fixture q Start

-- fixtureEnd :: TQueue RunEvent -> Loc -> IO ()
-- fixtureEnd q = SuiteRuntimeTest.fixture q End

-- logIteration :: TQueue RunEvent -> Maybe Text -> Int -> IO ()
-- logIteration q itMsg iidx =
--   logEvent q (IterationMessage iidx itMsg)

-- logMessage :: TQueue RunEvent -> Text -> IO ()
-- logMessage q txt' = logEvent q (Message txt')

-- -- remove when pyrelude updated
-- chkEq' t = assertEqual (toS t)

-- mkBranch :: TQueue RunEvent -> [IO (PreNode si so ti to ii io)] -> IO (PreNode si () ti ()  ii ())
-- mkBranch q subElms = PN.Branch Nothing <$> sequenceA subElms

-- mkFixture :: TQueue RunEvent -> Int -> IO (PreNode i () ti () ii ())
-- mkFixture q itCount = do
--   pure $
--     PN.Fixture
--       {
--         fxTag = Nothing,
--         logStart = fixtureStart q,
--         iterations = M.fromList $ zip (txt <$> [1..]) $ mkIterations q itCount,
--         logEnd = fixtureEnd q
--       }

-- -- mkHook :: TQueue RunEvent -> so -> IO (PreNode so so2 ti to) -> IO (PreNode si so ti to)
-- mkHook q hko nodeChild =
--   do
--     status <- atomically $ newTVar S.Pending
--     rslt <- (newEmptyTMVarIO :: IO (TMVar (Either SomeException o)))
--     nc <- nodeChild
--     pure
--       PN.OnceHook
--         {
--           hookTag = Nothing,
--           hookResult = rslt,
--           hook = \loc _ -> hookStart q loc hko,
--           hookChild = nc,
--           hookRelease = \loc _ -> hookEnd q loc
--         }

-- iterationMessage :: Int -> Text
-- iterationMessage i = "iteration " <> txt i

-- mkIterations :: TQueue RunEvent -> Int -> [oi -> ti -> ii -> IO ()]
-- mkIterations q size' =
--   let mkIt :: Int -> oi' -> ti' -> ii' -> IO ()
--       mkIt idx oi ti ii = logIteration q Nothing idx
--    in mkIt <$> [0 .. size' - 1]

-- root :: IO (PreNode () () () () () ()) -> IO PreNodeRoot
-- root = pure . PreNodeRoot

-- simpleSuiteWithHook :: TQueue RunEvent -> IO PreNodeRoot
-- simpleSuiteWithHook q = do
--   root
--     . mkHook q ()
--     $ mkFixture q 1

-- -- simpleBranchedSuiteWithHook :: TQueue RunEvent -> IO PreNodeRoot
-- -- simpleBranchedSuiteWithHook q = do
-- --   fx <- mkFixture q "Root.Hook 0" "Fixture 0" 1
-- --   root $ (: []) <$> mkHook q "Root" "Hook 0" fx

-- tQToList :: TQueue a -> IO [a]
-- tQToList q =
--   reverse <$> recurse []
--   where
--     recurse l =
--       atomically (tryReadTQueue q)
--         >>= maybe (pure l) (\a -> recurse (a : l))

-- boundaryId :: BranchType -> BoundaryType -> RunEvent -> Maybe Text
-- boundaryId brt bnt = \case
--   Boundary {branchType, boundaryType, id} -> branchType == brt && boundaryType == bnt ? (Just . unLoc $ id) $ Nothing
--   IterationMessage {} -> Nothing
--   Message {} -> Nothing

-- boundaryId' :: RunEvent -> Maybe Text
-- boundaryId' = \case
--   Boundary {id} -> Just . unLoc $ id
--   IterationMessage {} -> Nothing
--   Message {} -> Nothing

-- isBoundary :: RunEvent -> Bool
-- isBoundary = \case
--   Boundary {} -> True
--   IterationMessage {} -> False
--   Message {} -> False

-- isHook :: RunEvent -> Bool
-- isHook re = isBoundary re && branchType re == SuiteRuntimeTest.Hook

-- mboundaryType :: RunEvent -> Maybe BoundaryType
-- mboundaryType = \case
--   Boundary {boundaryType = bt} -> Just bt
--   IterationMessage {} -> Nothing
--   Message {} -> Nothing

-- chkHooks :: [NodeStats] -> [RunEvent] -> IO ()
-- chkHooks stats evntLst =
--   let hks = filter isHookStats stats
--       expectedHkIds = (id :: NodeStats -> Text) <$> hks
--       bndrys = filter isBoundary evntLst
--       actualHooks = filter isHook bndrys
--       actualHkIds = nub . catMaybes $ boundaryId' <$> actualHooks

--       chkStartAndEnd :: Text -> [RunEvent] -> IO ()
--       chkStartAndEnd msg = \case
--         [] -> chkFail $ msg <> " - expected start and end boundary but no boundarry found"
--         [bnd] -> chkFail $ msg <> " - expected start and end boundary but only one boundarry found " <> txt bnd
--         [start, end] -> do
--           -- hook should log a start before it ends
--           chkEq' (msg <> " " <> txt start) (Just Start) (mboundaryType start)
--           chkEq' (msg <> " " <> txt end) (Just End) (mboundaryType end)
--         a -> chkFail $ msg <> " too many boundary events" <> txt a

--       chkHkExists :: Text -> IO ()
--       chkHkExists id' =
--         let hkbds =
--               filter
--                 ( \n ->
--                     boundaryId SuiteRuntimeTest.Hook Start n == Just id'
--                       || boundaryId SuiteRuntimeTest.Hook End n == Just id'
--                 )
--                 bndrys
--          in chkStartAndEnd ("hook: " <> id') hkbds

--       chkUnexpectedHooks :: IO ()
--       chkUnexpectedHooks = chkEq' "Unexpected or empty hooks in actual" [] (actualHkIds \\ expectedHkIds)

--       chkSubElements :: Text -> IO ()
--       chkSubElements nId =
--         let hooksStats = fromJust $ find (\s -> nId == (id :: NodeStats -> Text) s) hks
--             actualSubElms =
--               groupBy (\e1 e2 -> boundaryId' e1 == boundaryId' e2)
--                 . filter (\re -> ((\id' -> isPrefixOf nId id' && nId /= id') <$> boundaryId' re) == Just True)
--                 . dropWhileEnd (\re -> boundaryId' re /= Just nId)
--                 . dropWhile (\re -> boundaryId' re /= Just nId)
--                 $ bndrys
--             actualSubHooks = filter (\g -> Just True == ((SuiteRuntimeTest.Hook ==) . branchType <$> head g)) actualSubElms
--             actualSubFixtures = filter (\g -> Just True == ((SuiteRuntimeTest.Fixture ==) . branchType <$> head g)) actualSubElms
--          in do
--               chkEq' "expected hook count" (hookCount hooksStats) (length actualSubHooks)
--               chkEq' "expected fixture count" (fixtureCount hooksStats) (length actualSubFixtures)
--               traverse_ (chkStartAndEnd "sub-hooks") actualSubHooks
--               traverse_ (chkStartAndEnd "sub-fixture") actualSubFixtures
--    in do
--         traverse_ chkHkExists expectedHkIds
--         chkUnexpectedHooks
--         traverse_ chkSubElements expectedHkIds

-- CHildren :: [NodeStats] -> [RunEvent] -> IO ()
-- CHildren stats evntLst =
--   let fixIds bt = catMaybes $ boundaryId SuiteRuntimeTest.Fixture bt <$> evntLst
--       fixStarts = fixIds Start
--       fixEnds = fixIds End

--       fixStatCount = count isFixtureStats stats

--       chkFixture :: NodeStats -> IO ()
--       chkFixture = \case
--         HookStats {} -> pure ()
--         FixtureStats {id, parent, iterationCount} ->
--           let matchesFix :: RunEvent -> Bool
--               matchesFix = \case
--                 Boundary {branchType, id = id'} -> branchType == SuiteRuntimeTest.Fixture && id' == Loc id
--                 -- IterationMessage {parentFix} -> parentFix == Loc id
--                 IterationMessage {} -> uu
--                 Message {} -> False

--               evntsToChk :: [RunEvent]
--               evntsToChk = P.filter matchesFix evntLst

--               emptyFix = iterationCount == 0

--               iternIdx :: RunEvent -> Maybe Int
--               iternIdx = \case
--                 Boundary {} -> Nothing
--                 IterationMessage {index} -> Just index
--                 Message {} -> Nothing

--               itrLst :: [Int]
--               itrLst = catMaybes $ iternIdx <$> evntsToChk

--               itrIdxs :: ST.Set Int
--               itrIdxs = ST.fromList itrLst

--               expectedIterationIdxs :: ST.Set Int
--               expectedIterationIdxs = emptyFix ? ST.empty $ ST.fromList [0 .. iterationCount - 1]

--               isIteration :: RunEvent -> Bool
--               isIteration = \case
--                 Boundary {} -> False
--                 IterationMessage {} -> True
--                 Message {} -> False

--               firstEv = head evntsToChk >>= mboundaryType
--               lastEv = last evntsToChk >>= mboundaryType
--            in emptyFix
--                 ? chk' ("Fixture: " <> id <> " has no iterations - no related run events should be logged") (null evntsToChk)
--                 $ do
--                   null evntsToChk
--                     ?
--                     -- run events for fixture must exist
--                     chkFail ("Fixture: " <> id <> " has " <> txt iterationCount <> " iterations but no run events ")
--                     $ do
--                       -- the first event must be
--                       chkEq' "first event should be fixture start" (Just Start) firstEv
--                       chkEq' "last event should be fixture end" (Just End) lastEv
--                       -- iteration counts should match
--                       chkEq' "iteration count" iterationCount $ length itrLst
--                       chkEq' "iteration indexes" expectedIterationIdxs itrIdxs
--    in do
--         chkEq' "expected fixture start count" fixStatCount $ length fixStarts
--         chkEq' "expected fixture end count" fixStatCount $ length fixEnds
--         for_ stats chkFixture

-- exeSuiteTests :: (TQueue RunEvent -> IO PreNodeRoot) -> Int -> IO ()
-- exeSuiteTests preSuite maxThreads = do
--   q <- atomically newTQueue
--   preSuite' <- preSuite q
--   stats <- getStats preSuite'
--   S.execute maxThreads uu preSuite'
--   l <- tQToList q
--   putStrLn ""
--   putStrLn "============ Stats ============"
--   pPrint stats
--   putStrLn ""
--   putStrLn "============ Logs ============"
--   pPrint l
--   CHildren stats l
--   chkHooks stats l

-- -- $> unit_simple_single

-- unit_simple_single :: IO ()
-- unit_simple_single = do
--   exeSuiteTests superSimplSuite 1

-- -- $> unit_simple_with_hook

-- unit_simple_with_hook :: IO ()
-- unit_simple_with_hook =
--   replicateM_ 1 $
--     exeSuiteTests simpleSuiteWithHook 1

-- -- ~ simple branch
-- simpleSuiteWithBranch :: TQueue RunEvent -> IO PreNodeRoot
-- simpleSuiteWithBranch q =
--   let subElms =
--         [ mkFixture q 2 :: IO (PreNode () () () () () ()),
--           mkFixture q 2 :: IO (PreNode () () () () () ())
--         ]
--    in root $ mkBranch q subElms

-- simpleSuiteBranchInHook :: TQueue RunEvent -> IO PreNodeRoot
-- simpleSuiteBranchInHook q =
--   root $
--     mkBranch
--       q
--       [ mkHook q 7 $
--           mkBranch
--             q
--             [ mkFixture q 2,
--               mkFixture q 2
--             ]
--       ]

-- deeplyNested :: TQueue RunEvent -> IO PreNodeRoot
-- deeplyNested q =
--   let
--       hk :: o -> IO (PreNode o o2 to to2 io io2) -> IO (PreNode i o to to2 io io2)
--       hk = mkHook q

--       branch :: [IO (PreNode si so ti to ii io)] -> IO (PreNode si () ti () ii ())
--       branch = mkBranch q

--       fx :: Int -> IO (PreNode i () ti () ii ())
--       fx = mkFixture q
--    in root $
--         branch
--           [ hk "hkOut" $
--               hk 88 $
--                 branch
--                   [ fx 2,
--                     fx 2,
--                     branch
--                       [ hk "Out Str" $
--                           fx 2
--                       ],
--                     branch
--                       [ fx 2
--                       ]
--                   ]
--           ]

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
