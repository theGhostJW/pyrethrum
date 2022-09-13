module SuiteRuntimeTest where

import Check (Checks)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import DSL.Interpreter
import Data.Aeson.Encoding (quarter)
import Data.Aeson.TH
import Data.Aeson.Types
-- TODO Add to Pyrelude
-- TODO Add to Pyrelude
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
    ExeEventType (Fixture, Group, OnceHook, OnceHookRelease, Test, TestHook, TestHookRelease, ThreadHook, ThreadHookRelease),
    Loc (..),
    LogControls (..),
    Sink,
    getTag,
    mkLogger,
    testLogControls,
  )
import Internal.SuiteRuntime
import qualified Internal.SuiteRuntime as S
import Polysemy
import Pyrelude as P
  ( Applicative ((<*>)),
    Bool (..),
    Either,
    Enum (succ),
    Eq (..),
    Foldable (foldl, sum),
    IO,
    Int,
    ListLike (all, drop, foldl', foldl1, head, null, takeWhileEnd, unsafeHead, unsafeLast),
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
    error,
    filter,
    find,
    first,
    flip,
    fmap,
    foldM_,
    foldl1',
    for_,
    fromJust,
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
    pure,
    replace,
    replicateM_,
    reverse,
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
    (<>),
    (>>),
    (>>=),
    (?),
    (\\),
    (||), Category (id),
  )
import Pyrelude.Test as T hiding (chkEq, chkEq', filter, maybe, singleton)
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
testProps fxTag idx = IOProps (fxTag <> "." <> txt idx)

data IOPropsNonDet = IOPropsNonDet
  { message' :: Text,
    delayms' :: DocFunc Int,
    fail' :: DocFunc Bool
  }
  deriving (Show)

data Template
  = TGroup
      { ttag :: Text,
        tChilds :: [Template]
      }
  | TOnceHook
      { ttag :: Text,
        shook :: IOProps,
        srelease :: IOProps,
        tChild :: Template
      }
  | TThreadHook
      { ttag :: Text,
        thook :: IOPropsNonDet,
        trelease :: IOPropsNonDet,
        tChild :: Template
      }
  | TTestHook
      { ttag :: Text,
        thook :: IOPropsNonDet,
        trelease :: IOPropsNonDet,
        tChild :: Template
      }
  | TFixture
      { ttag :: Text,
        tTests :: [IOProps]
      }
  deriving (Show)

type TextLogger = Text -> IO ()

foldTemplate :: forall a. a -> (Template -> Template -> a -> a) -> Template -> a
foldTemplate seed combine =
  pm seed . P.debug' "****** Template"
  where
    pm :: a -> Template -> a
    pm acc t =
      let recurse :: Template -> a
          recurse child = combine t child acc
       in case t of
            TGroup {tChilds} -> foldl' pm acc tChilds
            TOnceHook {tChild} -> recurse tChild
            TThreadHook {tChild} -> recurse tChild
            TTestHook {tChild} -> recurse tChild
            TFixture {} -> acc

parentMap :: Template -> M.Map Text (Maybe Template)
parentMap t = 
  here add tests to parent map
  P.debug' "****** parentMap" $ foldTemplate (M.singleton (ttag t) Nothing) (\p c m -> M.insert (ttag c) (Just p) m) t

templateList :: Template -> [Template]
templateList t = foldTemplate [t] (\_p c l -> c : l) t

-- map fo child loc => parent locs (only: TTestHook, TThreadHook, TGroup)
threadParentMap :: Template -> M.Map Text [Text]
threadParentMap root =
  (ttag <$>) <$> (threadParents rootMap <$> M.fromList ((\t -> (ttag t, t)) <$> templateList root)) & debug' "PARENT MAP"
  where
    rootMap :: M.Map Text (Maybe Template)
    rootMap = parentMap root & P.debug' "****** threadParents"

    threadParents :: M.Map Text (Maybe Template) -> Template -> [Template]
    threadParents rootmap tmp =
      reverse (prnts isTstHk [] tmp) <> reverse (prnts isThrdHkorGrp [] tmp)
      where
        prnts :: (Template -> Bool) -> [Template] -> Template -> [Template]
        prnts pred acc chld =
          (getValThrow rootmap (ttag chld & P.debug' "SEEKING TAG: ")) & P.debug' "!!!!!!! threadParents  !!!!!!"
            & maybe
              acc
              (\t -> prnts pred (pred t ? t : acc $ acc) t)

        isTstHk :: Template -> Bool
        isTstHk = \case
          TGroup {} -> False
          TOnceHook {} -> False
          TThreadHook {} -> False
          TTestHook {} -> True
          TFixture {} -> False

        isThrdHkorGrp :: Template -> Bool
        isThrdHkorGrp = \case
          TGroup {} -> True
          TOnceHook {} -> False
          TThreadHook {} -> True
          TTestHook {} -> False
          TFixture {} -> False


getValThrow :: (Ord k, Show k, Show v) => M.Map k v -> k -> v
getValThrow m k =
  (m M.!? k) & maybe
    (error $ show k <> " not found in " <> ppShow m)
    id


chkParentOrder :: Template -> [ExeEvent] -> IO ()
chkParentOrder rootTpl thrdEvts =
  chkParents "Parent start events (working back from child)" revEvntStartLocs
    >> chkParents "Parent end events (working forward from child)" evntEndLocs
  where
    tpm = threadParentMap rootTpl & P.debug' "tpm"
    revEvntStartLocs = catMaybes $ boundryLoc True <$> reverse thrdEvts
    evntEndLocs = catMaybes $ boundryLoc False <$> thrdEvts

    -- TODO: Update exception handling with debug info
    chkParents :: Text -> [Loc] -> IO ()
    chkParents errPrefix = \case
      [] -> pure ()
      l : ls ->
        getTag l
          & maybe
            (pure ())
            ( \t ->
                let expected = Just <$> P.debug' "chkParents" (getValThrow tpm (t & P.debug' "SEEKING (chkParents) TAG: "))
                    eaTags = P.debug' "eaTags" $ zip expected ls
                    actual = getTag . snd <$> eaTags
                 in when (length eaTags < length expected || expected /= actual) $
                      error $
                        toS errPrefix
                          <> " not as expected"
                          <> "\n expected:\n"
                          <> ppShow expected
                          <> "\n got:\n"
                          <> ppShow actual
            )

    boundryLoc useStart = \case
      StartExecution {} -> Nothing
      Start {loc} -> useStart ? Just loc $ Nothing
      End {loc} -> useStart ? Nothing $ Just loc
      Failure {} -> Nothing
      ParentFailure {} -> Nothing
      Debug {} -> Nothing
      EndExecution {} -> Nothing

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
      TTestHook {tChild} -> fxCount' ac tChild
      TFixture {tTests} -> ac + 1

mkPrenode :: TextLogger -> Template -> PreNode oi () ti () ii ()
mkPrenode l = \case
  TGroup
    { ttag,
      tChilds
    } -> uu
  TOnceHook
    { ttag,
      shook,
      srelease,
      tChild
    } -> uu
  TThreadHook
    { ttag,
      thook,
      trelease,
      tChild
    } -> uu
  TTestHook
    { ttag,
      thook,
      trelease,
      tChild
    } -> uu
  TFixture
    { ttag,
      tTests
    } -> mkFixture l ttag tTests

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
  let unpack = (snd <$>) . M.toList
      fld m a = case M.lookup (f a) m of
        Nothing -> M.insert (f a) [a] m
        Just as -> M.insert (f a) (a : as) m
   in unpack . foldl' fld M.empty . reverse

-- TODO - better formatting chkEq pyrelude
chkEqf' :: (Eq a, Show a) => a -> a -> Text -> IO ()
chkEqf' e a msg = chkEq' msg e a

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
          let ck = chkEq' "first index of thread should be 1" 1 . idx
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
                chkEqf' (succ idx1) idx2 $
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
  Debug {} -> False
  EndExecution {} -> False

chkMaxThreads :: Int -> [[ExeEvent]] -> IO ()
chkMaxThreads mxThrds threadedEvents =
  -- TODO: this should be 1 but is a workaround for debug using the base thread when set up in the test
  -- fix when logging is fully integrated with test
  let baseThrds = 2 -- should be 1
      allowed = mxThrds + baseThrds
   in chk' --TODO: chk' formatting
        ( "max execution threads + " <> txt baseThrds <> ": "
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
  Debug {} -> boom "Debug"
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

chkTestIdsConsecutive :: [ExeEvent] -> IO ()
chkTestIdsConsecutive evs =
  let tstIds = read . toS . Txt.takeWhileEnd (/= '.') . startTag <$> filter (isStart L.Test) evs
      chkidxless :: Int -> Int -> IO Int
      chkidxless i1 i2 = i1 >= i2 ? error "test index out of order" $ pure i2
   in null tstIds
        ? pure ()
        $ foldM_ chkidxless (-1) tstIds

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
      chkTestIdsConsecutive evts'

    chkEvent :: Maybe Loc -> ExeEvent -> Maybe Loc
    chkEvent mTstLoc evt =
      -- TODO: format on debug'
      -- P.debug' (txt $ ppShow evt) $
      mTstLoc
        & maybe
          ( -- outside fixture
            case evt of
              StartExecution {} -> Nothing
              Start {eventType, loc} -> chkOutEventStartEnd True loc eventType
              End {eventType, loc} -> chkOutEventStartEnd False loc eventType
              Failure {} -> Nothing
              ParentFailure {} -> Nothing
              Debug {} -> Nothing
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
                Debug {} -> mTstLoc
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
                $ fail $ "End " <> strTrg <> " when not started"
            )
            . matchesTarg

        chkInEventStartEnd :: Bool -> Loc -> Loc -> ExeEventType -> Maybe Loc
        chkInEventStartEnd isStart' fxLoc evtLoc evt' =
          let sevt = show evt'
           in matchesTarg evt'
                ? ( isStart'
                      ? fail ("Nested " <> sevt <> " - " <> sevt <> " started when a " <> sevt <> " is alreeady running in the same thread")
                      $ fxLoc == evtLoc
                        ? Nothing
                        $ fail (strTrg <> " end loc does not match " <> strTrg <> " start loc")
                  )
                $ failIn sevt

chkAllLeafEvents :: [[ExeEvent]] -> IO ()
chkAllLeafEvents evts =
  traverse_
    (`chkLeafEvents` evts)
    [ L.OnceHook,
      L.OnceHookRelease,
      L.ThreadHook,
      L.ThreadHookRelease,
      L.TestHook,
      L.TestHookRelease,
      L.Test
    ]

-- # TODO replace prelude: https://github.com/dnikolovv/practical-haskell/blob/main/replace-prelude/README.md
chkFixtures :: [[ExeEvent]] -> IO ()
chkFixtures =
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
              Debug {} -> Nothing
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
                Debug {} -> mfixLoc
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
              $ (fxLoc == evtLoc) ? Nothing $ fail "fixture end loc does not match fixture start loc"
          L.Test {} -> mfixLoc

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

-- chkTestHooks :: Template -> [[ExeEvent]] -> IO ()
-- chkTestHooks t =
--   traverse_ chkEvents
--   where
--     chkEvents :: [ExeEvent] -> IO ()
--     chkEvents evts' =
--       foldl' chkEvent Nothing evts'
--         & maybe
--           (pure ())
--           (\fx -> error $ "Fixture started not ended in same thread\n" <> ppShow fx)

--     -- no need to check called in right part of tree ie. within fixture out of test
--     -- those cases are covered in fixture and test
--     -- we also have check all tests occur in the correct fixture
--     chkEvent :: Maybe Loc -> ExeEvent -> Maybe Loc
--     chkEvent mfixLoc evt =
--       mfixLoc
--         & maybe
--           ( -- outside fixture
--             case evt of
--               StartExecution {} -> Nothing
--               Start {eventType, loc} -> chkOutOfFixtureStartEnd True loc eventType
--               End {eventType, loc} -> chkOutOfFixtureStartEnd False loc eventType
--               Failure {} -> Nothing
--               ParentFailure {} -> Nothing
--               Debug {} -> Nothing
--               EndExecution {} -> Nothing
--           )
--           ( -- within fixture
--             \fxLoc ->
--               case evt of
--                 StartExecution {} -> failIn "StartExecution"
--                 Start {eventType, loc} -> chkInFixtureStartEnd True fxLoc loc eventType
--                 End {eventType, loc} -> chkInFixtureStartEnd False fxLoc loc eventType
--                 Failure {} -> mfixLoc
--                 ParentFailure {} -> mfixLoc
--                 Debug {} -> mfixLoc
--                 EndExecution {} -> failIn "EndExecution"
--           )
--       where
--         fail msg = error $ msg <> "\n" <> ppShow evt
--         failOut et = fail $ et <> " must occur within start / end of fixture in same thread"
--         failIn et = fail $ et <> " must only occur outside a fixture in same thread"

--         chkOutOfFixtureStartEnd :: Bool -> Loc -> ExeEventType -> Maybe Loc
--         chkOutOfFixtureStartEnd isStart' evtLoc = \case
--           L.OnceHook -> Nothing
--           OnceHookRelease -> Nothing
--           L.ThreadHook -> Nothing
--           ThreadHookRelease -> Nothing
--           L.Group -> Nothing
--           L.TestHook -> failOut "TestHook"
--           TestHookRelease -> failOut "TestHookTestHookRelease"
--           L.Fixture ->
--             isStart'
--               ? Just evtLoc
--               $ fail "End fixture when fixture not started"
--           L.Test -> failOut "Test"

--         chkInFixtureStartEnd :: Bool -> Loc -> Loc -> ExeEventType -> Maybe Loc
--         chkInFixtureStartEnd isStart' fxLoc evtLoc = \case
--           L.OnceHook -> failIn "OnceHook"
--           OnceHookRelease -> failIn "OnceHookRelease"
--           L.ThreadHook -> failIn "ThreadHook"
--           ThreadHookRelease -> failIn "ThreadHookRelease"
--           L.TestHook -> mfixLoc
--           TestHookRelease -> mfixLoc
--           L.Group -> failIn "Group"
--           L.Fixture ->
--             isStart'
--               ? fail "Nested fixtures - fixture started when a fixture is alreeady running in the same thread"
--               $ (fxLoc == evtLoc) ? Nothing $ fail "fixture end loc does not match fixture start loc"
--           L.Test {} -> mfixLoc

(~) = (+) 5

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
      Debug {} -> acc
      EndExecution {} -> acc

    chkStart :: M.Map Loc (ST.Set Loc) -> ExeEvent -> M.Map Loc (ST.Set Loc)
    chkStart m e =
      M.member eLoc m
        ? error ("Duplicate start events in same thread\n " <> ppShow e)
        $ M.insert eLoc ST.empty $ ST.insert eLoc <$> m
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
    traverse_
      (evts &)
      [ chkThreadLogsInOrder,
        chkStartEndExecution,
        chkTestCount templateAsList,
        chkFxtrCount templateAsList
      ]
    traverse_
      (threadedEvents &)
      [ chkStartEndIntegrity,
        chkFixtures,
        traverse_ chkTestIdsConsecutive,
        chkAllLeafEvents,
        traverse_ (chkParentOrder t)
      ]
    chk'
      ( "max execution threads + 2: "
          -- 1 main thread + exe threads
          -- TODO: fix this to + 1 when phatom test debug thread is sorted
          <> txt (mxThrds + 2)
          <> " exceeded: "
          <> txt (length threadedEvents)
          <> "\n"
          <> txt (ppShow ((threadId <$>) <$> threadedEvents))
      )
      $ length threadedEvents <= mxThrds + 2
  where
    threadedEvents = groupOn threadId evts
    templateAsList = templateList t

debug :: Text -> Int -> Text -> ExeEvent
debug = Debug

validateTemplate :: Template -> IO ()
validateTemplate t =
  let tl = (ttag <$> templateList t)
      utl = nub tl
   in when (length tl /= length utl) $
        error $ "template must be such that tags are unique: \n" <> ppShow tl

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
  let lgr :: Text -> IO ()
      lgr msg = mkLogger sink ior tid (Debug msg)
  execute maxThreads lc $ mkPrenode lgr template
  log
    & maybe
      (chkFail "No Events Log")
      (\evts -> atomically (q2List evts) >>= chkLaws maxThreads template)

ioActionNonDet :: TextLogger -> IOPropsNonDet -> IO ()
ioActionNonDet log (IOPropsNonDet {message', delayms' = DocFunc {func = getDelay}, fail' = DocFunc {func = getFail}}) =
  do
    delayms <- getDelay
    fail <- getFail
    ioAction log $ IOProps message' delayms fail

ioAction :: TextLogger -> IOProps -> IO ()
ioAction log (IOProps {message, delayms, fail}) =
  do
    log message
    P.threadDelay delayms
    when fail $
      error . toS $ "exception thrown " <> message

mkTest :: TextLogger -> IOProps -> PN.Test si ti ii
mkTest log iop@IOProps {message, delayms, fail} = PN.Test message \a b c -> ioAction log iop

mkFixture :: TextLogger -> Text -> [IOProps] -> PreNode oi () ti () ii ()
mkFixture log tag = PN.Fixture (Just tag) . fmap (mkTest log)

noDelay :: DocFunc Int
noDelay = DocFunc "No Delay" $ pure 0

neverFail :: DocFunc Bool
neverFail = DocFunc "Never Fail" $ pure False

alwaysFail :: DocFunc Bool
alwaysFail = DocFunc "Always Fail" $ pure True

superSimplSuite :: Template
superSimplSuite =
  TFixture "Fx 0" [testProps "Fx 0" 0 0 False]

-- $> unit_simple_single

unit_simple_single :: IO ()
unit_simple_single = runTest 1 superSimplSuite

-- $ > unit_simple_single_failure

unit_simple_single_failure :: IO ()
unit_simple_single_failure = runTest 1 $ TFixture "Fx 0" [testProps "Fx 0" 0 0 True]

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

-- chkFixtures :: [NodeStats] -> [RunEvent] -> IO ()
-- chkFixtures stats evntLst =
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
--   chkFixtures stats l
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
