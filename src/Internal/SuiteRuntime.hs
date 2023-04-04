module Internal.SuiteRuntime where

import BasePrelude (NonTermination, retry)
import qualified BasePrelude as PN
import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import DSL.Logger (logMessage)
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty, replicateM, update)
import Data.Tuple.Extra (both, uncurry3)
import GHC.Exts
import Internal.PreNode (Context (..), OnceHook (..), PreNode (testHook), PreNodeRoot, TestHook (..), ThreadHook (..))
import qualified Internal.PreNode as PN (
  Fixture (..),
  PreNode (..),
  PreNodeRoot,
  Test (..),
 )
import Internal.RunTimeLogging (
  MessageLogger,
  EventSink,
  ExeEvent (..),
  ExeEventType (Group, TestHookRelease),
  Loc (Node, Root),
  LogControls (LogControls),
  SThreadId,
  logWorker,
  mkFailure,
  mkLogger,
  mkParentFailure,
  sink,
  stopWorker,
 )
import qualified Internal.RunTimeLogging as L
import LogTransformation.PrintLogDisplayElement (PrintLogDisplayElement (tstTitle))
import Polysemy.Bundle (subsumeBundle)
import PyrethrumExtras hiding (finally)
import PyrethrumExtras.IO (hPutStrLn, putStrLn)
import Text.Show.Pretty (pPrint)
import UnliftIO (
  Exception (displayException),
  bracket,
  catchAny,
  concurrently_,
  finally,
  forConcurrently_,
  isEmptyTBQueue,
  newIORef,
  newMVar,
  newTMVar,
  peekTQueue,
  pureTry,
  replicateConcurrently,
  replicateConcurrently_,
  swapTMVar,
  tryAny,
  tryPeekTQueue,
  tryReadTMVar,
  unGetTBQueue,
  wait,
  withAsync,
 )
import UnliftIO.Concurrent
import UnliftIO.Concurrent as C (ThreadId, forkFinally, forkIO, killThread, takeMVar, threadDelay, withMVar)
import UnliftIO.STM (
  STM,
  TMVar,
  TQueue,
  TVar,
  atomically,
  isEmptyTMVar,
  isEmptyTQueue,
  modifyTVar,
  newEmptyTMVar,
  newEmptyTMVarIO,
  newTMVarIO,
  newTQueueIO,
  newTVarIO,
  putTMVar,
  readTMVar,
  readTQueue,
  readTVar,
  readTVarIO,
  takeTMVar,
  tryReadTQueue,
  writeTQueue,
  writeTVar,
 )
import Prelude hiding (atomically, id, newEmptyTMVarIO, newTVarIO)

data HookStatus
  = HookPending
  | HookRunning
  | ChildRunning
  | ReleaseRunning
  | HookFinalising
  | HookDone
  | HookVoid
  deriving (Show, Eq)
data Status
  = Pending
  | Running
  | FullyRunning
  | Done
  deriving (Show, Eq, Ord)

getStatus :: ExeTree oi ti -> STM Status
getStatus = uu

--   readTVar . getStatusTVar
--  where
--   getStatusTVar :: ExeTree oi ti -> TVar Status
--   getStatusTVar = \case
--     XGroup{status} -> status
--     -- XTTHook{thSubNodes = XTGroup{status}} -> status
--     -- XTGroup {status} -> status
--     XFixtures{status} -> status

isDone :: Status -> Bool
isDone = (== Done)

nodeDone :: ExeTree oi ti -> STM Bool
nodeDone t = (== Done) <$> getStatus t

canRun :: ExeTree oi ti -> STM Bool
canRun rg =
  canRun' <$> getStatus rg
 where
  canRun' = \case
    Pending -> True
    -- HookExecuting -> True
    Running -> True
    FullyRunning -> False
    -- HookFinalising -> False
    Done -> False

waitDone :: ExeTree oi ti -> IO ()
waitDone rg = atomically $ do
  s <- getStatus rg
  unless (s == Done) retry

data IdxLst a = IdxLst
  { maxIndex :: Int
  , lst :: [a]
  , currIdx :: TVar Int
  }

mkIdxLst :: a -> STM (IdxLst a)
mkIdxLst elm = IdxLst 0 [elm] <$> newTVar 0

data ChildQ a = ChildQ
  { status :: TVar Status
  , childNodes :: TQueue a
  , runningCount :: TVar Int
  , maxThreads :: Maybe Int
  }

data XTest si ti ii = Test
  { id :: Text
  , test :: MessageLogger -> si -> ti -> ii -> IO ()
  , status :: TVar Status
  }

mkXTest :: PN.Test si ti ii -> STM (XTest si ti ii)
mkXTest PN.Test{id, test} = Test id test <$> newTVar Pending

data CanRun
  = Runnable
  | Saturated
  | RunDone

runChildQ :: forall a. (a -> IO ()) -> (a -> STM CanRun) -> ChildQ a -> IO ()
runChildQ runner canRun' q@ChildQ{childNodes, status, runningCount, maxThreads} =
  do
    eNext <- atomically $ do
      let pop = tryReadTQueue childNodes
      rc <- readTVar runningCount
      mNext <-
        maxThreads
          & maybe
            pop
            (\mt -> rc < mt ? pop $ pure Nothing)
      mNext
        & maybe
          ( do
              Left
                <$> if
                    | rc < 0 -> error "framework error - this should not happen - running count below zero"
                    | rc == 0 -> pure True
                    | otherwise -> writeTVar status FullyRunning >> pure False
          )
          ( \a -> do
              modifyTVar runningCount succ
              setStatusRunning status
              pure $ Right a
          )
    eNext
      & either
        ( \done ->
            when done $
              atomically $
                writeTVar status Done
        )
        ( \a ->
            do
              finally
                do
                  cr <- atomically $ canRun' a
                  cr & \case
                    -- runner MUST ensure the integrity of sub element status and handle all exceptions
                    -- element is placed back on the end of the q before running
                    Runnable -> atomically (writeTQueue childNodes a) >> runner a
                    Saturated -> pure ()
                    RunDone -> pure ()
                do
                  atomically $ modifyTVar runningCount pred
              runChildQ runner canRun' q
        )

mkChildQ :: Maybe Int -> [a] -> IO (ChildQ a)
mkChildQ maxC children = do
  s <- newTVarIO Pending
  q <- newTQueueIO
  rc <- newTVarIO 0
  atomically $ traverse_ (writeTQueue q) children
  pure $
    ChildQ
      { status = s
      , childNodes = q
      , runningCount = rc
      , maxThreads = maxC
      }

data OnceVal oi oo = OnceVal
  { hook :: OnceHook oi oo
  , status :: TVar HookStatus
  , value :: TMVar (Either Abandon oo)
  }

mkOnceVal :: OnceHook oi oo -> IO (OnceVal oi oo)
mkOnceVal h = OnceVal h <$> newTVarIO HookPending <*> newEmptyTMVarIO

data XFixture oi ti tsti where
  XFixture ::
    { loc :: Loc
    , status :: TVar Status
    , onceHook :: OnceVal oi oo
    , threadHook :: ThreadHook oo ti to
    , testHook :: TestHook oi ti tsti tsto
    , tests :: ChildQ (XTest oo to io)
    } ->
    XFixture oi ti ii

canRunXFixture :: XFixture oi ti tsti -> STM CanRun
canRunXFixture XFixture{onceHook, threadHook, testHook, tests} = do
  ohs <- readTVar onceHook.status
  uu

mkXFixture ::
  Loc ->
  PN.Fixture oi ti tsti ->
  IO (XFixture oi ti tsti)
mkXFixture loc PN.Fixture{onceHook, threadHook, testHook, tests, maxThreads} = do
  s <- newTVarIO Pending
  oh <- mkOnceVal onceHook
  xts <- atomically $ traverse mkXTest tests
  ts <- mkChildQ maxThreads xts
  pure $
    XFixture
      { loc
      , status = s
      , onceHook = oh
      , threadHook = threadHook
      , testHook = testHook
      , tests = ts
      }

data ExeTree oi ti where
  XGroup ::
    { loc :: Loc
    , maxThreads :: Maybe Int
    , oHook :: OnceVal oi oo
    , thrdHook :: ThreadHook oo ti to
    , childQ :: ChildQ (ExeTree oo to)
    } ->
    ExeTree oi ti
  XFixtures ::
    { loc :: Loc
    , maxThreads :: Maybe Int
    , status :: TVar Status
    , tHook :: TestHook oi ti () tsto
    , fixtures :: ChildQ (XFixture oi ti tsto)
    } ->
    ExeTree oi ti

prepare :: PreNode o t -> IO (ExeTree o t)
prepare =
  prepare' Root 0
 where
  consNoMxIdx :: IdxLst a -> a -> IdxLst a
  consNoMxIdx l@IdxLst{lst} i = l{lst = i : lst}

  prepare' :: Loc -> Int -> PN.PreNode oi ti -> IO (ExeTree oi ti)
  prepare' parentLoc subElmIdx pn = do
    let nodeLoc tag =
          Node
            { parent = parentLoc
            , tag = tag
            }
    case pn of
      PN.Group
        { title
        , maxThreads
        , onceHook
        , threadHook
        , onceSubNodes
        } -> do
          onceHk <- mkOnceVal onceHook
          let loc = nodeLoc title
          childQ <- traverse (prepare' parentLoc 0) onceSubNodes
          chldgrp <- mkChildQ maxThreads childQ
          pure $
            XGroup
              { loc
              , maxThreads
              , oHook = onceHk
              , thrdHook = threadHook
              , childQ = chldgrp
              }
      PN.Fixtures
        { title
        , maxThreads
        , testHook
        , fixtures
        } ->
          do
            let loc = nodeLoc title
            s <- newTVarIO Pending
            runningCount <- newTVarIO 0
            fxs <- traverse (mkXFixture loc) fixtures
            q <- mkChildQ maxThreads fxs
            pure $
              XFixtures
                { loc = loc
                , maxThreads
                , status = s
                , fixtures = q
                , tHook = testHook
                }

logAbandonned :: EventLogger -> Loc -> ExeEventType -> Abandon -> IO ()
logAbandonned logger floc fet Abandon{sourceLoc, sourceEventType, exception} =
  logger (mkParentFailure floc fet sourceLoc sourceEventType exception)

logFailure :: EventLogger -> Loc -> ExeEventType -> SomeException -> IO ()
logFailure evLogger loc et e = evLogger (mkFailure loc et (txt et <> "Failed at: " <> txt loc) e)

readOrLockHook :: TVar HookStatus -> TMVar (Either Abandon ho) -> STM (Maybe (Either Abandon ho))
readOrLockHook hs hVal =
  do
    s <- readTVar hs
    (==) s HookPending
      ? (writeTVar hs HookRunning >> pure Nothing)
      $ (Just <$> readTMVar hVal)

setRunningVal :: TVar Status -> TMVar (Either Abandon ho) -> Either Abandon ho -> STM (Either Abandon ho)
setRunningVal hs hVal eso = do
  putTMVar hVal eso
  -- success or failure always running will be set done by closing hook
  writeTVar hs Running
  pure eso

tryLockRun :: TVar HookStatus -> TMVar (Either Abandon ho) -> IO (Either Abandon ho) -> IO (Either Abandon ho)
tryLockRun hkStatus hkVal hkAction =
  atomically (readOrLockHook hkStatus hkVal)
    >>= maybe
      hkAction
      pure

runLogHook :: forall hi ho. EventLogger -> Context -> ExeEventType -> (Context -> hi -> IO ho) -> hi -> IO (Either Abandon ho)
runLogHook evtLogger ctx@Context{loc} hkEvent hook hi =
  withStartEnd evtLogger loc hkEvent $
    catchAll
      ( Right <$> hook ctx hi
      )
      ( \e ->
          logFailure evtLogger loc hkEvent e
            >> pure (Left $ Abandon loc hkEvent e)
      )

-- onceHookVal :: forall hi ho. EventLogger -> Context -> ExeEventType -> (Context -> hi -> IO ho) -> hi -> TVar Status -> TMVar (Either Abandon ho) -> IO (Either Abandon ho)
-- onceHookVal evtLogger ctx hkEvent hook hi hs hkVal =
--   runOrReturn
--     hs
--     hkVal
--     ( runLogHook evtLogger ctx hkEvent hook hi
--         >>= atomically . setRunningVal hs hkVal
--     )

withStartEnd :: EventLogger -> Loc -> ExeEventType -> IO a -> IO a
withStartEnd logger loc evt io = do
  logger $ Start evt loc
  finally io . logger $ End evt loc

abandonLogHook :: EventLogger -> ExeEventType -> Abandon -> Loc -> IO (Either Abandon a)
abandonLogHook logger evtTp abandon floc =
  do
    withStartEnd logger floc evtTp $
      logAbandonned logger floc evtTp abandon
    pure $ Left abandon

abandonnedOnceHookVal :: forall ho. EventLogger -> Abandon -> TVar HookStatus -> TMVar (Either Abandon ho) -> Loc -> IO (Either Abandon ho)
abandonnedOnceHookVal logger abandon hs hkVal loc =
  tryLockRun
    hs
    hkVal
    ( do
        abandonLogHook logger L.OnceHook abandon loc
        atomically $ writeTVar hs ChildRunning
        pure $ Left abandon
    )

threadHookVal :: forall oi ti to. EventLogger -> Context -> Either Abandon (ExeIn oi ti) -> ThreadHook oi ti to -> IO (Either Abandon to)
threadHookVal evLgr ctx@Context{loc, logger} hkIn thook =
  hkIn
    & either
      (\abandon -> abandonLogHook evLgr L.ThreadHook abandon loc)
      ( \(ExeIn oi ti) ->
          let
            thrdHk :: (Context -> oi -> ti -> IO to) -> Context -> ti -> IO to
            thrdHk thHk = flip thHk oi
            runHook hook' = runLogHook evLgr ctx L.ThreadHook (thrdHk hook') ti
           in
            thook & \case
              ThreadNone -> pure $ Right ti
              ThreadBefore{hook} -> runHook hook
              ThreadAfter{} -> pure $ Right ti
              ThreadAround{hook} -> runHook hook
      )

onceHookVal :: forall hi ho. EventLogger -> Context -> ExeEventType -> Either Abandon hi -> OnceVal hi ho -> IO (Either Abandon ho)
onceHookVal evtLgr ctx@Context{loc, logger} hkEvent ehi OnceVal{hook = oHook, status, value} =
  ehi
    & either
      (\abandon -> abandonnedOnceHookVal evtLgr abandon status value loc)
      ( \hi' ->
          let
            passThrough = pure @IO $ Right @Abandon hi'
            runHk hk =
              tryLockRun
                status
                value
                ( runLogHook evtLgr ctx hkEvent hk hi'
                    >>= uu --atomically . setRunningVal status value
                )
           in
            --- onceHookVal evtLgr ctx hkEvent hk hi' status value

            oHook & \case
              -- Hmmm may have type issues here later
              OnceNone -> passThrough
              OnceAfter{} -> passThrough
              OnceBefore{hook} -> runHk hook
              OnceAround{hook} -> runHk hook
      )

setStatusRunning :: TVar Status -> STM Bool
setStatusRunning status =
  do
    s <- readTVar status
    let change = s < Running
    when change $
      writeTVar status Running
    pure change

releaseHook :: EventLogger -> Context -> ExeEventType -> Either Abandon ho -> (Context -> ho -> IO ()) -> IO ()
releaseHook evtlgr ctx@Context{loc, logger} evt eho hkRelease =
  withStartEnd evtlgr loc evt $
    eho
      & either
        (logAbandonned evtlgr loc evt)
        ( \so ->
            catchAll
              (hkRelease ctx so)
              (evtlgr . mkFailure loc evt ("Hook Release Failed: " <> txt evt <> " " <> txt loc))
        )

releaseOnceHook ::
  EventLogger ->
  Either Abandon ho ->
  Context ->
  OnceVal oi ho ->
  IO ()
releaseOnceHook evtlgr eho ctx hk =
  do
    doRealease <- atomically tryLock
    when doRealease $
      finally
        ( hk.hook & \case
            OnceNone -> pure ()
            OnceBefore{} -> pure ()
            OnceAfter{releaseOnly} -> doRelease releaseOnly
            OnceAround{release} -> doRelease release
        )
        uu --(atomically $ writeTVar hk.status Done)
 where
  doRelease = releaseHook evtlgr ctx L.OnceHookRelease eho
  tryLock :: STM Bool
  tryLock =
    do
      s <- readTVar hk.status
      let result = uu --s < HookFinalising
      when result $
        writeTVar hk.status HookFinalising
      pure result

-- discardDone :: TQueue (ExeTree oi ti) -> STM ()
-- discardDone = processWhile nodeDone

-- processWhile :: forall oi ti. (ExeTree oi ti -> STM Bool) -> TQueue (ExeTree oi ti) -> STM ()
-- processWhile p q =
--   tryPeekTQueue q
--     >>= maybe
--       (pure ())
--       (p >=> bool (pure ()) (readTQueue q >> processWhile p q))

-- discardDoneMoveFullyRunning :: TQueue (ExeTree oi ti) -> TQueue (ExeTree oi ti) -> STM ()
-- discardDoneMoveFullyRunning fullyRunningQ =
--   processWhile
--     ( \n ->
--         getStatus n >>= \s ->
--           if
--               | s == Done -> pure True
--               | s > Running -> writeTQueue fullyRunningQ n >> pure True
--               | otherwise -> pure False
--     )

data Abandon = Abandon
  { sourceLoc :: Loc
  , sourceEventType :: ExeEventType
  , exception :: SomeException
  }
  deriving (Show)

data ExeIn oi ti = ExeIn
  { onceIn :: oi
  , threadIn :: ti
  }

executeNode :: forall oi ti. EventLogger -> Either Abandon (ExeIn oi ti) -> ExeTree oi ti -> IO ()
executeNode eventLogger hkIn rg =
  do
    wantRun <- atomically $ canRun rg
    when
      wantRun
      case rg of
        XGroup
          { loc
          , maxThreads
          , oHook
          , thrdHook
          , childQ
          } ->
            do
              -- onceHookVal:
              --  1. runs hook if required
              --  2. waits if hook is running
              --  3. updates hook status to running
              --  4. returns hook result
              -- must run for logging even if hkIn is Left
              let nxtHkIn so = (\exi -> exi{onceIn = so}) <$> hkIn
                  recurse a = uu -- to doexeNxt a oSubNodes
                  ctx = context loc
                  releaseContext = context . Node loc $ txt L.OnceHookRelease
              eso <- onceHookVal eventLogger ctx L.OnceHook siHkIn oHook
              finally
                ( eso
                    & either
                      (recurse . Left)
                      (runThreadHook . nxtHkIn)
                )
                ( do
                    wantRelease <- atomically $ do
                      childStatus <- readTVar childQ.status
                      s <- readTVar oHook.status
                      pure $ childStatus == Done && uu --s < HookFinalising
                    when wantRelease $
                      releaseOnceHook eventLogger eso releaseContext oHook
                )
           where
            runThreadHook thHkin =
              do
                let nxtHkIn ti = (\exi -> exi{threadIn = ti}) <$> hkIn
                    recurse a = uu -- todo exeNxt a thSubNodes -- write exeNxt in terms of XTGroup
                    hkCtx = context loc
                    releaseCtx = context . Node loc $ txt L.ThreadHookRelease
                eto <- threadHookVal eventLogger hkCtx thHkin thrdHook
                finally
                  ( eto
                      & either
                        (recurse . Left)
                        (recurse . nxtHkIn)
                  )
                  ( let
                      doRelease = releaseHook eventLogger releaseCtx L.ThreadHookRelease eto
                     in
                      thrdHook & \case
                        ThreadNone -> pure ()
                        ThreadBefore{} -> pure ()
                        ThreadAfter{releaseOnly} -> doRelease releaseOnly
                        ThreadAround{release} -> doRelease release
                  )
        fx@XFixtures
          { status
          , loc
          , fixtures
          , tHook
          } ->
            withStartEnd eventLogger loc L.Fixture $ do
              recurse hkIn
           where
            (<<::>>) t i = t <> " :: " <> i
            tstHkloc tstid = Node loc $ txt L.TestHook <<::>> tstid
            tstHkReleaseloc tstid = Node (tstHkloc tstid) $ txt L.TestHookRelease <<::>> tstid
            tstLoc tstid = Node (tstHkloc tstid) $ txt L.Test <<::>> tstid
            recurse fxIpts = do
              etest <- atomically $ do
                mtest <- tryReadTQueue uu -- fixtures
                mtest
                  & maybe
                    ( do
                        r <- readTVar uu -- runningCount
                        Left
                          <$> if
                              | r < 0 -> error "framework error - this should not happen - running count below zero"
                              | r == 0 -> pure True
                              | otherwise -> writeTVar status FullyRunning >> pure False
                    )
                    ( \t -> do
                        -- modifyTVar runningCount succ
                        setStatusRunning status
                        pure $ Right t
                    )
              etest
                & either
                  ( \done -> do
                      when done $
                        atomically (writeTVar status Done)
                  )
                  ( \PN.Test{id, test} -> do
                      to <- runTestHook (tstHkloc id) fxIpts () tHook
                      let unpak io' (ExeIn so2 to2) = (so2, to2, io')
                          ethInputs = liftA2 unpak to fxIpts
                          testLoc = tstLoc id

                      finally
                        ( withStartEnd eventLogger testLoc L.Test $
                            ethInputs & either
                              (logAbandonned' testLoc L.Test)
                              \(so2, to2, io') ->
                                catchAll
                                  (test msgLogger so2 to2 io')
                                  (logFailure' (tstLoc id) L.Test)
                        )
                        do
                          releaseTestHook id to tHook
                          uu -- atomically (modifyTVarrunningCount pred)
                          recurse fxIpts
                  )

            releaseTestHook :: forall so' to' tsti' tsto'. Text -> Either Abandon tsto' -> TestHook so' to' tsti' tsto' -> IO ()
            releaseTestHook tstId tsti = \case
              TestNone -> noRelease
              TestBefore{} -> noRelease
              TestAfter{releaseOnly} -> runRelease uu -- (\l a _tsto -> releaseOnly l a tsti)
              TestAround{release} -> runRelease release
             where
              noRelease = pure ()
              runRelease = releaseHook eventLogger (context $ tstHkReleaseloc tstId) L.TestHookRelease tsti

            runTestHook :: forall so' to' tsti' tsto'. Loc -> Either Abandon (ExeIn so' to') -> tsti' -> TestHook so' to' tsti' tsto' -> IO (Either Abandon tsto')
            runTestHook hkLoc fxIpts tsti testHk =
              do
                fxIpts
                  & either
                    logReturnAbandonned
                    runHook
             where
              logReturnAbandonned a =
                let
                  result = pure (Left a)
                  loga = logAbandonned eventLogger hkLoc L.TestHook a >> result
                 in
                  testHk & \case
                    TestNone{} -> result
                    TestBefore{hook} -> loga
                    TestAfter{} -> result
                    TestAround{hook} -> loga

              runHook (ExeIn oi ti) =
                catchAll
                  ( let
                      exHk h = Right <$> withStartEnd eventLogger hkLoc L.TestHook (h (context hkLoc) oi ti tsti)
                      voidHk = pure @IO $ Right @Abandon tsti
                     in
                      testHk & \case
                        TestNone{} -> voidHk
                        TestBefore{hook} -> exHk hook
                        TestAfter{} -> voidHk
                        TestAround{hook} -> exHk hook
                  )
                  ( \e -> do
                      logFailure eventLogger hkLoc L.TestHook e
                      pure . Left $ Abandon hkLoc L.TestHook e
                  )
 where
  siHkIn :: Either Abandon oi
  siHkIn = (.onceIn) <$> hkIn

  exeNxt :: forall oi' ti'. Either Abandon (ExeIn oi' ti') -> ExeTree oi' ti' -> IO ()
  exeNxt = executeNode eventLogger

  nxtAbandon :: Loc -> ExeEventType -> SomeException -> Abandon
  nxtAbandon loc' et e = fromLeft (Abandon loc' et e) hkIn

  logAbandonned' :: Loc -> ExeEventType -> Abandon -> IO ()
  logAbandonned' = logAbandonned eventLogger

  logFailure' :: Loc -> ExeEventType -> SomeException -> IO ()
  logFailure' = logFailure eventLogger

  context :: Loc -> Context
  context l = Context l msgLogger

  msgLogger :: Text -> IO ()
  msgLogger msg = eventLogger $ \idx trdId ->
    ApLog
      { idx = idx
      , threadId = trdId
      , msg = msg
      }

type EventLogger = (Int -> SThreadId -> ExeEvent) -> IO ()

newLogger :: EventSink -> ThreadId -> IO EventLogger
newLogger sink tid =
  do
    ir <- UnliftIO.newIORef (-1)
    pure $ mkLogger sink ir tid

executeGraph :: EventSink -> ExeTree () () -> Int -> IO ()
executeGraph sink xtri maxThreads =
  let hkIn = Right (ExeIn () ())
      thrdTokens = replicate maxThreads True
   in do
        rootThreadId <- myThreadId
        rootLogger <- newLogger sink rootThreadId
        finally
          ( rootLogger StartExecution
              >> forConcurrently_
                thrdTokens
                ( const do
                    tid' <- myThreadId
                    logger' <- newLogger sink tid'
                    executeNode logger' hkIn xtri
                )
          )
          (waitDone xtri >> rootLogger EndExecution)

execute :: Int -> LogControls m -> PN.PreNodeRoot -> IO ()
execute
  maxThreads
  LogControls
    { sink
    , logWorker
    , stopWorker
    }
  preRoot =
    concurrently_ logWorker linkExecute
   where
    linkExecute :: IO ()
    linkExecute =
      finally
        ( do
            exeTree <- prepare preRoot
            executeGraph sink exeTree maxThreads
        )
        stopWorker