module Internal.SuiteRuntime where

import BasePrelude (NonTermination, retry)
import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import DSL.Logger (logMessage)
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty, replicateM)
import Data.Tuple.Extra (both, uncurry3)
import GHC.Exts
import Internal.PreNode (Context (..), OnceHook (..), PreNode (testHook), PreNodeRoot, TestHook (..), ThreadHook (..))
import qualified Internal.PreNode as PN (
  PreNode (..),
  PreNodeRoot,
  Test (..),
 )
import Internal.RunTimeLogging (
  ApLogger,
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
import qualified Internal.RunTimeLogging as L.L
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
import Prelude hiding (atomically, newEmptyTMVarIO, newTVarIO)

data Status
  = Pending
  | HookExecuting -- only relevant to hooks
  | Running
  | FullyRunning
  | HookFinalising -- only relevant to hooks
  | Done
  deriving (Show, Eq, Ord)

getStatus :: ExeTree oi ti -> STM Status
getStatus =
  readTVar . getStatusTVar
 where
  getStatusTVar :: ExeTree oi ti -> TVar Status
  getStatusTVar = \case
    XGroup{status} -> status
    -- XTTHook{thSubNodes = XTGroup{status}} -> status
    -- XTGroup {status} -> status
    XFixtures{status} -> status

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
    HookExecuting -> True
    Running -> True
    FullyRunning -> False
    HookFinalising -> False
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

data XTGroup a = XTGroup
  { status :: TVar Status
  , childNodes :: TQueue a
  , fullyRunning :: TQueue a
  , runningCount :: TVar Int
  }

mkXTGroup :: [a] -> IO (XTGroup a)
mkXTGroup children = do
  s <- newTVarIO Pending
  rc <- newTVarIO 0
  q <- newTQueueIO
  fr <- newTQueueIO
  atomically $ traverse_ (writeTQueue q) children
  pure $
    XTGroup
      { status = s
      , childNodes = q
      , fullyRunning = fr
      , runningCount = rc
      }

data OnceVal oi oo = OnceVal
  { hook :: OnceHook oi oo
  , value :: TMVar (Either Abandon oo)
  }

mkOnceVal :: OnceHook oi oo -> IO (OnceVal oi oo)
mkOnceVal h = OnceVal h <$> newEmptyTMVarIO

data ExeTree oi ti where
  XGroup ::
    { loc :: Loc
    , status :: TVar Status
    , oHook :: OnceVal oi oo
    , thrdHook :: ThreadHook oo ti to
    , oSubNodes :: XTGroup (ExeTree oo to)
    } ->
    ExeTree oi ti
  XFixtures ::
    { loc :: Loc
    , status :: TVar Status
    , tHook :: TestHook oi ti () tsto
    , fixtures :: TQueue (PN.Test oi ti tsto)
    , runningCount :: TVar Int
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
        , onceHook
        , threadHooko
        , onceSubNodes
        } -> do
          s <- newTVarIO Pending
          onceHk <- mkOnceVal onceHook
          let loc = nodeLoc title
          childQ <- traverse (prepare' parentLoc 0) onceSubNodes
          chldgrp <- mkXTGroup childQ
          pure $
            XGroup
              { loc
              , status = s
              , oHook = onceHk
              , thrdHook = threadHooko
              , oSubNodes = chldgrp
              }
      PN.Fixtures
        { testHook
        , title
        , fixtures
        } ->
          do
            let loc = nodeLoc title
                converTest PN.Test{tstId, tst} = PN.Test tstId tst
            s <- newTVarIO Pending
            q <- newTQueueIO
            runningCount <- newTVarIO 0
            atomically $ traverse_ (writeTQueue q . converTest) uu -- fixtures
            pure $
              XFixtures
                { loc = loc
                , status = s
                , fixtures = q
                , runningCount
                , tHook = testHook
                }

logAbandonned :: EventLogger -> Loc -> ExeEventType -> Abandon -> IO ()
logAbandonned logger floc fet Abandon{sourceLoc, sourceEventType, exception} =
  logger (mkParentFailure floc fet sourceLoc sourceEventType exception)

logFailure :: EventLogger -> Loc -> ExeEventType -> SomeException -> IO ()
logFailure evLogger loc et e = evLogger (mkFailure loc et (txt et <> "Failed at: " <> txt loc) e)

readOrLockHook :: TVar Status -> TMVar (Either Abandon ho) -> STM (Maybe (Either Abandon ho))
readOrLockHook hs hVal =
  do
    s <- readTVar hs
    (==) s Pending
      ? (writeTVar hs HookExecuting >> pure Nothing)
      $ (Just <$> readTMVar hVal)

setRunningVal :: TVar Status -> TMVar (Either Abandon ho) -> Either Abandon ho -> STM (Either Abandon ho)
setRunningVal hs hVal eso = do
  putTMVar hVal eso
  -- success or failure always running will be set done by closing hook
  writeTVar hs Running
  pure eso

runOrReturn :: TVar Status -> TMVar (Either Abandon ho) -> IO (Either Abandon ho) -> IO (Either Abandon ho)
runOrReturn hkStatus hkVal hkAction =
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

normalHookVal :: forall hi ho. EventLogger -> Context -> ExeEventType -> (Context -> hi -> IO ho) -> hi -> TVar Status -> TMVar (Either Abandon ho) -> IO (Either Abandon ho)
normalHookVal evtLogger ctx hkEvent hook hi hs hkVal =
  runOrReturn
    hs
    hkVal
    ( runLogHook evtLogger ctx hkEvent hook hi
        >>= atomically . setRunningVal hs hkVal
    )

withStartEnd :: EventLogger -> Loc -> ExeEventType -> IO a -> IO a
withStartEnd logger loc evt io = do
  logger $ Start evt loc
  finally io . logger $ End evt loc

abandonLogHook :: ExeEventType -> EventLogger -> Abandon -> Loc -> IO (Either Abandon a)
abandonLogHook hkEvent logger abandon floc =
  do
    withStartEnd logger floc hkEvent $
      logAbandonned logger floc hkEvent abandon
    pure $ Left abandon

abandonnedHookVal :: forall ho. ExeEventType -> EventLogger -> Abandon -> TVar Status -> TMVar (Either Abandon ho) -> Loc -> IO (Either Abandon ho)
abandonnedHookVal hkEvent logger abandon hs hkVal loc =
  runOrReturn
    hs
    hkVal
    ( abandonLogHook hkEvent logger abandon loc
        >>= atomically . setRunningVal hs hkVal
    )

threadHookVal :: forall oi ti to. EventLogger -> Context -> Either Abandon (ExeIn oi ti) -> ThreadHook oi ti to -> IO (Either Abandon to)
threadHookVal evLgr ctx@Context{loc, logger} hkIn thook =
  hkIn
    & either
      (\abandon -> abandonLogHook L.ThreadHook evLgr abandon loc)
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

onceHookVal :: forall hi ho. EventLogger -> Context -> ExeEventType -> Either Abandon hi -> TVar Status -> OnceVal hi ho -> IO (Either Abandon ho)
onceHookVal evtLgr ctx@Context{loc, logger} hkEvent ehi hs OnceVal{hook = oHook, value} =
  ehi
    & either
      (\abandon -> abandonnedHookVal hkEvent evtLgr abandon hs value loc)
      ( \hi' ->
          let
            passThrough = pure @IO $ Right @Abandon hi'
            runHk hk = normalHookVal evtLgr ctx hkEvent hk hi' hs value
           in
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

releaseHookUpdateStatus ::
  EventLogger ->
  ExeEventType ->
  TVar Status ->
  Either Abandon ho ->
  Context ->
  OnceHook oi ho ->
  IO ()
releaseHookUpdateStatus evtlgr evt ns eho ctx hk =
  finally
    ( hk & \case
        OnceNone -> pure ()
        OnceBefore{} -> pure ()
        OnceAfter{releaseOnly} -> releaseHook evtlgr ctx evt eho releaseOnly
        OnceAround{release} -> releaseHook evtlgr ctx evt eho release
        -- releaseHook logger evt eho ctx hkRelease
    )
    (atomically $ writeTVar ns Done)

discardDone :: TQueue (ExeTree oi ti) -> STM ()
discardDone = processWhile nodeDone

processWhile :: forall oi ti. (ExeTree oi ti -> STM Bool) -> TQueue (ExeTree oi ti) -> STM ()
processWhile p q =
  tryPeekTQueue q
    >>= maybe
      (pure ())
      (p >=> bool (pure ()) (readTQueue q >> processWhile p q))

discardDoneMoveFullyRunning :: TQueue (ExeTree oi ti) -> TQueue (ExeTree oi ti) -> STM ()
discardDoneMoveFullyRunning fullyRunningQ =
  processWhile
    ( \n ->
        getStatus n >>= \s ->
          if
              | s == Done -> pure True
              | s > Running -> writeTQueue fullyRunningQ n >> pure True
              | otherwise -> pure False
    )

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
          { loc = hookLoc
          , status
          , oHook = oHook@OnceVal{hook}
          , thrdHook
          , oSubNodes = XTGroup{status = childrenStatus}
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
                  ctx = context hookLoc
                  releaseContext = context . Node hookLoc $ txt L.OnceHookRelease
              eso <- onceHookVal eventLogger ctx L.OnceHook siHkIn status oHook
              finally
                ( eso
                    & either
                      (recurse . Left)
                      (runThreadHook . nxtHkIn)
                )
                ( do
                    wantRelease <- atomically $ do
                      childStatus <- readTVar childrenStatus
                      s <- readTVar status
                      pure $ childStatus == Done && s < HookFinalising
                    when wantRelease $
                      releaseHookUpdateStatus eventLogger L.OnceHookRelease status eso releaseContext hook
                )
           where
            runThreadHook thHkin =
              do
                let nxtHkIn ti = (\exi -> exi{threadIn = ti}) <$> hkIn
                    recurse a = uu -- todo exeNxt a thSubNodes -- write exeNxt in terms of XTGroup
                    hkCtx = context hookLoc
                    releaseCtx = context . Node hookLoc $ txt L.ThreadHookRelease
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
          , runningCount
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
                mtest <- tryReadTQueue fixtures
                mtest
                  & maybe
                    ( do
                        r <- readTVar runningCount
                        Left
                          <$> if
                              | r < 0 -> error "framework error - this should not happen - running count below zero"
                              | r == 0 -> pure True
                              | otherwise -> writeTVar status FullyRunning >> pure False
                    )
                    ( \t -> do
                        modifyTVar runningCount succ
                        setStatusRunning status
                        pure $ Right t
                    )
              etest
                & either
                  ( \done -> do
                      when done $
                        atomically (writeTVar status Done)
                  )
                  ( \PN.Test{tstId, tst} -> do
                      to <- runTestHook (tstHkloc tstId) fxIpts () tHook
                      let unpak io' (ExeIn so2 to2) = (so2, to2, io')
                          ethInputs = liftA2 unpak to fxIpts
                          testLoc = tstLoc tstId

                      finally
                        ( withStartEnd eventLogger testLoc L.Test $
                            ethInputs & either
                              (logAbandonned' testLoc L.Test)
                              \(so2, to2, io') ->
                                catchAll
                                  (tst msgLogger so2 to2 io')
                                  (logFailure' (tstLoc tstId) L.Test)
                        )
                        do
                          releaseTestHook tstId to tHook
                          atomically (modifyTVar runningCount pred)
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