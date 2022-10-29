module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import DSL.Logger (logMessage)
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty, replicateM)
import Data.Tuple.Extra (both, uncurry3)
import GHC.Exts
import Internal.PreNode (PreNodeRoot, PreNode (testHook))
import qualified Internal.PreNode as PN
  ( PreNode (..),
    PreNodeRoot,
    Test (..),
  )
import Internal.RunTimeLogging
  ( ExeEvent (..),
    ExeEventType (Group, TestHookRelease),
    Loc (Node, Root),
    LogControls (LogControls),
    EventSink,
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
import Pyrelude as P hiding
  ( ThreadRunning,
    ThreadStatus,
    atomically,
    bracket,
    fail,
    finally,
    newMVar,
    newTVarIO,
    parent,
    pi,
    readTVarIO,
    threadStatus,
    withMVar,
  )
import Pyrelude.IO (hPutStrLn, putStrLn)
import Text.Show.Pretty (pPrint)
import UnliftIO
  ( Exception (displayException),
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
import UnliftIO.Concurrent as C (ThreadId, forkFinally, forkIO, killThread, takeMVar, threadDelay, withMVar)
import UnliftIO.STM
  ( STM,
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

data Status
  = Pending
  | HookExecuting -- only relevant to hooks
  | Running
  | FullyRunning
  | HookFinalising -- only relevant to hooks
  | Done
  deriving (Show, Eq, Ord)

getStatus :: ExeTree si so ti to -> STM Status
getStatus =
  readTVar . getStatusTVar
  where
    getStatusTVar :: ExeTree si so ti to -> TVar Status
    getStatusTVar = \case
      XTSHook {status} -> status
      XTTHook {thChildNode} -> getStatusTVar thChildNode
      XTGroup {nStatus} -> nStatus
      XTFix {nStatus} -> nStatus

isDone :: Status -> Bool
isDone = (== Done)

nodeDone :: ExeTree si so ti to -> STM Bool
nodeDone t = (== Done) <$> getStatus t

canRun :: ExeTree si so ti to -> STM Bool
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

modifyStatus :: (Status -> Status) -> ExeTree si so ti to -> STM ()
modifyStatus f et =
  case et of
    XTSHook {status} -> update status
    XTTHook {} -> pure ()
    XTGroup {nStatus} -> update nStatus
    XTFix {nStatus} -> update nStatus
  where
    update = flip modifyTVar f

waitDone :: ExeTree si so ti to -> IO ()
waitDone rg = atomically $ do
  s <- getStatus rg
  unless (s == Done) retry

data IdxLst a = IdxLst
  { maxIndex :: Int,
    lst :: [a],
    currIdx :: TVar Int
  }

mkIdxLst :: a -> STM (IdxLst a)
mkIdxLst elm = IdxLst 0 [elm] <$> newTVar 0

data ExeTree si so ti to where
  XTSHook ::
    { loc :: Loc,
      status :: TVar Status,
      sHook :: si -> IO so,
      sHookRelease :: so -> IO (),
      sHookVal :: TMVar (Either Abandon so),
      sChildNode :: ExeTree so cs ti to
    } ->
    ExeTree si so ti to
  XTTHook ::
    { loc :: Loc,
      thHook :: si -> ti -> IO to,
      thHookRelease :: to -> IO (),
      thChildNode :: ExeTree si so to tc
    } ->
    ExeTree si so ti to
  XTGroup ::
    { leafloc :: Loc,
      nStatus :: TVar Status,
      childNodes :: TQueue (ExeTree si so ti to),
      fullyRunning :: TQueue (ExeTree si so ti to)
    } ->
    ExeTree si () ti ()
  XTFix ::
    { leafloc :: Loc,
      nStatus :: TVar Status,
      -- in the next layer out these will default to
      -- a IO const funtion that logs Empty which can be filtered
      -- out of the log
      fxOHookStatus :: TVar Status,
      fxOHookVal :: TMVar (Either Abandon so2),
      fxOHook :: si -> IO so2,
      fxOHookRelease :: so2 -> IO (),
      fxTHook :: so2 -> ti -> IO to2,
      fxTHookRelease :: to2 -> IO (),
      tHook :: so2 -> to2 -> IO io,
      tHookRelease :: io -> IO (),
      iterations :: TQueue (Test so2 to2 io),
      runningCount :: TVar Int
    } ->
    ExeTree si () ti ()

data Test si ti ii = Test
  { tstLoc :: Loc,
    tst :: si -> ti -> ii -> IO ()
  }

prepare :: PN.PreNode () () () () -> IO (ExeTree () () () ())
prepare =
  prepare' Root 0
  where
    consNoMxIdx :: IdxLst a -> a -> IdxLst a
    consNoMxIdx l@IdxLst {lst} i = l {lst = i : lst}

    prepare' :: Loc -> Int -> PN.PreNode o oo t to -> IO (ExeTree o oo t to)
    prepare' parentLoc subElmIdx pn = do
      let nodeLoc elmType mtag =
            Node
              { parent = parentLoc,
                tag = fromMaybe "" mtag
              }
      ns <- newTVarIO Pending
      case pn of
        PN.Branch
          { bTag,
            subElms
          } -> do
            let loc = nodeLoc "Branch" bTag
            idx <- newTVarIO 0
            fr <- newTQueueIO
            q <- newTQueueIO
            -- load the queue
            c <- traverse (prepare' loc 0) subElms
            atomically $ traverse_ (writeTQueue q) c
            pure $
              XTGroup
                { leafloc = loc,
                  nStatus = ns,
                  childNodes = q,
                  fullyRunning = fr
                }
        PN.OnceHook
          { hookTag,
            hook,
            hookChild,
            hookRelease
          } -> do
            s <- newTVarIO Pending
            v <- newEmptyTMVarIO
            let loc = nodeLoc "SingletonHook" hookTag
            child <- prepare' loc 0 hookChild
            pure $
              XTSHook
                { loc,
                  status = s,
                  sHook = hook loc,
                  sHookRelease = hookRelease loc,
                  sHookVal = v,
                  sChildNode = child
                }
        PN.ThreadHook
          { threadTag,
            threadHook,
            threadHookChild,
            threadHookRelease
          } ->
            let loc = nodeLoc "ThreadHook" threadTag
             in XTTHook loc (threadHook loc) (threadHookRelease loc)
                  <$> prepare' loc 0 threadHookChild
        PN.Fixture
          { onceFxHook,
            onceFxHookRelease,
            threadFxHook,
            threadFxHookRelease,
            testHook,
            testHookRelease,
            fxTag,
            iterations
          } ->
            do
              let loc = nodeLoc "Fixture" fxTag
                  converTest PN.Test {tstId, tst} = Test (Node loc tstId) tst
              s <- newTVarIO Pending
              ohs <- newTVarIO Pending
              q <- newTQueueIO
              v <- newEmptyTMVarIO
              runningCount <- newTVarIO 0
              atomically $ traverse_ (writeTQueue q . converTest) iterations
              pure $
                XTFix
                  { leafloc = loc,
                    nStatus = ns,
                    iterations = q,
                    runningCount,
                    fxOHookStatus = ohs,
                    fxOHookVal = v,
                    fxOHook = onceFxHook loc,
                    fxOHookRelease = onceFxHookRelease loc,
                    fxTHook = threadFxHook loc,
                    fxTHookRelease = threadFxHookRelease loc,
                    tHook = testHook loc,
                    tHookRelease = testHookRelease loc
                  }

logAbandonned :: Logger -> Loc -> Abandon -> IO ()
logAbandonned logger loc Abandon {sourceLoc, sourceEventType, exception} =
  logger (mkParentFailure sourceLoc loc sourceEventType exception)

logFailure :: Logger -> Loc -> ExeEventType -> SomeException -> IO ()
logFailure logger loc et e = logger (mkFailure loc (txt et <> "Failed at: " <> txt loc) e)

readOrLockHook :: TVar Status -> TMVar (Either Abandon ho) -> STM (Maybe (Either Abandon ho))
readOrLockHook hs hVal = do
  s <- readTVar hs
  s
    == Pending
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

runLogHook :: forall hi ho. ExeEventType -> Logger -> (hi -> IO ho) -> hi -> Loc -> IO (Either Abandon ho)
runLogHook hkEvent logger hook hi loc =
  withStartEnd logger loc hkEvent $
    catchAll
      ( Right <$> hook hi
      )
      ( \e ->
          logFailure logger loc hkEvent e
            >> pure (Left $ Abandon loc hkEvent e)
      )

normalHookVal :: forall hi ho. ExeEventType -> Logger -> (hi -> IO ho) -> hi -> TVar Status -> TMVar (Either Abandon ho) -> Loc -> IO (Either Abandon ho)
normalHookVal hkEvent logger hook hi hs hkVal loc =
  runOrReturn
    hs
    hkVal
    ( runLogHook hkEvent logger hook hi loc
        >>= atomically . setRunningVal hs hkVal
    )

withStartEnd :: Logger -> Loc -> ExeEventType -> IO a -> IO a
withStartEnd logger loc evt io = do
  logger $ Start loc evt
  finally io . logger $ End loc evt

abandonLogHook :: ExeEventType -> Logger -> Abandon -> Loc -> IO (Either Abandon a)
abandonLogHook hkEvent logger abandon loc =
  do
    withStartEnd logger loc hkEvent $
      logAbandonned logger loc abandon
    pure $ Left abandon

abandonnedHookVal :: forall ho. ExeEventType -> Logger -> Abandon -> TVar Status -> TMVar (Either Abandon ho) -> Loc -> IO (Either Abandon ho)
abandonnedHookVal hkEvent logger abandon hs hkVal loc =
  runOrReturn
    hs
    hkVal
    ( abandonLogHook hkEvent logger abandon loc
        >>= atomically . setRunningVal hs hkVal
    )

threadHookVal :: forall so hi ho. Logger -> Either Abandon (ExeIn so hi) -> ExeEventType -> (so -> hi -> IO ho) -> Loc -> IO (Either Abandon ho)
threadHookVal logger hkIn hkEvent hook loc =
  hkIn
    & either
      (\abandon -> abandonLogHook hkEvent logger abandon loc)
      (\(ExeIn si ti) -> runLogHook hkEvent logger (hook si) ti loc)

onceHookVal :: forall hi ho. Logger -> ExeEventType -> Either Abandon hi -> (hi -> IO ho) -> TVar Status -> TMVar (Either Abandon ho) -> Loc -> IO (Either Abandon ho)
onceHookVal logger hkEvent ehi hook hs hkVal loc =
  either
    (\abandon -> abandonnedHookVal hkEvent logger abandon hs hkVal loc)
    (\hi' -> normalHookVal hkEvent logger hook hi' hs hkVal loc)
    ehi

setStatusRunning :: TVar Status -> STM Bool
setStatusRunning status =
  do
    s <- readTVar status
    let change = s < Running
    when change $
      writeTVar status Running
    pure change

releaseHook :: Logger -> ExeEventType -> Either Abandon ho -> Loc -> (ho -> IO ()) -> IO ()
releaseHook logger evt eho loc hkRelease =
  withStartEnd logger loc evt $
    eho
      & either
        (logAbandonned logger loc)
        ( \so ->
            catchAll
              (hkRelease so)
              (logger . mkFailure loc ("Hook Release Failed: " <> txt evt <> " " <> txt loc))
        )

releaseHookUpdateStatus :: Logger -> ExeEventType -> TVar Status -> Either Abandon ho -> Loc -> (ho -> IO ()) -> IO ()
releaseHookUpdateStatus logger evt ns eho loc hkRelease =
  finally
    (releaseHook logger evt eho loc hkRelease)
    (atomically $ writeTVar ns Done)

discardDone :: TQueue (ExeTree a b c d) -> STM ()
discardDone = processWhile nodeDone

processWhile :: forall a b c d. (ExeTree a b c d -> STM Bool) -> TQueue (ExeTree a b c d) -> STM ()
processWhile p q =
  tryPeekTQueue q
    >>= maybe
      (pure ())
      (p >=> bool (pure ()) (readTQueue q >> processWhile p q))

discardDoneMoveFullyRunning :: TQueue (ExeTree si so ti to) -> TQueue (ExeTree si so ti to) -> STM ()
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
  { sourceLoc :: Loc,
    sourceEventType :: ExeEventType,
    exception :: SomeException
  }
  deriving (Show)

data ExeIn si ti = ExeIn
  { singletonIn :: si,
    threadIn :: ti
  }

executeNode :: forall si so ti to. Logger -> Either Abandon (ExeIn si ti) -> ExeTree si so ti to -> IO ()
executeNode logger hkIn rg =
  do
    wantRun <- atomically $ canRun rg
    when
      wantRun
      case rg of
        XTSHook
          { loc,
            status,
            sHook,
            sHookRelease,
            sHookVal,
            sChildNode
          } ->
            do
              -- onceHookVal:
              --  1. runs hook if required
              --  2. waits if hook is running
              --  3. updates hook status to running
              --  4. returns hook result
              -- must run for logging even if hkIn is Left
              eso <- onceHookVal logger L.OnceHook siHkIn sHook status sHookVal loc
              let nxtHkIn so = (\exi -> exi {singletonIn = so}) <$> hkIn
                  -- nxtAbandon' = nxtAbandon loc L.OnceHook
                  recurse a = exeNxt a sChildNode
               in finally
                    ( either
                        (recurse . Left)
                        (recurse . nxtHkIn)
                        eso
                    )
                    ( do
                        wantRelease <- atomically $ do
                          cs <- getStatus sChildNode
                          s <- readTVar status
                          pure $ cs == Done && s < HookFinalising
                        when wantRelease $
                          releaseHookUpdateStatus logger L.OnceHook status eso loc sHookRelease
                    )
        XTTHook
          { loc,
            thHook,
            thHookRelease,
            thChildNode
          } ->
            do
              eto <- threadHookVal logger hkIn L.ThreadHook thHook loc
              let nxtHkIn ti = (\exi -> exi {threadIn = ti}) <$> hkIn
                  recurse a = exeNxt a thChildNode
              finally
                ( either
                    (recurse . Left)
                    (recurse . nxtHkIn)
                    eto
                )
                (releaseHook logger L.ThreadHookRelease eto loc thHookRelease)
        self@XTGroup
          { leafloc,
            nStatus,
            childNodes,
            fullyRunning
          } ->
            withStartEnd logger leafloc L.Group $
              do
                whenLeft hkIn (logAbandonned' leafloc)
                recurse
            where
              -- when the last thread finishes child nodes and fully running will be done
              recurse =
                atomically nxtChild
                  >>= maybe
                    (atomically updateStatusFromQs)
                    (\c -> exeNxt hkIn c >> recurse)

              updateStatusFromQs :: STM ()
              updateStatusFromQs = do
                s <- readTVar nStatus
                isDone s
                  ? pure ()
                  $ do
                    discardDone fullyRunning
                    discardDoneMoveFullyRunning fullyRunning childNodes
                    mtChilds <- isEmptyTQueue childNodes
                    mtFullyRunning <- isEmptyTQueue fullyRunning
                    let completed = mtChilds && mtFullyRunning
                    if
                        | completed -> writeTVar nStatus Done
                        | mtChilds -> writeTVar nStatus FullyRunning
                        | otherwise -> pure ()

              nxtChild =
                tryReadTQueue childNodes
                  >>= maybe
                    (pure Nothing)
                    ( \cld ->
                        let ioCld = pure $ Just cld
                            qChld = writeTQueue childNodes cld
                            qChldReturn = qChld >> ioCld
                            qFullyRunning = writeTQueue fullyRunning cld >> nxtChild
                         in getStatus cld
                              >>= \case
                                Pending ->
                                  setStatusRunning nStatus >> qChldReturn
                                HookExecuting ->
                                  -- just plonk on the end of the q and go around again
                                  -- may pick up same node if there is only one node running but that is ok
                                  -- it just turns into a wait
                                  qChld >> nxtChild
                                Running -> qChldReturn
                                FullyRunning -> qFullyRunning
                                HookFinalising -> qFullyRunning
                                -- discard if done
                                Done -> nxtChild
                    )
        fx@XTFix
          { nStatus,
            leafloc,
            iterations,
            runningCount,
            fxOHookStatus,
            fxOHookVal,
            fxOHook,
            fxOHookRelease,
            fxTHook,
            fxTHookRelease,
            tHook,
            tHookRelease
          } ->
            do
              eso <- onceHookVal logger L.FixtureOnceHook siHkIn fxOHook fxOHookStatus fxOHookVal leafloc
              fxIn <- threadHookVal logger (liftA2 ExeIn eso $ threadIn <$> hkIn) L.FixtureThreadHook fxTHook leafloc
              let fxIpts = liftA2 ExeIn eso fxIn
              withStartEnd logger leafloc L.Fixture $ do
                whenLeft fxIn (logAbandonned' leafloc)
                recurse fxIpts
            where
              recurse fxIpts = do
                etest <- atomically $ do
                  mtest <- tryReadTQueue iterations
                  mtest
                    & maybe
                      ( do
                          r <- readTVar runningCount
                          Left
                            <$> if
                                | r < 0 -> error "framework error - this should not happen - running count below zero"
                                | r == 0 -> pure True
                                | otherwise -> writeTVar nStatus FullyRunning >> pure False
                      )
                      ( \t -> do
                          modifyTVar runningCount succ
                          setStatusRunning nStatus
                          pure $ Right t
                      )
                etest
                  & either
                    ( \done -> do
                        when done $
                          atomically (writeTVar nStatus Done)
                        releaseHook logger L.FixtureThreadHookRelease (threadIn <$> fxIpts) leafloc fxTHookRelease
                        when done $
                          releaseHookUpdateStatus logger L.FixtureOnceHookRelease fxOHookStatus (singletonIn <$> fxIpts) leafloc fxOHookRelease
                    )
                    ( \(Test tstLoc test) -> do
                        io <- runTestHook fxIpts tHook
                        let unpak io' (ExeIn so2 to2) = (so2, to2, io')
                            ethInputs = liftA2 unpak io fxIpts

                        finally
                          ( withStartEnd logger tstLoc L.Test $
                              ethInputs & either
                                (logAbandonned' tstLoc)
                                \(so2, to2, io') ->
                                  catchAll
                                    (test so2 to2 io')
                                    (logFailure' tstLoc L.Test)
                          )
                          $ atomically (modifyTVar runningCount pred) >> recurse fxIpts
                    )

              runTestHook :: forall io' so' to'. Either Abandon (ExeIn so' to') -> (so' -> to' -> IO io') -> IO (Either Abandon io')
              runTestHook fxIpts testHk =
                do
                  withStartEnd logger leafloc L.TestHook $
                    either
                      logReturnAbandonned
                      runHook
                      fxIpts
                where
                  logReturnAbandonned a =
                    logAbandonned logger leafloc a
                      >> pure (Left a)

                  runHook (ExeIn si ti) =
                    catchAll
                      (Right <$> testHk si ti)
                      ( \e -> do
                          logFailure logger leafloc L.TestHook e
                          pure . Left $ Abandon leafloc L.TestHook e
                      )
  where
    siHkIn :: Either Abandon si
    siHkIn = singletonIn <$> hkIn

    exeNxt :: forall si' so' ti' to'. Either Abandon (ExeIn si' ti') -> ExeTree si' so' ti' to' -> IO ()
    exeNxt = executeNode logger

    nxtAbandon :: Loc -> ExeEventType -> SomeException -> Abandon
    nxtAbandon loc' et e = fromLeft (Abandon loc' et e) hkIn

    logAbandonned' :: Loc -> Abandon -> IO ()
    logAbandonned' = logAbandonned logger

    logFailure' :: Loc -> ExeEventType -> SomeException -> IO ()
    logFailure' = logFailure logger

type Logger = (Int -> Text -> ExeEvent) -> IO ()

newLogger :: EventSink -> ThreadId -> IO Logger
newLogger sink tid =
  do
    ir <- UnliftIO.newIORef 0
    pure $ mkLogger sink ir tid

executeGraph :: EventSink -> ExeTree () () () () -> Int -> IO ()
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
    { sink,
      logWorker,
      stopWorker
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