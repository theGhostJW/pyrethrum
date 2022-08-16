module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import DSL.Logger (logMessage)
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty, replicateM)
import Data.Tuple.Extra (both)
import Extra (uncurry3)
import GHC.Exts
import Internal.PreNode as PN
  ( PreNode (..),
    PreNodeRoot,
    Test (..),
    rootNode,
  )
import Internal.RunTimeLogging as L
  ( ExeEvent (..),
    ExeEventType (..),
    Index (Index),
    Loc (..),
    LogControls (LogControls),
    PThreadId,
    Sink,
    logWorker,
    mkFailure,
    mkLogger,
    mkParentFailure,
    sink,
    stopWorker,
  )
import qualified Internal.RunTimeLogging as L (ExeEventType (OnceHook, OnceHookRelease, TestHook, ThreadHook, ThreadHookRelease))
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
import qualified Prelude as PRL

data Status
  = Pending
  | HookExecuting -- only relevant to hooks
  | Running
  | FullyRunning
  | HookFinalising -- only relevant to hooks
  | Done
  deriving (Show, Eq, Ord)

getStatus :: ExeTree si so ti to ii io -> STM Status
getStatus =
  readTVar . getStatusTVar
  where
    getStatusTVar :: ExeTree si so ti to ii io -> TVar Status
    getStatusTVar = \case
      RTNodeS {status} -> status
      RTNodeT {tChildNode} -> getStatusTVar tChildNode
      RTNodeI {iChildNode} -> getStatusTVar iChildNode
      RTGroup {nStatus} -> nStatus
      RTFix {nStatus} -> nStatus

isDone :: Status -> Bool
isDone = (== Done)

nodeDone :: ExeTree si so ti to ii io -> STM Bool
nodeDone t = (== Done) <$> getStatus t

canRun :: ExeTree si so ti to ii io -> STM Bool
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

modifyStatus :: (Status -> Status) -> ExeTree si so ti to ii io -> STM ()
modifyStatus f et =
  case et of
    RTNodeS {status} -> update status
    RTNodeT {} -> pure ()
    RTNodeI {} -> pure ()
    RTGroup {nStatus} -> update nStatus
    RTFix {nStatus} -> update nStatus
  where
    update = flip modifyTVar f

waitDone :: ExeTree si so ti to ii io -> STM ()
waitDone rg = do
  s <- getStatus rg
  unless (s == Done) retry

data IdxLst a = IdxLst
  { maxIndex :: Int,
    lst :: [a],
    currIdx :: TVar Int
  }

mkIdxLst :: a -> STM (IdxLst a)
mkIdxLst elm = IdxLst 0 [elm] <$> newTVar 0

data ExeTree si so ti to ii io where
  RTNodeS ::
    { loc :: Loc,
      status :: TVar Status,
      sHook :: si -> IO so,
      sHookRelease :: so -> IO (),
      sHookVal :: TMVar (Either SomeException so),
      sChildNode :: ExeTree so cs ti to ii io
    } ->
    ExeTree si so ti to ii io
  RTNodeT ::
    { loc :: Loc,
      tHook :: si -> ti -> IO to,
      tHookRelease :: to -> IO (),
      tChildNode :: ExeTree si so to tc ii io
    } ->
    ExeTree si so ti to ii io
  RTNodeI ::
    { loc :: Loc,
      iHook :: si -> ti -> ii -> IO io,
      iHookRelease :: io -> IO (),
      iChildNode :: ExeTree si so ti to io iic
    } ->
    ExeTree si so ti to ii io
  RTGroup ::
    { leafloc :: Loc,
      nStatus :: TVar Status,
      childNodes :: TQueue (ExeTree si so ti to ii io),
      fullyRunning :: TQueue (ExeTree si so ti to ii io)
    } ->
    ExeTree si () ti () ii ()
  RTFix ::
    { leafloc :: Loc,
      logStart :: IO (),
      nStatus :: TVar Status,
      iterations :: TQueue (Test si ti ii),
      runningCount :: TVar Int
    } ->
    ExeTree si () ti () ii ()

prepare :: PreNode () () () () () () -> IO (ExeTree () () () () () ())
prepare =
  prepare' Root 0
  where
    consNoMxIdx :: IdxLst a -> a -> IdxLst a
    consNoMxIdx l@IdxLst {lst} i = l {lst = i : lst}

    mkLoc :: Loc -> Int -> Maybe Text -> Text -> Loc
    mkLoc parentLoc subElmIdx childlabel elmType =
      Node parentLoc $
        childlabel
          & maybe
            (elmType <> "[" <> txt subElmIdx <> "]")
            P.id

    prepare' :: Loc -> Int -> PreNode o oo t to i io -> IO (ExeTree o oo t to i io)
    prepare' parentLoc subElmIdx pn = do
      let mkLoc' = mkLoc parentLoc subElmIdx
      ns <- newTVarIO Pending
      case pn of
        Branch
          { bTag,
            subElms
          } -> do
            let loc = mkLoc' bTag "Branch"
            idx <- newTVarIO 0
            fr <- newTQueueIO
            q <- newTQueueIO
            -- load the queue
            c <- traverse (prepare' loc 0) subElms
            atomically $ traverse_ (writeTQueue q) c
            pure $
              RTGroup
                { leafloc = loc,
                  nStatus = ns,
                  childNodes = q,
                  fullyRunning = fr
                }
        PN.OnceHook
          { hookTag,
            hook,
            hookChild,
            hookResult,
            hookRelease
          } -> do
            s <- newTVarIO Pending
            v <- newEmptyTMVarIO
            let loc = mkLoc' hookTag "SingletonHook"
            child <- prepare' loc 0 hookChild
            pure $
              RTNodeS
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
            let loc = mkLoc' threadTag "ThreadHook"
             in RTNodeT loc (threadHook loc) (threadHookRelease loc)
                  <$> prepare' loc 0 threadHookChild
        PN.TestHook
          { testTag,
            testHook,
            testHookChild,
            testHookRelease
          } ->
            let loc = mkLoc' testTag "TestHook"
             in RTNodeI loc (testHook loc) (testHookRelease loc)
                  <$> prepare' loc 0 testHookChild
        PN.Fixture
          { fxTag,
            logStart,
            iterations,
            logEnd
          } ->
            do
              let loc = mkLoc' fxTag "ThreadHook"
              s <- newTVarIO Pending
              q <- newTQueueIO
              runningCount <- newTVarIO 0
              atomically $ traverse_ (writeTQueue q) iterations
              pure $
                RTFix
                  { leafloc = loc,
                    logStart = logStart loc,
                    nStatus = ns,
                    iterations = q,
                    runningCount
                  }

logAbandonned :: Logger -> Loc -> Abandon -> IO ()
logAbandonned lgr loc Abandon {sourceLoc, exception} = lgr (mkParentFailure sourceLoc loc exception)

logFailure :: Logger -> Loc -> ExeEventType -> SomeException -> IO ()
logFailure lgr loc et e = lgr (mkFailure loc (txt et <> "Failed at: " txt loc) e)

readOrLockHook :: TVar Status -> TMVar (Either SomeException ho) -> STM (Maybe (Either SomeException ho))
readOrLockHook hs hVal = do
  s <- readTVar hs
  s == Pending
    ? (writeTVar hs HookExecuting >> pure Nothing)
    $ (Just <$> readTMVar hVal)

setHookValStatus :: TVar Status -> TMVar (Either SomeException ho) -> Either SomeException ho -> STM (Either SomeException ho)
setHookValStatus hs hVal eso = do
  putTMVar hVal eso
  -- success or failure always running will be set done by closing hook
  writeTVar hs Running
  pure eso

runOrReturn :: TVar Status -> TMVar (Either SomeException ho) -> IO (Either SomeException ho) -> IO (Either SomeException ho)
runOrReturn hkStatus hkVal hkAction =
  atomically (readOrLockHook hkStatus hkVal)
    >>= maybe
      hkAction
      pure

runLogHook :: forall hi ho. ExeEventType -> Logger -> (hi -> IO ho) -> hi -> Loc -> IO (Either SomeException ho)
runLogHook hkEvent logger hook hi loc =
  withStartEnd logger loc hkEvent $
    catchAll
      ( Right <$> hook hi
      )
      ( \e ->
          logFailure logger loc hkEvent e
            >> pure (Left e)
      )

normalHookVal :: forall hi ho. ExeEventType -> Logger -> (hi -> IO ho) -> hi -> TVar Status -> TMVar (Either SomeException ho) -> Loc -> IO (Either SomeException ho)
normalHookVal hkEvent logger hook hi hs hkVal loc =
  runOrReturn
    hs
    hkVal
    ( runLogHook hkEvent logger hook hi loc
        >>= atomically . setHookValStatus hs hkVal
    )

withStartEnd :: Logger -> Loc -> ExeEventType -> IO a -> IO a
withStartEnd lgr loc evt io = do
  lgr $ Start loc evt
  finally io $ lgr $ End loc evt

abandonLogHook :: ExeEventType -> Logger -> Abandon -> Loc -> IO (Either SomeException a)
abandonLogHook hkEvent logger abandon@Abandon {exception} loc =
  do
    withStartEnd logger loc hkEvent $
      logAbandonned logger loc abandon
    pure $ Left exception

abandonnedHookVal :: forall ho. ExeEventType -> Logger -> Abandon -> TVar Status -> TMVar (Either SomeException ho) -> Loc -> IO (Either SomeException ho)
abandonnedHookVal hkEvent logger abandon hs hkVal loc =
  runOrReturn
    hs
    hkVal
    ( abandonLogHook hkEvent logger abandon loc
        >>= atomically . setHookValStatus hs hkVal
    )

threadHookVal :: forall hi ho. Logger -> ExeEventType -> Either Abandon (hi, hi -> IO ho) -> Loc -> IO (Either SomeException ho)
threadHookVal logger hkEvent ehi loc =
  either
    (\abandon -> abandonLogHook hkEvent logger abandon loc)
    (\(hi', hook) -> runLogHook hkEvent logger hook hi' loc)
    ehi

singletonHookVal :: forall hi ho. Logger -> ExeEventType -> Either Abandon (hi, hi -> IO ho) -> TVar Status -> TMVar (Either SomeException ho) -> Loc -> IO (Either SomeException ho)
singletonHookVal logger hkEvent ehi hs hkVal loc =
  either
    (\abandon -> abandonnedHookVal hkEvent logger abandon hs hkVal loc)
    (\(hi', hook) -> normalHookVal hkEvent logger hook hi' hs hkVal loc)
    ehi

data TestHkSrc hi = TestHkSrc
  { tstHkLoc :: Loc,
    tstHkHook :: IO (Either Abandon hi),
    tstHkRelease :: hi -> IO ()
  }

runTestHook :: Logger -> Either Abandon (ExeIn si ti) -> TestHkSrc hi -> (si -> ti -> hi -> IO io) -> IO (Either Abandon io)
runTestHook
  logger
  aExIn
  TestHkSrc
    { tstHkLoc,
      tstHkHook,
      tstHkRelease
    }
  testHk =
    do
      ehki <- tstHkHook
      withStartEnd logger tstHkLoc L.TestHook $
        either
          logReturnAbandonned
          (uncurry3 runHook)
          (inputs ehki)
    where
      logReturnAbandonned a =
        logAbandonned logger tstHkLoc a
          >> pure (Left a)

      runHook si ti ii =
        catchAll
          (Right <$> testHk si ti ii)
          ( \e -> do
              logFailure logger tstHkLoc L.TestHook e
              pure . Left $ Abandon tstHkLoc e
          )

      inputs ehki = do
        ii <- ehki
        (ExeIn si ti) <- aExIn
        pure (si, ti, ii)

runIfCan :: TVar Status -> IO () -> IO ()
runIfCan s action =
  atomically (readTVar s)
    >>= bool (pure ()) action . (< FullyRunning)

setStatusRunning :: TVar Status -> STM ()
setStatusRunning status = modifyTVar status \s -> s < Running ? Running $ s

releaseHook :: Logger -> ExeEventType -> Either Abandon ho -> Loc -> (ho -> IO ()) -> IO ()
releaseHook lgr evt eho loc hkRelease =
  withStartEnd lgr loc evt $
    eho
      & either
        (logAbandonned lgr loc)
        ( \so ->
            catchAll
              (hkRelease so)
              (lgr . mkFailure loc ("Hook Release Failed: " <> txt evt <> " " <> txt loc))
        )

releaseHookUpdateStatus :: Logger -> ExeEventType -> TVar Status -> Either Abandon ho -> Loc -> (ho -> IO ()) -> IO ()
releaseHookUpdateStatus lgr evt ns eho loc hkRelease =
  finally
    (releaseHook lgr evt eho loc hkRelease)
    (atomically $ writeTVar ns Done)

discardDone :: TQueue (ExeTree a b c d e f) -> STM ()
discardDone = processWhile nodeDone

processWhile :: forall a b c d e f. (ExeTree a b c d e f -> STM Bool) -> TQueue (ExeTree a b c d e f) -> STM ()
processWhile p q =
  tryPeekTQueue q
    >>= maybe
      (pure ())
      (\n -> p n >>= bool (pure ()) (readTQueue q >> processWhile p q))

discardDoneMoveFullyRunning :: TQueue (ExeTree si so ti to ii io) -> TQueue (ExeTree si so ti to ii io) -> STM ()
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
    exception :: SomeException
  }
  deriving (Show)

data ExeIn si ti = ExeIn
  { singletonIn :: si,
    threadIn :: ti
  }

-- TODO SCENARIO: T1 - Pending; T2 - Running
executeNode :: forall si so ti to ii io. Logger -> Either Abandon (ExeIn si ti) -> TestHkSrc ii -> ExeTree si so ti to ii io -> IO ()
executeNode logger hkIn tstHk rg =
  do
    let exeNxt :: forall si' so' ti' to' ii' io'. Either Abandon (ExeIn si' ti') -> TestHkSrc ii' -> ExeTree si' so' ti' to' ii' io' -> IO ()
        exeNxt = executeNode logger

        nxtAbandon :: Loc -> SomeException -> Abandon
        nxtAbandon loc' e = either id (const $ Abandon loc' e) hkIn

    wantRun <- atomically $ canRun rg
    when
      wantRun
      case rg of
        RTNodeS
          { loc,
            status,
            sHook,
            sHookRelease,
            sHookVal,
            sChildNode
          } ->
            do
              -- singletonHookVal:
              --  1. runs hook if required
              --  2. waits if hook is running
              --  3. updates hook status to running
              --  4. returns hook result
              -- must run for logging even if hkIn is Left
              let siIn = (,sHook) . singletonIn <$> hkIn
              eso <- singletonHookVal logger L.OnceHook siIn status sHookVal loc
              let nxtHkIn so = (\exi -> exi {singletonIn = so}) <$> hkIn
                  nxtAbandon' = nxtAbandon loc
                  recurse a = exeNxt a tstHk sChildNode
               in finally
                    ( either
                        (recurse . Left . nxtAbandon')
                        (recurse . nxtHkIn)
                        eso
                    )
                    ( do
                        wantRelease <- atomically $ do
                          cs <- getStatus sChildNode
                          s <- readTVar status
                          pure $ cs == Done && s < HookFinalising
                        when wantRelease $
                          releaseHookUpdateStatus logger L.OnceHook status (mapLeft nxtAbandon' eso) loc sHookRelease
                    )
        RTNodeT
          { loc,
            tHook,
            tHookRelease,
            tChildNode
          } ->
            do
              let tiIn = (\(ExeIn si ti) -> (ti, tHook si)) <$> hkIn
              eto <- threadHookVal logger L.ThreadHook tiIn loc
              let nxtHkIn ti = (\exi -> exi {threadIn = ti}) <$> hkIn
                  nxtAbandon' = nxtAbandon loc
                  recurse a = exeNxt a tstHk tChildNode
              finally
                ( either
                    (recurse . Left . nxtAbandon')
                    (recurse . nxtHkIn)
                    eto
                )
                (releaseHook logger L.OnceHook (mapLeft nxtAbandon' eto) loc tHookRelease)
        ihk@RTNodeI
          { loc,
            iHook,
            iHookRelease,
            iChildNode
          } ->
            let newTstHk =
                  TestHkSrc
                    { tstHkLoc = loc,
                      tstHkHook = runTestHook logger hkIn tstHk iHook,
                      tstHkRelease =
                        withStartEnd logger loc TestHookRelease
                          . handleAll (logFailure logger loc L.TestHookRelease)
                          . iHookRelease
                    }
             in exeNxt hkIn newTstHk iChildNode
        self@RTGroup
          { leafloc,
            nStatus,
            childNodes,
            fullyRunning
          } ->
            recurse
            where
              -- when the last thread finishes child nodes and fully running will be done
              recurse =
                atomically nxtChildStarted
                  >>= maybe
                    ( atomically updateStatusFromQs
                        >>= flip when (logger $ End leafloc Group)
                    )
                    ( \(c, wasStarted) -> do
                        when wasStarted $ do
                          logger (Start leafloc Group)
                          whenLeft
                            hkIn
                            (logAbandonned logger leafloc)

                        exeNxt hkIn tstHk c
                        recurse
                    )

              updateStatusFromQs :: STM Bool
              updateStatusFromQs = do
                s <- readTVar nStatus
                isDone s
                  ? pure False
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
                    pure completed

              nxtChildStarted = atomically $
                do
                  s <- readTVar nStatus
                  ((,s == Pending) <$>) <$> nxtChild

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
        fx@RTFix
          { nStatus,
            iterations,
            runningCount
          } ->
            runIfCan nStatus recurse
            where
              recurse :: IO ()
              recurse = do
                mtest <-
                  atomically $
                    tryReadTQueue iterations
                      >>= maybe
                        ( do
                            r <- readTVar runningCount
                            (r < 1)
                              ? writeTVar nStatus Done
                              $ writeTVar nStatus FullyRunning
                            pure Nothing
                        )
                        ( \i -> do
                            modifyTVar runningCount succ
                            setStatusRunning nStatus
                            pure $ Just i
                        )
                whenJust mtest $
                  \i ->
                    do
                      ethti <- ioti
                      let tst :: ii -> IO ()
                          tst =
                            ethti
                              & either
                                (const . const $ pure ()) -- TODO log parent hook failure
                                (action i si)
                      finally
                        ( catchAll
                            (void $ runTestHook tstHk tst)
                            (print . displayException) -- TODO log parent iteration failure
                        )
                        (atomically (modifyTVar runningCount pred) >> recurse)

type Logger = (Index -> PThreadId -> ExeEvent) -> IO ()

newLogger :: Sink -> IO Logger
newLogger sink = do
  r <- UnliftIO.newIORef (Index 0)
  pure $ mkLogger sink r

executeGraph :: Sink -> ExeTree () () () () () () -> Int -> IO ()
executeGraph sink xtri maxThreads = do
  logger <- newLogger sink
  logger StartExecution
  finally
    ( replicateConcurrently_
        maxThreads
        do
          logger' <- newLogger sink
          executeNode
            logger'
            ()
            (pure $ Right ())
            TestHkSrc
              { tstHkLoc = Loc "Root",
                tstHkHook = pure $ Right (),
                tstHkRelease = const $ pure ()
              }
            xtri
    )
    (logger EndExecution)

execute :: Int -> LogControls -> PreNodeRoot -> IO ()
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
              rn <- rootNode preRoot
              exeTree <- prepare rn
              executeGraph sink exeTree maxThreads
          )
          stopWorker

-- modifyStatusRecursively :: (Status -> Status) -> ExeTree si so ti to ii io -> STM ()
-- modifyStatusRecursively f n =
--   modifyStatus f n
--     >> case n of
--       RTNodeS {loc, sChildNode} -> recurse sChildNode
--       RTNodeT {loc, tChildNode} -> recurse tChildNode
--       RTNodeI {loc, iChildNode} -> recurse iChildNode
--       RTGroup {leafloc, childNodes, fullyRunning} -> abandonQ childNodes
--       RTFix {leafloc, iterations} -> pure ()
--   where
--     recurse :: ExeTree si' so' ti' to' ii' io' -> STM ()
--     recurse = modifyStatusRecursively f

--     abandonQ :: TQueue (ExeTree si' so' ti' to' ii' io') -> STM ()
--     abandonQ q =
--       tryReadTQueue q
--         >>= maybe
--           (pure ())
--           \rg -> recurse rg >> abandonQ q

-- logAbandonRecursively :: forall si so ti to ii io. Logger -> Loc -> SomeException -> (IO () -> IO ()) -> ExeTree si so ti to ii io -> IO ()
-- logAbandonRecursively logger parentLoc e iLogger = \case
--   n@RTNodeS {loc, sChildNode} -> hkRecurse loc n sChildNode L.OnceHook OnceHookRelease iLogger
--   n@RTNodeT {loc, tChildNode} -> hkRecurse loc n tChildNode L.ThreadHook ThreadHookRelease iLogger
--   n@RTNodeI {loc, iChildNode} ->
--     let nxtIlogger = wrapAction loc L.TestHook TestHookRelease
--      in logAbandonRecursively logger parentLoc e nxtIlogger iChildNode
--   RTGroup {leafloc, childNodes, nStatus} ->
--     let abandonChildren = abandonQ childNodes iLogger
--      in isAbandonned nStatus
--           >>= bool
--             abandonChildren
--             (logAbandonnedWithAction abandonChildren leafloc Group)
--   RTFix {leafloc, iterations, abandonning, nStatus} -> do
--     finally
--       ( do
--           atomically $ writeTVar abandonning True
--       )
--       (atomically $ writeTVar abandonning False)
--   where
--     abandonned :: STM Status -> IO Bool
--     abandonned s = atomically $ (== Abandon) <$> s

--     isAbandonned :: TVar Status -> IO Bool
--     isAbandonned = abandonned . readTVar

--     nodeAbandonned :: ExeTree si' so' ti' to' ii' io' -> IO Bool
--     nodeAbandonned = abandonned . getStatus

--     mkAbaondonLogEntry :: Loc -> IO ()
--     mkAbaondonLogEntry loc = logger (mkParentFailure parentLoc loc e)

--     logAbandonnedWithAction :: IO () -> Loc -> ExeEventType -> IO ()
--     logAbandonnedWithAction action loc ev = do
--       logger $ Start loc ev
--       mkAbaondonLogEntry loc
--       finally
--         action
--         (logger $ End loc ev)

--     logAbandonned :: Loc -> ExeEventType -> IO ()
--     logAbandonned = logAbandonnedWithAction (pure ())

--     wrapAction :: Loc -> ExeEventType -> ExeEventType -> IO () -> IO ()
--     wrapAction loc hkEvnt releaseHkEvnt childAction =
--       finally
--         (logAbandonned loc hkEvnt >> childAction)
--         $ logAbandonned loc releaseHkEvnt

--     hkRecurse :: Loc -> ExeTree si' so' ti' to' ii' io' -> ExeTree si'' so'' ti'' to'' ii'' io'' -> ExeEventType -> ExeEventType -> (IO () -> IO ()) -> IO ()
--     hkRecurse loc node childNode hkEvnt releaseHkEvnt ilgr = do
--       nodeAbandonned node
--         >>= bool
--           (recurse childNode ilgr)
--           (wrapAction loc hkEvnt releaseHkEvnt (recurse childNode ilgr))

--     recurse :: forall si' so' ti' to' ii' io'. ExeTree si' so' ti' to' ii' io' -> (IO () -> IO ()) -> IO ()
--     recurse cld ilgr = logAbandonRecursively logger parentLoc e ilgr cld

--     abandonIterations :: TQueue (TQueue (Test si' ti' ii')) -> loc -> (IO () -> IO ()) -> IO ()
--     abandonIterations q loc preHookLog =
--       atomically (tryReadTQueue q)
--         >>= maybe
--           (pure ())
--           (preHookLog $ logAbandonned loc TestIteration)
--           >> abandonIterations q preHookLog

--     abandonQ :: TQueue (ExeTree si' so' ti' to' ii' io') -> (IO () -> IO ()) -> IO ()
--     abandonQ q ilgr =
--       atomically (tryReadTQueue q)
--         >>= maybe
--           (pure ())
--           (logAbandonRecursively logger parentLoc e ilgr)
--           >> abandonQ q ilgr