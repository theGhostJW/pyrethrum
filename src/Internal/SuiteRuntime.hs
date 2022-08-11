module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import DSL.Logger (logMessage)
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty, replicateM)
import Data.Tuple.Extra (both)
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
      RTNodeM {nStatus} -> nStatus
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
    RTNodeM {nStatus} -> update nStatus
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
  RTNodeM ::
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
              RTNodeM
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
             in do
                  v <- newEmptyTMVarIO
                  sni <- newTVarIO 0
                  fxi <- newTVarIO 0
                  child <- prepare' loc 0 threadHookChild
                  pure $
                    RTNodeT
                      { loc,
                        tHook = threadHook loc,
                        tHookRelease = threadHookRelease loc,
                        tChildNode = child
                      }
        PN.TestHook
          { testTag,
            testHook,
            testHookChild,
            testHookRelease
          } ->
            let loc = mkLoc' testTag "TestHook"
             in do
                  child <- prepare' loc 0 testHookChild
                  pure $
                    RTNodeI
                      { loc,
                        iHook = testHook loc,
                        iHookRelease = testHookRelease loc,
                        iChildNode = child
                      }
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

data HookResult so = HookResult
  { hasExecuted :: Bool,
    value :: Either SomeException so
  }

data HookLogging = HookLogging
  { hkEvent :: L.ExeEventType,
    logger :: Logger,
    mAbandon :: Maybe Abandon
  }

logAbandonned :: Logger -> Loc -> Abandon -> IO ()
logAbandonned lgr loc Abandon {sourceLoc, exception} = lgr (mkParentFailure sourceLoc loc exception)

-- logAbandonnedWithAction :: IO () -> Loc -> ExeEventType -> IO ()
-- logAbandonnedWithAction action loc ev = do
--   logger $ Start loc ev
--   mkAbaondonLogEntry loc
--   finally
--     action
--     (logger $ End loc ev)

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

runOrReturn :: TVar Status -> TMVar (Either SomeException ho) -> IO (HookResult ho) -> IO (HookResult ho)
runOrReturn hkStatus hkVal hkAction =
  atomically (readOrLockHook hkStatus hkVal)
    >>= maybe
      hkAction
      (pure . HookResult False)

normalHookVal :: forall hi ho. ExeEventType -> Logger -> (hi -> IO ho) -> hi -> TVar Status -> TMVar (Either SomeException ho) -> Loc -> IO (HookResult ho)
normalHookVal hkEvent logger hook hi hs hkVal loc =
  runOrReturn
    hs
    hkVal
    ( do
        let setValStatus = setHookValStatus hs hkVal
        eho <-
          withStartEnd logger loc hkEvent $
            catchAll
              ( do
                  ho <- hook hi
                  atomically . setValStatus $ Right ho
              )
              ( \e -> do
                  logger (mkFailure loc ("Hook Failed: " <> txt hkEvent <> " " <> txt loc) e)
                  atomically . setValStatus $ Left e
              )
        pure $ HookResult True eho
    )

withStartEnd :: Logger -> Loc -> ExeEventType -> IO a -> IO a
withStartEnd lgr loc evt io = do
  lgr $ Start loc evt
  finally io $ lgr $ End loc evt

abandonnedHookVal :: forall ho. ExeEventType -> Logger -> Abandon -> TVar Status -> TMVar (Either SomeException ho) -> Loc -> IO (HookResult ho)
abandonnedHookVal hkEvent logger abandon@Abandon {exception} hs hkVal loc =
  runOrReturn
    hs
    hkVal
    ( do
        withStartEnd logger loc hkEvent $
          logAbandonned logger loc abandon
        atomically $ setHookValStatus hs hkVal $ Left exception
        pure $ HookResult True $ Left exception
    )


hookVal :: forall hi ho. Logger -> ExeEventType -> IO (Either Abandon (hi, hi -> IO ho)) -> TVar Status -> TMVar (Either SomeException ho) -> Loc -> IO (HookResult ho)
hookVal logger hkEvent ehi  hs hkVal loc =
  ehi
    >>= either
      (\abandon -> abandonnedHookVal hkEvent logger abandon hs hkVal loc)
      (\(hi', hook) -> normalHookVal hkEvent logger hook hi' hs hkVal loc)

threadSource ::
  forall si ti to.
  Logger ->

  IO (Either Abandon ti)  ->
  -- | cached value
  TMVar (Either SomeException to) ->
  -- | status
  TVar Status ->
  -- | singleton in
  si ->
  -- | thread hook in
  IO (Either SomeException ti) ->
  -- | thread hook
  (si -> ti -> IO to) ->
  -- | hook location
  Loc ->
  IO (Either SomeException to)
threadSource logger ethIn mHkVal hkStatus si hk loc =
      ethIn
        >>= either
          (pure . Left)
          (\ti -> value <$> hookVal hl (hk si) ti hkStatus mHkVal loc)

data TestHk io = TestHk
  { tstHkLoc :: Loc,
    tstHkHook :: IO (Either SomeException io),
    tstHkRelease :: io -> IO ()
  }

runHookWith :: TestHk ii -> (ii -> IO io) -> IO (Either SomeException io)
runHookWith
  TestHk
    { tstHkLoc,
      tstHkHook,
      tstHkRelease
    }
  iteration = do
    hkRslt <- tstHkHook
    hkRslt
      & either
        ( \e -> do
            -- TODO: log error
            error $ "hook failed\n" <> displayException e
        )
        ( \io ->
            finally
              ( catchAll
                  (Right <$> iteration io)
                  ( \e -> --do
                  -- TODO: log error
                      error $ "test iteration failed failed\n" <> displayException e
                  )
              )
              ( catchAll
                  (tstHkRelease io)
                  ( \e -> --do
                  -- TODO: log error
                      error $ "test iteration hook release\n" <> displayException e
                  )
              )
        )

runIfCan :: TVar Status -> IO () -> IO ()
runIfCan s action =
  atomically (readTVar s)
    >>= bool (pure ()) action . (< FullyRunning)

setStatusRunning :: TVar Status -> STM ()
setStatusRunning status = modifyTVar status \s -> s < Running ? Running $ s

releaseHook :: Logger -> ExeEventType -> Either Abandon ho -> TVar Status -> Loc -> (ho -> IO ()) -> IO ()
releaseHook lgr evt eho ns loc hkRelease =
  finally
    (withStartEnd lgr loc evt $
      eho & either
        (logAbandonned lgr loc)
        (
          \so -> catchAll
            (hkRelease so)
            (lgr . mkFailure loc ("Hook Release Failed: " <> txt evt <> " " <> txt loc))
        )

    )
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

executeNode :: forall si so ti to ii io. Logger -> Either Abandon (IO (ExeIn si ti)) -> TestHk ii -> ExeTree si so ti to ii io -> IO ()
executeNode logger hkIn tstHk rg =
  do
    let exeNxt :: forall si' so' ti' to' ii' io'. Either Abandon (IO (ExeIn si' ti')) -> TestHk ii' -> ExeTree si' so' ti' to' ii' io' -> IO ()
        exeNxt = executeNode logger

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
              -- hookVal:
              --  1. runs hook if required
              --  2. waits if hook is running
              --  3. updates hook status to running
              --  4. returns hook result
              -- must run for logging even if hkIn is Left
              let siIn =  traverse ((, sHook)  . singletonIn <$>) hkIn
              HookResult {value = eso} <- hookVal logger L.OnceHook siIn status sHookVal loc
              let nxtHkIn so = ((\exi -> exi {singletonIn = so}) <$>) <$> hkIn
                  recurse so = exeNxt (nxtHkIn so) tstHk sChildNode
                  nxtAbandon e = either id (const $ Abandon loc e) hkIn
                  recurseAbandon e = exeNxt (Left $ nxtAbandon e) tstHk sChildNode
               in finally
                    ( either
                        recurseAbandon
                        recurse
                        eso
                    )
                    ( do
                        s <- atomically $ getStatus sChildNode
                        when (s == Done) $
                          releaseHook logger L.OnceHook (mapLeft nxtAbandon eso) status loc sHookRelease
                    )
        RTNodeT
          { loc,
            tHook,
            tHookRelease,
            tChildNode
          } ->
            do
              -- create a new instance of cache for every thread
              toVal <- newEmptyTMVarIO
              status <- newTVarIO Pending
              let tiIn = traverse ((\(ExeIn si ti) -> (ti, tHook si) ) <$>) hkIn
              HookResult {value = eto} <- hookVal logger L.ThreadHook tiIn status toVal loc
              finally
                (exeNxt si ts tstHk tChildNode)
                do
                  -- only run clean up if hook has run
                  notRun <- atomically $ isEmptyTMVar toVal
                  unless notRun $ do
                    ethHkVal <- atomically $ readTMVar toVal
                    whenRight ethHkVal $
                      \to -> releaseHook to status loc tHookRelease
        ihk@RTNodeI
          { loc,
            iHook,
            iHookRelease,
            iChildNode
          } ->
            let newTstHk =
                  TestHk
                    { tstHkLoc = loc,
                      tstHkHook = ioti >>= either (pure . Left) (runHookWith tstHk . iHook si),
                      tstHkRelease = iHookRelease
                    }
             in exeNxt si ioti newTstHk iChildNode
        RTNodeM
          { leafloc,
            nStatus,
            childNodes,
            fullyRunning
          } ->
            runIfCan nStatus recurse
            where
              recurse :: IO ()
              recurse =
                atomically nxtChild
                  >>= maybe
                    updateStatusFromQs
                    ( \c ->
                        finally
                          ( catchAll
                              (exeNxt si ioti tstHk c)
                              ( \e -> -- TODO: log error - exceptions should be handled
                                  error $ "this should not happen\n" <> displayException e
                              )
                          )
                          recurse
                    )

              updateStatusFromQs = atomically $ do
                s <- readTVar nStatus
                isDone s
                  ? pure ()
                  $ do
                    discardDone fullyRunning
                    discardDoneMoveFullyRunning fullyRunning childNodes
                    mtChilds <- isEmptyTQueue childNodes
                    mtFullyRunning <- isEmptyTQueue fullyRunning
                    mtChilds && mtFullyRunning
                      ? writeTVar nStatus Done
                      $ mtChilds && s /= FullyRunning
                        ? writeTVar nStatus FullyRunning
                        $ pure ()

              nxtChild =
                tryReadTQueue childNodes
                  >>= maybe
                    (pure Nothing)
                    ( \cld ->
                        let thisChild = pure $ Just cld
                            enqChild = writeTQueue childNodes cld
                            enqChildReturn = enqChild >> thisChild
                         in getStatus cld
                              >>= \case
                                Pending ->
                                  setStatusRunning nStatus >> enqChildReturn
                                HookExecuting ->
                                  -- just plonk on the end of the q and go around again
                                  -- may pick up same node if htere is only one node running but that is ok
                                  -- it just turns into a wait
                                  enqChild >> nxtChild
                                Running -> enqChildReturn
                                FullyRunning ->
                                  writeTQueue fullyRunning cld >> nxtChild
                                HookFinalising ->
                                  writeTQueue fullyRunning cld >> nxtChild
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
                            (void $ runHookWith tstHk tst)
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
            TestHk
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
--       RTNodeM {leafloc, childNodes, fullyRunning} -> abandonQ childNodes
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
--   RTNodeM {leafloc, childNodes, nStatus} ->
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