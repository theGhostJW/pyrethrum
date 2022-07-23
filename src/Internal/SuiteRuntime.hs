module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import Data.Function (const, ($), (&))
import Data.Map.Strict as M (foldrWithKey, toList)
import Data.Sequence (Seq (Empty), empty, replicateM)
import Data.Tuple.Extra (both)
import GHC.Exts
import GHC.IO.FD (release)
import Internal.PreNode
  ( PreNode (..),
    PreNodeRoot,
    rootNode,
  )
import Internal.RunTimeLogging (ExeEvent (EndExecution, StartExecution), Index (Index), Loc (..), LogControls (LogControls), PThreadId, Sink, logWorker, mkLogger, sink, stopWorker)
import LogTransformation.PrintLogDisplayElement (PrintLogDisplayElement (tstTitle))
import Polysemy.Bundle (subsumeBundle)
import Pyrelude as P hiding
  ( ThreadRunning,
    ThreadStatus,
    atomically,
    bracket,
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
    isEmptyTBQueue,
    newIORef,
    newMVar,
    newTMVar,
    peekTQueue,
    pureTry,
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

setStatusIfApplicable :: ExeTree si so ti to ii io -> Status -> STM ()
setStatusIfApplicable et s =
  case et of
    RTNodeS {status} -> writeTVar status s
    RTNodeT {} -> pure ()
    RTNodeI {} -> pure ()
    RTNodeM {nStatus} -> writeTVar nStatus s
    RTFix {nStatus} -> writeTVar nStatus s

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

data Iteration si ti ii = Iteration
  { id :: Text,
    action :: si -> ti -> ii -> IO ()
  }

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
      iterations :: TQueue (Iteration si ti ii),
      runningCount :: TVar Int
    } ->
    ExeTree si () ti () ii ()

prepare :: PreNode () () () () () () -> IO (ExeTree () () () () () ())
prepare =
  prepare' (Loc "ROOT") 0
  where
    consNoMxIdx :: IdxLst a -> a -> IdxLst a
    consNoMxIdx l@IdxLst {lst} i = l {lst = i : lst}

    mkLoc :: Loc -> Int -> Maybe Text -> Text -> Loc
    mkLoc parentLoc subElmIdx childlabel elmType =
      Loc . ((unLoc parentLoc <> " . ") <>) $
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
        OnceHook
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
        ThreadHook
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
        TestHook
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
        Fixture
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
              atomically $ traverse_ (writeTQueue q) $ PRL.uncurry Iteration <$> M.toList iterations
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

hookVal :: forall hi ho. (hi -> IO ho) -> hi -> TVar Status -> TMVar (Either SomeException ho) -> Loc -> IO (HookResult ho)
hookVal hook hi hs hkVal loc =
  atomically readOrLock
    >>= maybe
      ( catchAll
          (hook hi >>= atomically . setHookStatus . Right)
          (atomically . setHookStatus . Left)
          >>= pure . HookResult True
      )
      (pure . HookResult False)
  where
    readOrLock :: STM (Maybe (Either SomeException ho))
    readOrLock = do
      s <- readTVar hs
      s == Pending
        ? (writeTVar hs HookExecuting >> pure Nothing)
        $ (Just <$> readTMVar hkVal)

    setHookStatus :: Either SomeException ho -> STM (Either SomeException ho)
    setHookStatus eso = do
      putTMVar hkVal eso
      eso
        & either
          (const $ writeTVar hs Done)
          (const $ writeTVar hs Running)
      pure eso

threadSource ::
  forall si ti to.
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
threadSource mHkVal hkStatus si tio hk loc =
  tio
    >>= either
      (pure . Left)
      (\ti -> value <$> hookVal (hk si) ti hkStatus mHkVal loc)

failRecursively :: forall si so ti to ii io. Text -> SomeException -> ExeTree si so ti to ii io -> STM ()
failRecursively msg e = recurse
  where
    failure :: Status
    failure = Done

    failQ :: TQueue (ExeTree si' so' ti' to' ii' io') -> STM ()
    failQ q =
      tryReadTQueue q
        >>= maybe
          (pure ())
          \rg -> recurse rg >> retry

    emptyQ :: TQueue a -> STM ()
    emptyQ q =
      tryReadTQueue q
        >>= maybe
          (pure ())
          (const retry)

    recurse :: ExeTree si' so' ti' to' ii' io' -> STM ()
    recurse rg = do
      setStatusIfApplicable rg failure
      case rg of
        RTNodeS {sChildNode} -> recurse sChildNode
        RTNodeT {tChildNode} -> recurse tChildNode
        RTNodeI {iChildNode} -> recurse iChildNode
        RTNodeM {childNodes, fullyRunning} -> do
          failQ childNodes
          failQ fullyRunning
        RTFix {iterations} -> emptyQ iterations

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

releaseHook :: ho -> TVar Status -> Loc -> (ho -> IO ()) -> IO ()
releaseHook ho ns label hkRelease =
  finally
    (hkRelease ho)
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

executeNode :: forall si so ti to ii io. Sink -> si -> IO (Either SomeException ti) -> TestHk ii -> ExeTree si so ti to ii io -> IO ()
executeNode sink si ioti tstHk rg =
  do
    let exeNxt :: forall si' so' ti' to' ii' io'. si' -> IO (Either SomeException ti') -> TestHk ii' -> ExeTree si' so' ti' to' ii' io' -> IO ()
        exeNxt = executeNode sink

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
              --  3. updates hook status
              --  4. returns hook result
              HookResult {hasExecuted, value = ethHkRslt} <- hookVal sHook si status sHookVal loc
              if hasExecuted
                then
                  ethHkRslt -- the hook that executes waits for child completion and sets status
                    & either
                      ( \e ->
                          atomically $
                            -- status already set by hookVal
                            failRecursively ("Parent hook failed: " <> unLoc loc) e sChildNode
                      )
                      ( \so ->
                          finally
                            (exeNxt so ioti tstHk sChildNode)
                            ( do
                                atomically $ waitDone sChildNode
                                releaseHook so status loc sHookRelease
                            )
                      )
                else
                  ethHkRslt
                    & either
                      (const $ pure ())
                      (\so -> exeNxt so ioti tstHk sChildNode)
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
              let ts = threadSource toVal status si ioti tHook loc
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

type EventLogger = (Index -> PThreadId -> ExeEvent) -> IO ()

newLogger :: Sink -> IO EventLogger
newLogger sink = do
  r <- UnliftIO.newIORef (Index 0)
  pure $ mkLogger sink r

executeGraph :: Sink -> ExeTree () () () () () () -> Int -> IO ()
executeGraph sink xtri maxThreads = do
  logger <- newLogger sink
  logger StartExecution
  finally
    ( replicateM_
        maxThreads
        ( C.forkIO $ do
            logger' <- newLogger sink
            uu
        )
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
