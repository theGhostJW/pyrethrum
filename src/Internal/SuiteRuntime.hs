module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import DSL.Logger (logMessage)
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty, replicateM)
import Data.Tuple.Extra (both, uncurry3)
import GHC.Exts
import Internal.PreNode (PreNode (testHook), PreNodeRoot, OnceHook)
import qualified Internal.PreNode as PN
  ( PreNode (..),
    PreNodeRoot,
    Test (..),
  )
import Internal.RunTimeLogging
  ( ApLogger,
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
import PyrethrumExtras.IO (hPutStrLn, putStrLn)
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
import Prelude hiding (newEmptyTMVarIO, newTVarIO, atomically)
import PyrethrumExtras hiding (finally)
import BasePrelude (retry, NonTermination)
import UnliftIO.Concurrent

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
      XTOHook {status} -> status
      XTTHook {thSubNodes=XTGroup { status } } -> status
      -- XTGroup {status} -> status
      XTFix {status} -> status

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
  { maxIndex :: Int,
    lst :: [a],
    currIdx :: TVar Int
  }

mkIdxLst :: a -> STM (IdxLst a)
mkIdxLst elm = IdxLst 0 [elm] <$> newTVar 0

data XTGroup oi ti = XTGroup { 
      status :: TVar Status,
      childNodes :: TQueue (ExeTree oi ti),
      fullyRunning :: TQueue (ExeTree oi ti)
    }


data ExeTree oi ti where
  XTOHook ::
    { loc :: Loc,
      status :: TVar Status,
      sHook :: OnceHook oi oo,
      sHookVal :: TMVar (Either Abandon oo),
      oSubNodes :: XTGroup oo ti
    } ->
    ExeTree oi ti
  XTTHook ::
    { loc :: Loc,
      thHook :: Loc -> ApLogger -> oi -> ti -> IO to,
      thHookRelease :: Loc -> ApLogger -> to -> IO (),
      thSubNodes :: XTGroup oi to
    } ->
    ExeTree oi ti
  XTFix ::
    { loc :: Loc,
      status :: TVar Status,
      tHook :: Loc -> ApLogger -> oi -> ti -> IO io,
      tHookRelease :: Loc -> ApLogger -> io -> IO (),
      fixtures :: TQueue (Test oi ti io),
      runningCount :: TVar Int
    } ->
    ExeTree oi ti

data Test oi ti ii = Test
  { tstId :: Text,
    tst :: ApLogger -> oi -> ti -> ii -> IO ()
  }

prepare :: PreNode o t -> IO (ExeTree o t)
prepare =
  prepare' Root 0
  where
    consNoMxIdx :: IdxLst a -> a -> IdxLst a
    consNoMxIdx l@IdxLst {lst} i = l {lst = i : lst}

    -- mkGroup :: forall oi ti. Loc -> [PreNode oi ti] -> IO (XTGroup oi ti) 
    mkGroup parentLoc subElms = do
            fr <- newTQueueIO
            q <- newTQueueIO
            s <- newTVarIO Pending
            -- load the queue
            c <- traverse (prepare' parentLoc 0) subElms
            atomically $ traverse_ (writeTQueue q) c
            pure $
              XTGroup
                { 
                  status = s,
                  childNodes = q,
                  fullyRunning = fr
                }

    prepare' :: Loc -> Int -> PN.PreNode oi ti -> IO (ExeTree oi ti)
    prepare' parentLoc subElmIdx pn = do
      let nodeLoc tag =
            Node
              { parent = parentLoc,
                tag = tag
              }
      case pn of
        PN.OnceHook
          { title,
            hook,
            onceSubNodes
          } -> do
            s <- newTVarIO Pending
            v <- newEmptyTMVarIO
            let loc = nodeLoc title
            child <- mkGroup loc onceSubNodes
            pure $
              XTOHook
                { loc,
                  status = s,
                  sHook = hook,
                  sHookVal = v,
                  oSubNodes = child
                }
        PN.ThreadHook
          { title,
            threadHook,
            threadSubNodes,
            threadHookRelease
          } ->
            let loc = nodeLoc title
             in do 
              subMods <- mkGroup loc threadSubNodes
              pure $ XTTHook  { 
               loc = loc,
               thHook = threadHook,
               thHookRelease = threadHookRelease,
               thSubNodes = subMods
              }
        PN.Fixtures
          { 
            testHook,
            testHookRelease,
            title,
            fixtures
          } ->
            do
              let loc = nodeLoc title
                  converTest PN.Test {tstId, tst} = Test tstId tst
              s <- newTVarIO Pending
              q <- newTQueueIO
              runningCount <- newTVarIO 0
              atomically $ traverse_ (writeTQueue q . converTest) fixtures
              pure $
                XTFix
                  { loc = loc,
                    status = s,
                    fixtures = q,
                    runningCount,
                    tHook = testHook,
                    tHookRelease = testHookRelease
                  }

logAbandonned :: Logger -> Loc -> ExeEventType -> Abandon -> IO ()
logAbandonned logger floc fet Abandon {sourceLoc, sourceEventType, exception} =
  logger (mkParentFailure floc fet sourceLoc sourceEventType exception)

logFailure :: Logger -> Loc -> ExeEventType -> SomeException -> IO ()
logFailure logger loc et e = logger (mkFailure loc et (txt et <> "Failed at: " <> txt loc) e)

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

runLogHook :: forall hi ho. ExeEventType -> Logger -> (Loc -> ApLogger -> hi -> IO ho) -> hi -> Context -> IO (Either Abandon ho)
runLogHook hkEvent logger hook hi ctx@Context {cloc, apLogger} =
  withStartEnd logger cloc hkEvent $
    catchAll
      ( Right <$> hook cloc apLogger hi
      )
      ( \e ->
          logFailure logger cloc hkEvent e
            >> pure (Left $ Abandon cloc hkEvent e)
      )

normalHookVal :: forall hi ho. ExeEventType -> Logger -> (Loc -> ApLogger -> hi -> IO ho) -> hi -> TVar Status -> TMVar (Either Abandon ho) -> Context -> IO (Either Abandon ho)
normalHookVal hkEvent logger hook hi hs hkVal ctx =
  runOrReturn
    hs
    hkVal
    ( runLogHook hkEvent logger hook hi ctx
        >>= atomically . setRunningVal hs hkVal
    )

withStartEnd :: Logger -> Loc -> ExeEventType -> IO a -> IO a
withStartEnd logger loc evt io = do
  logger $ Start evt loc
  finally io . logger $ End evt loc

abandonLogHook :: ExeEventType -> Logger -> Abandon -> Loc -> IO (Either Abandon a)
abandonLogHook hkEvent logger abandon floc =
  do
    withStartEnd logger floc hkEvent $
      logAbandonned logger floc hkEvent abandon
    pure $ Left abandon

abandonnedHookVal :: forall ho. ExeEventType -> Logger -> Abandon -> TVar Status -> TMVar (Either Abandon ho) -> Loc -> IO (Either Abandon ho)
abandonnedHookVal hkEvent logger abandon hs hkVal loc =
  runOrReturn
    hs
    hkVal
    ( abandonLogHook hkEvent logger abandon loc
        >>= atomically . setRunningVal hs hkVal
    )

threadHookVal :: forall so hi ho. Logger -> Either Abandon (ExeIn so hi) -> ExeEventType -> (Loc -> ApLogger -> so -> hi -> IO ho) -> Context -> IO (Either Abandon ho)
threadHookVal logger hkIn hkEvent hook ctx@Context {cloc, apLogger} =
  hkIn
    & either
      (\abandon -> abandonLogHook hkEvent logger abandon cloc)
      ( \(ExeIn oi ti) ->
          let thrdHk loc aplgr = hook loc aplgr oi
           in runLogHook hkEvent logger thrdHk ti ctx
      )

onceHookVal :: forall hi ho. Logger -> ExeEventType -> Either Abandon hi -> (Loc -> ApLogger -> hi -> IO ho) -> TVar Status -> TMVar (Either Abandon ho) -> Context -> IO (Either Abandon ho)
onceHookVal logger hkEvent ehi hook hs hkVal ctx =
  either
    (\abandon -> abandonnedHookVal hkEvent logger abandon hs hkVal $ ctx.cloc)
    (\hi' -> normalHookVal hkEvent logger hook hi' hs hkVal ctx)
    ehi

setStatusRunning :: TVar Status -> STM Bool
setStatusRunning status =
  do
    s <- readTVar status
    let change = s < Running
    when change $
      writeTVar status Running
    pure change

releaseHook :: Logger -> ExeEventType -> Either Abandon ho -> Context -> (Loc -> ApLogger -> ho -> IO ()) -> IO ()
releaseHook logger evt eho ctx@Context {cloc, apLogger} hkRelease =
  withStartEnd logger cloc evt $
    eho
      & either
        (logAbandonned logger cloc evt)
        ( \so ->
            catchAll
              (hkRelease cloc apLogger so)
              (logger . mkFailure cloc evt ("Hook Release Failed: " <> txt evt <> " " <> txt cloc))
        )

releaseHookUpdateStatus :: Logger -> ExeEventType -> TVar Status -> Either Abandon ho -> Context -> (Loc -> ApLogger -> ho -> IO ()) -> IO ()
releaseHookUpdateStatus logger evt ns eho ctx hkRelease =
  finally
    (releaseHook logger evt eho ctx hkRelease)
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
  { sourceLoc :: Loc,
    sourceEventType :: ExeEventType,
    exception :: SomeException
  }
  deriving (Show)

data ExeIn oi ti = ExeIn
  { singletonIn :: oi,
    threadIn :: ti
  }

data Context = Context
  { cloc :: Loc,
    apLogger :: ApLogger
  }

executeNode :: forall oi ti. Logger -> Either Abandon (ExeIn oi ti) -> ExeTree oi ti -> IO ()
executeNode logger hkIn rg =
  do
    wantRun <- atomically $ canRun rg
    when
      wantRun
      case rg of
        XTOHook
          { loc = hookLoc,
            status,
            sHook,
            sHookVal,
            oSubNodes = XTGroup {status = childrenStatus}
          } ->
            do
              -- onceHookVal:
              --  1. runs hook if required
              --  2. waits if hook is running
              --  3. updates hook status to running
              --  4. returns hook result
              -- must run for logging even if hkIn is Left
              let nxtHkIn so = (\exi -> exi {singletonIn = so}) <$> hkIn
                  recurse a = uu -- to doexeNxt a oSubNodes
                  ctx = context hookLoc
                  releaseContext = context . Node hookLoc $ txt L.OnceHookRelease
              eso <- onceHookVal logger L.OnceHook siHkIn sHook status sHookVal ctx
              finally
                ( eso & either
                    (recurse . Left)
                    (recurse . nxtHkIn)
                )
                ( do
                    wantRelease <- atomically $ do
                      childStatus <- readTVar childrenStatus
                      s <- readTVar status
                      pure $ childStatus == Done && s < HookFinalising
                    when wantRelease $
                      releaseHookUpdateStatus logger L.OnceHookRelease status eso releaseContext sHookRelease
                )
        XTTHook
          { loc = hookLoc,
            thHook,
            thHookRelease,
            thSubNodes
          } ->
            do
              let nxtHkIn ti = (\exi -> exi {threadIn = ti}) <$> hkIn
                  recurse a = uu -- todo exeNxt a thSubNodes -- write exeNxt in terms of XTGroup
                  hkCtx = context hookLoc
                  releaseCtx = context . Node hookLoc $ txt L.ThreadHookRelease
              eto <- threadHookVal logger hkIn L.ThreadHook thHook hkCtx
              finally
                ( either
                    (recurse . Left)
                    (recurse . nxtHkIn)
                    eto
                )
                (releaseHook logger L.ThreadHookRelease eto releaseCtx thHookRelease)
        -- self@XTGroup
        --   { loc,
        --     status,
        --     childNodes,
        --     fullyRunning
        --   } ->
        --     withStartEnd logger loc L.Group recurse
        --     where
        --       -- when the last thread finishes child nodes and fully running will be done
        --       recurse =
        --         atomically nxtChildNode
        --           >>= maybe
        --             (atomically updateStatusFromQs)
        --             (\c -> exeNxt hkIn c >> recurse)

        --       updateStatusFromQs :: STM ()
        --       updateStatusFromQs = do
        --         s <- readTVar status
        --         isDone s
        --           ? pure ()
        --           $ do
        --             discardDone fullyRunning
        --             discardDoneMoveFullyRunning fullyRunning childNodes
        --             mtChilds <- isEmptyTQueue childNodes
        --             mtFullyRunning <- isEmptyTQueue fullyRunning
        --             let completed = mtChilds && mtFullyRunning
        --             if
        --                 | completed -> writeTVar status Done
        --                 | mtChilds -> writeTVar status FullyRunning
        --                 | otherwise -> pure ()

        -- nxtChildNode =
        --   tryReadTQueue childNodes
        --     >>= maybe
        --       (pure Nothing)
        --       ( \cld ->
        --           let ioCld = pure $ Just cld
        --               qChld = writeTQueue childNodes cld
        --               qChldReturn = qChld >> ioCld
        --               qFullyRunning = writeTQueue fullyRunning cld >> nxtChildNode
        --            in getStatus cld
        --                 >>= \case
        --                   Pending ->
        --                     setStatusRunning status >> qChldReturn
        --                   HookExecuting ->
        --                     -- just plonk on the end of the q and go around again
        --                     -- may pick up same node if there is only one node running but that is ok
        --                     -- it just turns into a wait
        --                     qChld >> nxtChildNode
        --                   Running -> qChldReturn
        --                   FullyRunning -> qFullyRunning
        --                   HookFinalising -> qFullyRunning
        --                   -- discard if done
        --                   Done -> nxtChildNode
        --       )
        fx@XTFix
          { status,
            loc,
            fixtures,
            runningCount,
            tHook,
            tHookRelease
          } ->
            withStartEnd logger loc L.Fixture $ do
              -- eso <- onceHookVal logger L.FixtureOnceHook siHkIn fxOHook fxOHookStatus fxOHookVal onceHkCtx
              -- fxIn <- threadHookVal logger (liftA2 ExeIn eso $ (.threadIn) <$> hkIn) L.FixtureThreadHook fxTHook threadHkCtx
              recurse hkIn
            where
              -- ctx = context loc

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
                    ( \Test {tstId, tst} -> do
                        io <- runTestHook (tstHkloc tstId) fxIpts tHook
                        let unpak io' (ExeIn so2 to2) = (so2, to2, io')
                            ethInputs = liftA2 unpak io fxIpts
                            testLoc = tstLoc tstId

                        finally
                          ( withStartEnd logger testLoc L.Test $
                              ethInputs & either
                                (logAbandonned' testLoc L.Test)
                                \(so2, to2, io') ->
                                  catchAll
                                    (tst apLog so2 to2 io')
                                    (logFailure' (tstLoc tstId) L.Test)
                          )
                          do
                            releaseHook logger L.TestHookRelease io (context $ tstHkReleaseloc tstId) tHookRelease
                            atomically (modifyTVar runningCount pred)
                            recurse fxIpts
                    )

              runTestHook :: forall io' so' to'. Loc -> Either Abandon (ExeIn so' to') -> (Loc -> ApLogger -> so' -> to' -> IO io') -> IO (Either Abandon io')
              runTestHook hkLoc fxIpts testHk =
                do
                  withStartEnd logger hkLoc L.TestHook $
                    either
                      logReturnAbandonned
                      runHook
                      fxIpts
                where
                  logReturnAbandonned a =
                    logAbandonned logger hkLoc L.TestHook a
                      >> pure (Left a)

                  runHook (ExeIn oi ti) =
                    catchAll
                      (Right <$> testHk hkLoc apLog oi ti)
                      ( \e -> do
                          logFailure logger hkLoc L.TestHook e
                          pure . Left $ Abandon hkLoc L.TestHook e
                      )
  where
    siHkIn :: Either Abandon oi
    siHkIn = (.singletonIn) <$> hkIn

    exeNxt :: forall oi' ti'. Either Abandon (ExeIn oi' ti') -> ExeTree oi' ti' -> IO ()
    exeNxt = executeNode logger

    nxtAbandon :: Loc -> ExeEventType -> SomeException -> Abandon
    nxtAbandon loc' et e = fromLeft (Abandon loc' et e) hkIn

    logAbandonned' :: Loc -> ExeEventType -> Abandon -> IO ()
    logAbandonned' = logAbandonned logger

    logFailure' :: Loc -> ExeEventType -> SomeException -> IO ()
    logFailure' = logFailure logger

    context :: Loc -> Context
    context l = Context l apLog

    apLog :: Text -> IO ()
    apLog msg = logger $ \idx trdId ->
      ApLog
        { idx = idx,
          threadId = trdId,
          msg = msg
        }

type Logger = (Int -> SThreadId -> ExeEvent) -> IO ()

newLogger :: EventSink -> ThreadId -> IO Logger
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