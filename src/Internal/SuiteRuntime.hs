module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import DSL.Logger (logMessage)
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty, replicateM)
import Data.Tuple.Extra (both, uncurry3)
import GHC.Exts
import Internal.PreNode (PreNodeRoot)
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
    Sink,
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
      sHookVal :: TMVar (Either SomeException so),
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
      fxSHookVal :: TMVar (Either SomeException so),
      fxSHook :: si -> IO so,
      fxSHookRelease :: so -> IO (),
      fxHook :: si -> ti -> IO to,
      fxHookRelease :: to -> IO (),
      tHook :: si -> to -> IO io,
      tHookRelease :: io -> IO (),
      iterations :: TQueue (Test so to io),
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
          { fxTag,
            fxHook,
            fxHookRelease,
            testHook,
            testHookRelease,
            iterations
          } ->
            do
              let loc = nodeLoc "Fixture" fxTag
                  converTest PN.Test {tstId, tst} = Test (Node loc tstId) tst
              s <- newTVarIO Pending
              q <- newTQueueIO
              runningCount <- newTVarIO 0
              atomically $ traverse_ (writeTQueue q . converTest) iterations
              pure $
                XTFix
                  { leafloc = loc,
                    nStatus = ns,
                    iterations = q,
                    runningCount
                  }

logAbandonned :: Logger -> Loc -> Abandon -> IO ()
logAbandonned lgr loc Abandon {sourceLoc, sourceEventType, exception} =
  lgr (mkParentFailure sourceLoc loc sourceEventType exception)

logFailure :: Logger -> Loc -> ExeEventType -> SomeException -> IO ()
logFailure lgr loc et e = lgr (mkFailure loc (txt et <> "Failed at: " <> txt loc) e)

readOrLockHook :: TVar Status -> TMVar (Either SomeException ho) -> STM (Maybe (Either SomeException ho))
readOrLockHook hs hVal = do
  s <- readTVar hs
  s
    == Pending
    ? (writeTVar hs HookExecuting >> pure Nothing)
    $ (Just <$> readTMVar hVal)

setRunningVal :: TVar Status -> TMVar (Either SomeException ho) -> Either SomeException ho -> STM (Either SomeException ho)
setRunningVal hs hVal eso = do
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
        >>= atomically . setRunningVal hs hkVal
    )

withStartEnd :: Logger -> Loc -> ExeEventType -> IO a -> IO a
withStartEnd lgr loc evt io = do
  lgr $ Start loc evt
  finally io . lgr $ End loc evt

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
        >>= atomically . setRunningVal hs hkVal
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

-- data TestHkSrc hi = TestHkSrc
--   { tstHkLoc :: Loc,
--     tstHkHook :: Maybe Abandon -> IO (Either Abandon hi),
--     tstHkRelease :: Maybe Abandon -> hi -> IO ()
--   }

unpackInputs :: Either Abandon ii -> Either Abandon (ExeIn si ti) -> Either Abandon (si, ti, ii)
unpackInputs ehki aExIn =
  do
    ii <- ehki
    (ExeIn si ti) <- aExIn
    Right (si, ti, ii)

-- runTestHook :: Logger -> Either Abandon (ExeIn si ti) -> TestHkSrc hi -> (si -> ti -> hi -> IO io) -> Maybe Abandon -> IO (Either Abandon io)
-- runTestHook
--   logger
--   aExIn
--   TestHkSrc
--     { tstHkLoc,
--       tstHkHook,
--       tstHkRelease
--     }
--   testHk
--   inPlaceAbandon =
--     do
--       ehki <- tstHkHook inPlaceAbandon
--       withStartEnd logger tstHkLoc L.TestHook $
--         either
--           logReturnAbandonned
--           (uncurry3 runHook)
--           (inputs ehki)
--     where
--       logReturnAbandonned a =
--         logAbandonned logger tstHkLoc a
--           >> pure (Left a)

--       runHook si ti ii =
--         catchAll
--           (Right <$> testHk si ti ii)
--           ( \e -> do
--               logFailure logger tstHkLoc L.TestHook e
--               pure . Left $ Abandon tstHkLoc L.TestHook e
--           )

--       inputs ehki =
--         inPlaceAbandon
--           & maybe
--             (unpackInputs ehki aExIn)
--             Left

setStatusRunning :: TVar Status -> STM Bool
setStatusRunning status =
  do
    s <- readTVar status
    let change = s < Running
    when change $
      writeTVar status Running
    pure change

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

discardDone :: TQueue (ExeTree a b c d) -> STM ()
discardDone = processWhile nodeDone

processWhile :: forall a b c d e f. (ExeTree a b c d -> STM Bool) -> TQueue (ExeTree a b c d) -> STM ()
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
              -- singletonHookVal:
              --  1. runs hook if required
              --  2. waits if hook is running
              --  3. updates hook status to running
              --  4. returns hook result
              -- must run for logging even if hkIn is Left
              let siIn = (,sHook) . singletonIn <$> hkIn
              eso <- singletonHookVal logger L.OnceHook siIn status sHookVal loc
              let nxtHkIn so = (\exi -> exi {singletonIn = so}) <$> hkIn
                  nxtAbandon' = nxtAbandon loc L.OnceHook
                  recurse a = exeNxt a sChildNode
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
        XTTHook
          { loc,
            thHook,
            thHookRelease,
            thChildNode
          } ->
            do
              let tiIn = (\(ExeIn si ti) -> (ti, thHook si)) <$> hkIn
              eto <- threadHookVal logger L.ThreadHook tiIn loc
              let nxtHkIn ti = (\exi -> exi {threadIn = ti}) <$> hkIn
                  nxtAbandon' = nxtAbandon loc L.ThreadHook
                  recurse a = exeNxt a thChildNode
              finally
                ( either
                    (recurse . Left . nxtAbandon')
                    (recurse . nxtHkIn)
                    eto
                )
                (releaseHook logger L.ThreadHookRelease (mapLeft nxtAbandon' eto) loc thHookRelease)

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
            runningCount
          } -> uu
            -- do
            -- withStartEnd logger leafloc L.Fixture $
            --   do
            --     whenLeft hkIn (logAbandonned' leafloc)
            --     recurse
            -- where
            --   recurse :: IO ()
            --   recurse = do
            --     etest <-
            --       atomically $
            --         tryReadTQueue iterations
            --           >>= maybe
            --             ( do
            --                 r <- readTVar runningCount
            --                 Left
            --                   <$> if
            --                       | r < 0 -> error "framework error - this should not happen - running count below zero"
            --                       | r == 0 -> pure True
            --                       | otherwise -> writeTVar nStatus FullyRunning >> pure False
            --             )
            --             ( \t -> do
            --                 modifyTVar runningCount succ
            --                 setStatusRunning nStatus
            --                 pure $ Right t
            --             )
            --     etest
            --       & either
            --         ( \done ->
            --             when done $
            --               atomically $
            --                 writeTVar nStatus Done
            --         )
            --         ( \(Test tstLoc test) -> do
            --             ho <- tstHkHook $ leftToMaybe hkIn
            --             let ethInputs = unpackInputs ho hkIn

            --             finally
            --               ( withStartEnd logger tstLoc L.Test $
            --                   ethInputs & either
            --                     (logAbandonned' tstLoc)
            --                     \i ->
            --                       catchAll
            --                         (uncurry3 test i)
            --                         (logFailure' tstLoc L.Test)
            --               )
            --               $ atomically (modifyTVar runningCount pred) >> recurse
            --         )
  where
    exeNxt :: forall si' so' ti' to' ii' io'. Either Abandon (ExeIn si' ti') -> ExeTree si' so' ti' to' -> IO ()
    exeNxt = executeNode logger

    nxtAbandon :: Loc -> ExeEventType -> SomeException -> Abandon
    nxtAbandon loc' et e = fromLeft (Abandon loc' et e) hkIn

    logAbandonned' :: Loc -> Abandon -> IO ()
    logAbandonned' = logAbandonned logger

    logFailure' :: Loc -> ExeEventType -> SomeException -> IO ()
    logFailure' = logFailure logger

type Logger = (Int -> Text -> ExeEvent) -> IO ()

newLogger :: Sink -> ThreadId -> IO Logger
newLogger sink tid =
  do
    ir <- UnliftIO.newIORef 0
    pure $ mkLogger sink ir tid

executeGraph :: Sink -> ExeTree () () () () -> Int -> IO ()
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