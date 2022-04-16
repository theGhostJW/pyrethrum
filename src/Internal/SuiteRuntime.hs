{-# LANGUAGE MagicHash #-}

module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty)
import Data.Tuple.Extra (both)
import GHC.Exts
import Internal.PreNode
  ( CompletionStatus (Fault),
    HookStatus (Finalised, Finalising),
    finalised, FixtureStatus(..),
  )
import qualified Internal.PreNode as PN
  ( CompletionStatus (Fault, Murdered, Normal),
    HookStatus (..),
    PreNode (..),
    PreNodeRoot (..),
  )
import Polysemy.Bundle (subsumeBundle)
import Pyrelude (bool, threadDelay)
import Pyrelude hiding
  ( ThreadRunning,
    ThreadStatus,
    atomically,
    bool,
    bracket,
    newMVar,
    newTVarIO,
    parent,
    readTVarIO,
    threadDelay,
    threadStatus,
    withMVar,
  )
import Pyrelude.IO (hPutStrLn, putStrLn)
import UnliftIO
  ( Exception (displayException),
    bracket,
    catchAny,
    concurrently_,
    newMVar,
    newTMVar,
    pureTry,
    tryAny,
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

data NodeRoot o = NodeRoot
  { rootStatus :: TVar PN.HookStatus,
    children :: [Node () o]
  }

data Node i o where
  Hook ::
    { hookLabel :: Text,
      hookParent :: Either i (Node i0 i),
      hookStatus :: TVar PN.HookStatus,
      hook :: i -> IO o,
      hookResult :: TMVar (Either SomeException o),
      hookChildren :: IO [Node o o2],
      hookRelease :: Int -> o -> IO ()
    } ->
    Node i o
  Fixture ::
    { fixtureLabel :: Text,
      logStart :: IO (),
      fixStatus :: TVar FixtureStatus,
      fixParent :: Either i (Node i0 i),
      iterations :: [i -> IO ()],
      logEnd :: IO ()
    } ->
    Node i ()

data HookRunTime = HookRunTime
  { address :: Text,
    currentStatus :: TVar HookStatus
  }

hookInfo :: Node i o -> IO [HookRunTime]
hookInfo =
  hookInfo' (pure [])
  where
    hookInfo' :: IO [HookRunTime] -> Node a b -> IO [HookRunTime]
    hookInfo' accum node = do
      accum' <- accum
      case node of
        Hook
          { hookLabel,
            hookParent,
            hookStatus,
            hook,
            hookResult,
            hookChildren,
            hookRelease
          } ->
            let me = HookRunTime hookLabel hookStatus
             in do
                  c <- hookChildren >>= traverse (hookInfo' accum)
                  pure $ me : concat c
        Fixture {} -> accum

data AvailableFixture
  = FixPending PendingFixture
  | FixInitialised InitialisedFixture

data PendingFixture = PendingFixture
  { pIndex :: Int,
    pFixtureLabel :: Text, -- for debugging
    pLogStart :: IO (),
    pFixStatus :: TVar FixtureStatus,
    pIterations :: IO (TVar (Either SomeException [IO ()])),
    pLogEnd :: IO (),
    pReleaseParentHook :: IO ()
  }

data InitialisedFixture = InitialisedFixture
  { index :: Int,
    fixtureLabel :: Text, -- for debugging
    logStart :: IO (),
    fixStatus :: TVar FixtureStatus,
    iterations :: TVar (Either SomeException [IO ()]),
    activeThreads :: TVar [RunningThread],
    logEnd :: IO (),
    releaseParentHook :: IO ()
  }

type Logger = Text -> IO ()

isDone :: FixtureStatus -> Bool
isDone = \case
  Pending -> False
  Starting -> False
  Active -> False
  Done _ -> True
  BeingKilled -> False

logVal :: Show a => Logger -> Text -> IO a -> IO a
logVal db pfx val = val >>= \a -> db (pfx <> " Value: " <> txt a) >> val

logSTMVal :: Show a => Logger -> Text -> STM a -> STM a
logSTMVal db pfx val = val >>= \a -> dbStm db (pfx <> " Value: " <> txt a) >> val

nodeFinished :: Logger -> Node i o -> IO Bool
nodeFinished db =
  let log :: Show a => Text -> STM a -> STM a
      log = logSTMVal db
   in \case
        Fixture {fixStatus, fixtureLabel} -> atomically $ isDone <$> log ("FIXTURE STATUS: (nodeFinished): - " <> fixtureLabel <> " ") (readTVar fixStatus)
        Hook {hookStatus} -> atomically $ finalised <$> log "Hook Finished Status" (readTVar hookStatus)

data CanFinaliseHook
  = NotReady
  | CanBeFinalised
      { oldHookStatus :: HookStatus
      }
  | FinalisedAlready

trySetFinalising :: Logger -> TVar HookStatus -> STM CanFinaliseHook
trySetFinalising db hs' =
  let nr = pure NotReady
      fa = pure FinalisedAlready
   in do
        hs <- readTVar hs'
        dbStm db $ "HOOK STATUS: trySetFinalising " <> txt hs
        case hs of
          PN.Unintialised -> nr
          PN.Intitialising -> nr
          PN.Running -> nr
          PN.Complete cs ->
            case cs of
              s@PN.Normal ->
                writeTVar hs' PN.Finalising $> CanBeFinalised hs
              PN.Fault txt' ex -> writeTVar hs' (PN.Finalised $ PN.Fault ("Pre hook faulted: " <> txt') ex) >> fa
              m@(PN.Murdered _) -> writeTVar hs' (PN.Finalised m) >> fa
          PN.BeingMurdered -> nr
          PN.Finalising -> nr
          PN.Finalised _ -> fa

recurseHookRelease :: Logger -> Node i o -> IO ()
recurseHookRelease db n =
  do
    db "!!!!!!!! recurseHookRelease !!!!!!!!"
    case n of
      Fixture {} -> db "!!!!!!!! recurseHookRelease: Fixture !!!!!!!!" >> pure ()
      hk@Hook {hookResult, hookStatus, hookChildren, hookRelease, hookParent = parent} ->
        let --
            recurse = either (const $ pure ()) (recurseHookRelease db) parent
            setStatus = atomically . writeTVar hookStatus
            finaliseRecurse s = setStatus (PN.Finalised s) >> recurse
         in do
              hs <- atomically (trySetFinalising db hookStatus)
              case hs of
                NotReady -> db "!!!!!!!! recurseHookRelease: NOT READY !!!!!!!!" >> pure ()
                FinalisedAlready -> db "!!!!!!!! recurseHookRelease: FinalisedAlready !!!!!!!!" >> recurse
                CanBeFinalised oldStatus -> do
                  db "draw children"
                  hc <- hookChildren
                  db $ "hookChildren count: " <> txt (length hc)
                  childrenDone <- hookChildren >>= traverse (nodeFinished db)
                  not (all id childrenDone)
                    ? do
                      db "!!!!!!!! recurseHookRelease: oldStatus children not done !!!!!!!!"
                      setStatus oldStatus
                    $ do
                      db "READING HOOK RESULT"
                      ehr <- atomically $ readTMVar hookResult
                      db "FINISHED READING HOOK RESULT"
                      eitherf
                        ehr
                        ( \e -> do
                            db ("Hook release not executed - this code should never run\n" <> txt e)
                            finaliseRecurse $ PN.Fault "Hook execution failed" e
                        )
                        ( \hr -> do
                            ethr <- tryAny $ hookRelease 1 hr
                            eitherf
                              ethr
                              ( \e -> do
                                  db ("Hook release threw an exception\n" <> txt e)
                                  finaliseRecurse $ PN.Fault "Hook release threw an exception" e
                              )
                              (const $ finaliseRecurse PN.Normal)
                        )

loadFixture :: forall i o. Logger -> Text -> Either o (Node i o) -> [o -> IO ()] -> TVar FixtureStatus -> IO () -> IO () -> (Int -> IO PendingFixture)
loadFixture db fixtureLabel parent iterations fixStatus logStart logEnd =
  do
    let loadedIterations :: Either SomeException o -> Either SomeException [IO ()]
        loadedIterations hookVal = do
          hv <- hookVal
          Right $ (\f -> f hv) <$> iterations
    \i ->
      pure $
        PendingFixture
          { pIndex = i,
            pFixtureLabel = fixtureLabel,
            pLogStart = logStart,
            -- fixture status TVar is now common to pending fixture and source fixture
            pFixStatus = fixStatus,
            pIterations =
              do
                hookVal <- db "LOCK EXECUTE HOOK" >> lockExecuteHook db parent
                newTVarIO $ loadedIterations hookVal,
            pLogEnd = logEnd,
            pReleaseParentHook = either (void . pure) (recurseHookRelease db) parent
          }

data ThreadStatus
  = ThreadInitialising Int
  | ThreadRunning
  | ThreadDone
  deriving (Eq, Show)

data RunningThread = RunningThread
  { thread :: IO ThreadId,
    status :: TVar ThreadStatus
  }

{- TODO
  ~ gather fixtures
  ~ simple
  ~ fixture address
  ~ log start and end fixture (generate form test suite)
  ~ exceptions
  ~ killing
  ~ test with null iterations
-}

linkParents :: Logger -> PN.PreNodeRoot o -> IO (NodeRoot o)
linkParents db PN.PreNodeRoot {children} =
  do
    db "CALLING LINKED PARENTS"
    status <- newTVarIO PN.Unintialised

    children' <- children
    childNodes <- traverse (linkParents' db $ Left ()) children'
    pure $
      NodeRoot
        { rootStatus = status,
          children = childNodes
        }

linkParents' :: Logger -> Either o (Node i o) -> PN.PreNode o o' -> IO (Node o o')
linkParents' db parent preNode =
  do
    db "!!!!!!!! CALLING linkParents' (PRIME) !!!!! "
    case preNode of
      PN.Hook {hookAddress, hook, hookStatus, hookResult, hookChildren, hookRelease} -> do
        let mkChildren h' = traverse (linkParents' db $ Right h') hookChildren
            h =
              Internal.SuiteRuntime.Hook
                { hookLabel = hookAddress,
                  hookParent = parent,
                  hookStatus = hookStatus,
                  hook = hook,
                  hookResult = hookResult,
                  hookChildren = mkChildren h,
                  hookRelease = hookRelease
                }
        pure h
      PN.Fixture {fixtureAddress, fixtureStatus, logStart, iterations, logEnd} -> do
        pure $
          Internal.SuiteRuntime.Fixture
            { fixtureLabel = fixtureAddress,
              logStart = logStart,
              fixParent = parent,
              fixStatus = fixtureStatus,
              iterations = iterations,
              logEnd = logEnd
            }

isUninitialised :: PN.HookStatus -> Bool
isUninitialised = \case
  PN.Unintialised -> True
  PN.Intitialising -> False
  PN.Running -> False
  PN.Complete cs -> False
  PN.Finalising -> False
  PN.Finalised _ -> False
  PN.BeingMurdered -> False

tryLock :: Logger -> TVar PN.HookStatus -> STM Bool
tryLock db status =
  do
    hs <- readTVar status
    dbStm db $ "tryLock HOOK STATUS: " <> txt hs
    -----
    readTVar status
      >>= bool
        (pure False)
        (writeTVar status PN.Intitialising >> pure True)
        . isUninitialised

data SkippedException = SkippedException Text SomeException
  deriving (Show, Typeable)

instance Exception SkippedException

dbStm :: Logger -> Text -> STM ()
dbStm db = unsafeIOToSTM . db

-- dbStm :: Logger -> Text -> STM ()
-- dbStm db t = pure ()

-- executeHook only to be run when want executeHook has set
-- staus to initalising should only ever run once per branch
-- TODO - test exception on output of branch parent and in resource aquisition
executeHook :: Logger -> Node i o -> IO ()
executeHook db =
  \case
    Fixture {} -> pure ()
    Hook
      { hookParent,
        hookStatus,
        hook,
        hookResult,
        hookChildren
      } -> do
        -- up to here need stus update pre and post execute hook
        eInput <- db "CALL PARENT LOCK EXECUTE HOOK" >> lockExecuteHook db hookParent
        result <-
          eitherf
            eInput
            (pure . Left . toException . SkippedException "Parent Failed")
            (tryAny . hook)

        -- set hook result and status
        atomically do
          mtb <- isEmptyTMVar hookResult
          dbStm db $ "HOOK RESULT PUT EMPTY BEFORE: " <> txt mtb
          putTMVar hookResult result -- writes hook result to the TMVar
          mt <- isEmptyTMVar hookResult
          dbStm db $ "HOOK RESULT PUT EMPTY AFTER: " <> txt mt
          mt2 <- isEmptyTMVar hookResult
          dbStm db $ "HOOK RESULT SECOND: " <> txt mt2
          writeTVar hookStatus
            . PN.Complete
            . either
              (PN.Fault "Hook execution failed")
              (const PN.Normal)
            $ result

lockExecuteHook :: Logger -> Either o (Node i o) -> IO (Either SomeException o)
lockExecuteHook db parent =
  eitherf
    parent
    (\o -> db "NO PARENT HOOK RETURNING VALUE" >> pure (Right o))
    ( \case
        Fixture {} -> db "hook lock - FIXTURE RETURNING PURE" >> pure (Right ())
        hk@Hook
          { hookParent,
            hookStatus,
            hookResult,
            hook,
            hookChildren,
            hookRelease
          } -> do
            wantLaunch <- atomically $ tryLock db hookStatus
            db $ "HOOK LOCK >>> " <> txt wantLaunch
            when
              wantLaunch
              $ executeHook db hk --  this writes hook result to the TMVar
            mt <- atomically $ isEmptyTMVar hookResult
            db $ "READING HOOK !!!!!!!!!!!!!!!!!!!!!!!!!! EMPTY: " <> txt mt
            r <- atomically $ readTMVar hookResult
            db "RETURNING FROM LOCK EXECUTE HOOK " >> pure r
    )

mkFixturesHooks :: Logger -> Node i o -> IO ([Int -> IO PendingFixture], [HookRunTime])
mkFixturesHooks db =
  recurse $ pure ([], [])
  where
    recurse :: IO ([Int -> IO PendingFixture], [HookRunTime]) -> Node i o -> IO ([Int -> IO PendingFixture], [HookRunTime])
    recurse accum node = do
      (fxs, hks) <- accum
      case node of
        Hook
          { hookLabel,
            hookStatus,
            hookChildren
          } ->
            let acmNxt = pure (fxs, HookRunTime hookLabel hookStatus : hks)
             in hookChildren >>= foldl' recurse acmNxt
        Fixture
          { fixtureLabel,
            logStart,
            fixParent,
            iterations,
            fixStatus,
            logEnd
          } -> do
            let fx = loadFixture db fixtureLabel fixParent iterations fixStatus logStart logEnd
            pure (fx : fxs, hks)

data Executor = Executor
  { maxThreads :: Int,
    threadsInUse :: TVar Int,
    fixturesPending :: TQueue PendingFixture,
    fixturesStartNext :: TVar [PendingFixture],
    fixturesStarted :: TQueue InitialisedFixture
  }

data ThreadStats = ThreadStats
  { maxThreads :: Int,
    inUse :: Int
  }
  deriving (Show)

data NoFixture
  = EmptyQueues
  | FixtureStarting
  | NoFixturesReady
  | NoThreadsAvailable ThreadStats
  deriving (Show)

data NoCandidate
  = EmptyQueue
  | CantUseAnyMoreThreads
  | InvalidFixtureInPendingList
  | Finished

data IterationRun = IterationRun
  { parentFixture :: InitialisedFixture,
    iteration :: IO ()
  }

updateStatusReturnCompleted :: Logger -> InitialisedFixture -> STM Bool
updateStatusReturnCompleted
  db
  InitialisedFixture
    { fixStatus,
      iterations,
      activeThreads,
      fixtureLabel
    } =
    let completionBlocked :: FixtureStatus -> Bool
        completionBlocked = \case
          Pending -> False
          Starting -> True
          Active -> False
          Done _ -> False
          BeingKilled -> True
     in do
          ethits <- readTVar iterations
          s <- readTVar fixStatus
          let doneAlready = isDone s
          dbStm db $ "FIXTURE STATUS: updateStatusReturnCompleted (before) - " <> txt fixtureLabel <> " " <> txt s
          eitherf
            ethits
            ( \e -> do
                unless doneAlready $
                  writeTVar fixStatus (Done $ PN.Fault "Parent hook failed" e)
                pure $ not doneAlready
            )
            ( \i -> do
                dbStm db $ "EMPTY ITERATIONS: " <> txt (null i)
                a <- readTVar activeThreads
                dbStm db $ "EMPTY ACTIVE THREADS: " <> txt (null a)
                let completed = not doneAlready && (null i && null a && not (completionBlocked s))
                when completed $
                  writeTVar fixStatus (Done PN.Normal)
                s' <- readTVar fixStatus
                dbStm db $ "FIXTURE STATUS: updateStatusReturnCompleted (after) - " <> fixtureLabel <> " " <> txt s'
                pure completed
            )

canForkThread :: FixtureStatus -> Bool
canForkThread = \case
  Pending -> False
  Starting -> False
  Active -> True
  Done cs -> False
  BeingKilled -> False

takeIteration :: InitialisedFixture -> STM (Maybe IterationRun)
takeIteration fixture@InitialisedFixture {iterations, fixStatus} = do
  status <- readTVar fixStatus
  if canForkThread status
    then
      readTVar iterations
        >>= either
          (const $ pure Nothing)
          ( \case
              [] -> pure Nothing
              x : xs -> do
                writeTVar iterations $ Right xs
                pure . Just $ IterationRun fixture x
          )
    else pure Nothing

nextActiveFixtureRemoveDone :: Logger -> TQueue InitialisedFixture -> STM (Maybe InitialisedFixture)
nextActiveFixtureRemoveDone db activeQ =
  let getNxt :: Maybe Int -> STM (Maybe InitialisedFixture)
      getNxt mInitilIndex = do
        mfx <- tryReadTQueue activeQ
        maybef
          mfx
          (pure Nothing)
          \ifx@InitialisedFixture {index = currentIdx, fixStatus} ->
            let nxtInitial :: Maybe Int
                nxtInitial = mInitilIndex <|> Just currentIdx

                reQu :: STM ()
                reQu = writeTQueue activeQ ifx

                reQuGetNxt :: STM (Maybe InitialisedFixture)
                reQuGetNxt = reQu >> getNxt nxtInitial

                reQReturnThisFixture :: STM (Maybe InitialisedFixture)
                reQReturnThisFixture = reQu $> Just ifx
             in -- if we are back where we started we are done
                mInitilIndex == Just currentIdx
                  ? pure Nothing
                  $ do
                    dbStm db "nextActiveFixtureRemoveDone"
                    status <- readTVar fixStatus
                    case status of
                      Pending -> reQReturnThisFixture
                      Starting -> reQuGetNxt
                      Active -> reQReturnThisFixture
                      -- done fixtures are not (reQued) added to the back of the q
                      Done cs -> getNxt nxtInitial
                      -- just put at end of q and continue eventually this fixture
                      -- will become done and get kicked out of the queue above
                      BeingKilled -> reQuGetNxt
   in getNxt Nothing

-- returns the next fixture and puts on end of active fixture queue
nextFixture :: Logger -> TQueue PendingFixture -> TVar [PendingFixture] -> TQueue InitialisedFixture -> STM (Either NoFixture AvailableFixture)
nextFixture db pendingQ nxtLst activeQ =
  let notEmpty q = not <$> isEmptyTQueue q
   in do
        hasPending <- notEmpty pendingQ
        hasActive <- notEmpty activeQ
        nxtLst' <- readTVar nxtLst
        let fixturesStarting = not . null $ nxtLst'
        if
            | hasPending ->
              do
                pfx <- readTQueue pendingQ
                writeTVar nxtLst (pfx : nxtLst')
                pure . Right . FixPending $ pfx
            | hasActive ->
              maybe
                (Left NoFixturesReady)
                (Right . FixInitialised)
                <$> nextActiveFixtureRemoveDone db activeQ
            | fixturesStarting -> pure $ Left NoFixturesReady
            | otherwise ->
              pure $ Left EmptyQueues

releaseThread :: TVar Int -> STM ()
releaseThread = flip modifyTVar (\i -> i - 1)

reserveThread :: Executor -> Logger -> STM (Either ThreadStats ThreadStats)
reserveThread
  exe@Executor
    { maxThreads,
      threadsInUse
    }
  db = do
    used <- readTVar threadsInUse
    let reserved = used < maxThreads
        newUsed = reserved ? used + 1 $ used
        stats = ThreadStats {maxThreads = maxThreads, inUse = newUsed}
    --
    if reserved
      then do
        dbStm db "THREAD RESERVED"
        writeTVar threadsInUse newUsed
      else --
        dbStm db "NO THREADS AVAILABLE NONE RESERVED"

    pure $ (reserved ? Right $ Left) stats

isPending :: FixtureStatus -> Bool
isPending = \case
  Pending -> True
  Starting -> False
  Active -> False
  Done cs -> False
  BeingKilled -> False

isStarting :: FixtureStatus -> Bool
isStarting = \case
  Pending -> False
  Starting -> True
  Active -> False
  Done cs -> False
  BeingKilled -> False

setToStartedIfPending :: TVar FixtureStatus -> STM Bool
setToStartedIfPending fixStatus =
  do
    s <- readTVar fixStatus
    let pending = isPending s
    when pending $
      writeTVar fixStatus Starting
    pure pending

setActiveIfStarting :: TVar FixtureStatus -> STM Bool
setActiveIfStarting fixStatus =
  do
    s <- readTVar fixStatus
    let is = isStarting s
    when is $
      writeTVar fixStatus Active
    pure is

removeFinishedThreads :: [RunningThread] -> STM [RunningThread]
removeFinishedThreads rthds =
  let removeFinishedThreads' :: [RunningThread] -> [RunningThread] -> STM [RunningThread]
      removeFinishedThreads' accum = \case
        [] -> pure accum
        rt@RunningThread {status} : rts -> do
          s <- readTVar status
          s == ThreadRunning
            ? removeFinishedThreads' (rt : accum) rts
            $ removeFinishedThreads' accum rts
   in removeFinishedThreads' [] rthds

logError :: Text -> IO ()
logError t = print $ "Something went wrong with the runtime: " <> t

runFixture :: TVar ThreadStatus -> InitialisedFixture -> Logger -> IO ()
runFixture
  threadStatus
  fx@InitialisedFixture
    { logStart,
      fixStatus,
      iterations,
      activeThreads,
      logEnd
    }
  db =
    let runIterations :: IO ()
        runIterations = do
          wantLogStart <- atomically $ setToStartedIfPending fixStatus
          db $ "RF - WantLogStart " <> txt wantLogStart
          -- threadDelay 10_000_00
          if wantLogStart
            then do
              elst <- tryAny logStart
              eitherf
                elst
                ( \e -> do
                    db "RF - LogStartleft"
                    atomically $ writeTVar fixStatus $ Done (PN.Fault "Failed logging fixture start" e)
                    -- if we get here things have really screwed up ye old terminal as a last resort
                    logError $ "Failed logging fixture start\n" <> toS (displayException e)

                    catchAny
                      (db "RF - Loging Log End" >> logEnd)
                      (\e' -> logError $ "Failed logging fixture end\n" <> toS (displayException e'))
                )
                ( const $ do
                    db "RF - Rerun Iterations"
                    atomically $ setActiveIfStarting fixStatus
                    runIterations
                )
            else do
              mi <- atomically $ takeIteration fx
              maybe
                (db "RF - No Iterations Left" >> pure ())
                (\IterationRun {iteration} -> db "RF - Run ITERATION" >> iteration >> db "RF - Run ITERATION DONE" >> runIterations)
                mi
     in do
          db "RF - BODY"
          db $ "RF - BODY THREAD POINTER IS: " <> unsafeAddr threadStatus
          s <- readTVarIO threadStatus
          db $ "RF - BODY THREAD STATUS: " <> txt s
          case s of
            ThreadInitialising _ -> C.threadDelay 1_000 >> runFixture threadStatus fx db
            ThreadRunning -> runIterations
            ThreadDone -> pure ()

-- Any is a type to which any type can be safely unsafeCoerced to.

-- A datatype that has the same layout as Word and so can be casted to it.
newtype Ptr' a = Ptr' a

-- Any is a type to which any type can be safely unsafeCoerced to.
aToWord# :: GHC.Exts.Any -> Word#
aToWord# a = let !mb = Ptr' a in case unsafeCoerce# mb :: Word of W# addr -> addr

unsafeAddr :: a -> Text
unsafeAddr a = txt $ I# (word2Int# (aToWord# (unsafeCoerce# a)))

forkFixtureThread :: Logger -> TVar Int -> InitialisedFixture -> IO ()
forkFixtureThread
  db
  threadsInUse
  fx@InitialisedFixture {activeThreads, logEnd, releaseParentHook, fixtureLabel, fixStatus} =
    do
      db "III forkFixtureThread started"
      thrdStatus <- newTVarIO (ThreadInitialising 999)

      let releaseThread' :: IO ()
          releaseThread' =
            atomically $ do
              -- set this threadstatus to done
              writeTVar thrdStatus ThreadDone
              -- remove all finished threads from active threads list
              ats <- readTVar activeThreads
              newAts <- removeFinishedThreads ats
              writeTVar activeThreads newAts
              -- decrement global threads in use
              upre <- readTVar threadsInUse
              dbStm db "THREAD DONE"
              dbStm db $ "BEFORE DECREMENT THREAD: " <> txt upre
              releaseThread threadsInUse
              u <- readTVar threadsInUse
              dbStm db $ "AFTER DECREMENT THREAD: " <> txt u

          tfx = C.forkFinally
            (db "@@@@ FIXTURE START" >> runFixture thrdStatus fx db >> db "@@@@@ FIXTURE END !!!!!!!!!!!!!!!!!!!")
            -- finally clean up
            \_ ->
              finally
                (db "THREAD RELEASE START " >> releaseThread' >> db "THREAD RELEASE END")
                ( do
                    fxCompleted <- atomically $ updateStatusReturnCompleted db fx
                    db $ "FIXTURE COMPLETED: " <> txt fxCompleted
                    fs <- atomically $ readTVar fixStatus
                    db $ "FIXTURE STATUS (forkFixtureThread): " <> fixtureLabel <> " " <> txt fs
                    when
                      fxCompleted
                      do
                        logEnd
                        db "PRE PARENT HOOK RELEASE"
                        releaseParentHook
                        db "POST PARENT HOOK RELEASE"
                )

      db "III ABove Atomically"
      atomically $ do
        dbStm db "III Read thread status"
        dbStm db "III Adding thread status"
        modifyTVar activeThreads (RunningThread tfx thrdStatus :)
        sinitial <- readTVar thrdStatus
        dbStm db $ "III Thread BEFORE RUNNING SET: " <> txt sinitial
        writeTVar thrdStatus ThreadRunning
        s <- readTVar thrdStatus
        dbStm db $ "III Thread RUNNING SET I: " <> txt s
        dbStm db $ "MAIN FORK THREAD POINTER IS: " <> unsafeAddr thrdStatus
        dbStm db $ "MAIN FORK THREAD POINTER II IS: " <> unsafeAddr thrdStatus
      id' <- tfx
      db $ "III DONE forkFixtureThread: " <> txt id'

setFixtureInitialised :: Int -> TVar [PendingFixture] -> TQueue InitialisedFixture -> InitialisedFixture -> STM ()
setFixtureInitialised idx fixturesStartNext fixturesStarted newInitFixture = do
  nxtStart <- readTVar fixturesStartNext
  writeTVar fixturesStartNext $ filter (\PendingFixture {pIndex} -> pIndex /= idx) nxtStart
  writeTQueue fixturesStarted newInitFixture

runHooks :: PendingFixture -> IO InitialisedFixture
runHooks
  PendingFixture
    { pIndex,
      pFixtureLabel,
      pFixStatus,
      pLogStart,
      pIterations,
      pLogEnd,
      pReleaseParentHook
    } =
    do
      i <- pIterations
      t <- newTVarIO []
      atomically $
        readTVar i >>= writeTVar pFixStatus . either (Done . Fault "Pre-hooks failed") (const Active)
      pure $
        InitialisedFixture
          { index = pIndex,
            fixtureLabel = pFixtureLabel,
            logStart = pLogStart,
            fixStatus = pFixStatus,
            iterations = i,
            activeThreads = t,
            logEnd = pLogEnd,
            releaseParentHook = pReleaseParentHook
          }

execute' :: Executor -> Logger -> IO ()
execute'
  exe@Executor
    { maxThreads,
      threadsInUse,
      fixturesPending,
      fixturesStartNext,
      fixturesStarted
    }
  db = do
    eAvailFx <-
      atomically $ do
        eStats <- reserveThread exe db
        eitherf
          eStats
          (pure . Left . NoThreadsAvailable)
          (const $ nextFixture db fixturesPending fixturesStartNext fixturesStarted)

    let recurse = execute' exe db
        waitRecurse = C.threadDelay 10_000 >> recurse
        threadRelease = db "THREAD RELEASE" >> atomically (releaseThread threadsInUse)

    eitherf
      eAvailFx
      ( \case
          -- both the pending and active que of fixtures
          -- are empty so we are done
          -- thread was reserved so release thread
          -- has no effect because app is about to end but may
          -- later if multi-process runs are implemented and thread release implementation
          -- is changed
          EmptyQueues -> threadRelease >> db "EmptyQ - EXECUTION DONE" >> pure ()
          -- all the fixtures are not in a state to run any more threads
          -- eg being killed. We expect they may become available later of be finished
          -- and removed from the active que leading to empty ques we wait and try again
          -- thread was reserved so release thread
          NoFixturesReady -> threadRelease >> db "NoFixturesReady" >> waitRecurse
          FixtureStarting -> threadRelease >> db "FixtureStarting" >> waitRecurse
          -- all threads in use wait try again
          -- no threads reserved so none need to be released
          nt@NoThreadsAvailable {} -> db (txtPretty nt) >> waitRecurse
      )
      ( \case
          FixPending pfx@PendingFixture {pIndex} -> do
            db "@@@@ Running Hooks"
            fx <- runHooks pfx
            atomically $ setFixtureInitialised pIndex fixturesStartNext fixturesStarted fx
            forkFixtureThread db threadsInUse fx >> recurse
          FixInitialised fxInit -> forkFixtureThread db threadsInUse fxInit >> recurse
      )

qFixture :: TQueue PendingFixture -> (Int, Int -> IO PendingFixture) -> IO ()
qFixture q (idx, mkFix) = mkFix idx >>= atomically . writeTQueue q

executeLinked :: Logger -> Int -> NodeRoot o -> IO ()
executeLinked db maxThreads NodeRoot {rootStatus, children} =
  let reveseConcat :: [([a], [b])] -> ([a], [b])
      reveseConcat hfxs =
        foldl' (\(fxs, hks) (fx, hk) -> (fx <> fxs, hk <> hks)) ([], []) $ (reverse *** reverse) <$> hfxs
   in do
        db "Before fxs"
        fxsHksArr <- traverse (mkFixturesHooks db) children
        let (fxs, hks) = reveseConcat fxsHksArr
        db "After fks"
        let idxFxs = zip [0 ..] fxs

        -- create queue
        pendingQ <- newTQueueIO
        startNextQ <- newTVarIO []
        runningQ <- newTQueueIO
        -- load all fixtures to pending queue
        traverse_ (qFixture pendingQ) $ zip [0 ..] fxs
        initialThreadsInUse <- newTVarIO 0
        -- db "Before hks"
        -- hks <- concat <$> traverse hookInfo children
        -- db "After hks"

        db "Executing"
        execute' (Executor maxThreads initialThreadsInUse pendingQ startNextQ runningQ) db
        db "EXECUTION DONE !!!!!!!"

        db "Waiting on Hooks"

        let hookWait :: IO [HookRunTime] -> IO ()
            hookWait hrt' =
              do
                hrt <- hrt'
                case hrt of
                  [] -> db "HOOKS DONE" >> pure ()
                  (HookRunTime {currentStatus} : hrts) -> do
                    headStatus <- atomically $ readTVar currentStatus
                    db $ "HOOK HEAD STATUS: " <> txt headStatus
                    finalised headStatus
                      ? hookWait (pure hrts)
                      $ C.threadDelay 1_000_000 >> hookWait hrt'
        hookWait $ pure hks
        db "RUN COMPLETE !!!!!!!"

execute :: Int -> PN.PreNodeRoot o -> IO ()
execute maxThreads preRoot = do
  -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln
  chn <- newChan
  let db :: Bool -> Text -> IO ()
      db terminate msg =
        wantDebug
          ? writeChan chn (terminate, msg)
          $ pure ()

      logger :: Text -> IO ()
      logger = db False

      printDebugLogs :: IO ()
      printDebugLogs = printDebugLogs'
        where
          printDebugLogs' = do
            (terminate, msg) <- readChan chn
            putStrLn msg
            terminate
              ? pure ()
              $ printDebugLogs'

      linkExecute :: IO ()
      linkExecute = do
        root <- linkParents logger preRoot
        executeLinked logger maxThreads root
        when wantDebug $
          db True "Execution Complete"

      wantDebug = True
   in wantDebug
        ? concurrently_ printDebugLogs linkExecute
        $ linkExecute
