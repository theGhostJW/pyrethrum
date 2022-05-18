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
    FixtureStatus (..),
    HookStatus (Finalised, Finalising),
    finalised,
  )
import qualified Internal.PreNode as PN
  ( CompletionStatus (Fault, Murdered, Normal),
    HookStatus (..),
    PreNode (..),
    PreNodeRoot (..),
  )
import LogTransformation.PrintLogDisplayElement (PrintLogDisplayElement (tstTitle))
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

data NodeRoot = NodeRoot
  { rootStatus :: TVar PN.HookStatus,
    rootNode :: Node () () () ()
  }

data Node si so ti to where
  Branch ::
    { branchLabel :: Text,
      branchStatus :: TVar FixtureStatus,
      branchIn :: Either si (Node pi si tpi ti),
      subElms :: IO [Node si so ti to]
    } ->
    Node si () ti ()
  SingletonHook ::
    { hookLabel :: Text,
      hookStatus :: TVar PN.HookStatus,
      hook :: si -> IO so,
      hookResult :: TMVar (Either SomeException so),
      singletonHookChild :: IO (Node so co to cto),
      singletonHookRelease :: so -> IO ()
    } ->
    Node si so ti to
  ThreadHook ::
    { hookLabel :: Text,
      threadHook:: ti -> IO to,
      hookChild :: IO (Node si so to to2),
      threadHookRelease :: to -> IO ()
    } ->
    Node si so ti to
  Fixture ::
    { fixtureLabel :: Text,
      logStart :: IO (),
      fixStatus :: TVar FixtureStatus,
      iterations :: [si -> ti -> IO ()],
      logEnd :: IO ()
    } ->
    Node si () ti ()


data RTFix s t = RTFix {
    fixtureLabel :: Text,
    logStart :: IO (),
    fixStatus :: TVar FixtureStatus,
    iterations :: [s -> t -> IO ()],
    logEnd :: IO ()
}
data RTNode si so ti to = RTNode {
  fixtureLabel :: Text,
  status :: TVar PN.HookStatus,
  maxIdx :: Int,
  lastIdx :: TVar Int,
  fxs :: [RTFix so to],
  subNodes :: forall cso cto. [RTNode so cso to cto]
} 

data HookRunTime = HookRunTime
  { address :: Text,
    currentStatus :: TVar HookStatus
  }

hookInfo :: Node i o e f -> IO [HookRunTime]
hookInfo =
  hookInfo' (pure [])
  where
    hookInfo' :: IO [HookRunTime] -> Node a b c d -> IO [HookRunTime]
    hookInfo' accum node = do
      accum' <- accum
      case node of
        Branch {subElms} -> do
          c <- subElms >>= traverse (hookInfo' accum)
          pure $ concat c
        SingletonHook
          { hookLabel,
            hookStatus,
            hook,
            hookResult,
            singletonHookChild,
            singletonHookRelease
          } ->
            let me = HookRunTime hookLabel hookStatus
             in do
                  c <- singletonHookChild >>= hookInfo' accum
                  pure $ me : c
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

nodeFinished :: Logger -> Node i o c d -> IO Bool
nodeFinished db =
  let log :: Show a => Text -> STM a -> STM a
      log = logSTMVal db
   in \case
        Branch {subElms} -> do
          allDone <- subElms >>= traverse (nodeFinished db)
          pure $ all id allDone
        SingletonHook {hookStatus} -> atomically $ finalised <$> log "SingletonHook Finished Status" (readTVar hookStatus)
        Fixture {fixStatus, fixtureLabel} -> atomically $ isDone <$> log ("FIXTURE STATUS: (nodeFinished): - " <> fixtureLabel <> " ") (readTVar fixStatus)

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

recurseHookRelease :: Logger -> Node i o c d -> IO ()
recurseHookRelease db n =
  do
    -- crawls up tree releasing all AnyHooks for which sub- elements have been completed
    db "!!!!!!!! recurseHookRelease !!!!!!!!"
    case n of
      b@Branch {branchLabel, branchIn, subElms} -> do
        se <- subElms
        seDone <- nodeFinished db b
        seDone
          ? pure ()
          $ eitherf
            branchIn
            (const $ pure ())
            (recurseHookRelease db)
      Fixture {} -> db "!!!!!!!! recurseHookRelease: Fixture !!!!!!!!" >> pure ()
      hk@SingletonHook {hookResult, hookStatus, singletonHookChild, singletonHookRelease, singletonHookIn = parent} ->
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
                  hc <- singletonHookChild
                  done <- nodeFinished db hc
                  if not done
                    then do
                      db "!!!!!!!! recurseHookRelease: oldStatus children not done !!!!!!!!"
                      setStatus oldStatus
                    else do
                      db "READING HOOK RESULT"
                      ehr <- atomically $ readTMVar hookResult
                      db "FINISHED READING HOOK RESULT"
                      eitherf
                        ehr
                        ( \e -> do
                            db ("SingletonHook release not executed because hook execution failed - this code should never run\n" <> txt e)
                            finaliseRecurse $ PN.Fault "SingletonHook execution failed" e
                        )
                        ( \hr -> do
                            ethr <- tryAny $ singletonHookRelease hr
                            eitherf
                              ethr
                              ( \e -> do
                                  db ("SingletonHook release threw an exception\n" <> txt e)
                                  finaliseRecurse $ PN.Fault "SingletonHook release threw an exception" e
                              )
                              (const $ finaliseRecurse PN.Normal)
                        )

loadFixture :: forall i o ti to. Logger -> Text -> Either o (Node i o ti to) -> [o -> to -> IO ()] -> TVar FixtureStatus -> IO () -> IO () -> (Int -> IO PendingFixture)
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

linkParents :: Logger -> PN.PreNodeRoot -> IO NodeRoot
linkParents db PN.PreNodeRoot {rootNode} =
  do
    db "CALLING LINKED PARENTS"
    status <- newTVarIO PN.Unintialised

    rootNode' <- rootNode
    root <- (linkParents' db $ Left ()) rootNode'
    pure $
      NodeRoot
        { rootStatus = status,
          rootNode = root
        }

linkParents' :: forall o i ti to o' to'. Logger -> Either o (Node i o ti to) -> PN.PreNode o o' to to' -> IO (Node o o' to to')
linkParents' db parent preNode =
  do
    db "!!!!!!!! CALLING linkParents' (PRIME) !!!!! "
    case preNode of
      PN.AnyHook {hookAddress, hook, hookStatus, hookResult, hookChild, hookRelease} -> do
        let h :: Node o o' to to'
            h =  Internal.SuiteRuntime.SingletonHook
                { hookLabel = hookAddress,
                  singletonHookIn = parent,
                  hookStatus = hookStatus,
                  hook = hook,
                  hookResult = hookResult,
                  singletonHookChild = linkParents' db (Right h) hookChild,
                  singletonHookRelease = hookRelease
                }
         in pure h
      PN.ThreadHook
        { threadHookAddress, -- used in testing
          threadHook,
          threadHookChild,
          threadHookRelease
        } -> uu
      PN.Branch {branchAddress, subElms} ->
        pure $
          Branch
            { branchLabel = branchAddress,
              branchIn = parent,
              subElms = traverse (linkParents' db parent) subElms
            }
      PN.Fixture
        { fixtureAddress,
          fixtureStatus,
          logStart,
          iterations,
          logEnd
        } ->
          pure $
            Internal.SuiteRuntime.Fixture
              { fixtureLabel = fixtureAddress,
                logStart = logStart,
                singletonIn = parent,
                fixStatus = fixtureStatus,
                iterations = uu, -- iterations,
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
executeHook :: Logger -> Node i o ti to -> IO ()
executeHook db =
  \case
    Branch {} -> pure ()
    Fixture {} -> pure ()
    SingletonHook
      { singletonHookIn,
        hookStatus,
        hook,
        hookResult,
        singletonHookChild 
      } -> do
        -- up to here need stus update pre and post execute hook
        eInput <- db "CALL PARENT LOCK EXECUTE HOOK" >> lockExecuteHook db singletonHookIn
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
              (PN.Fault "SingletonHook execution failed")
              (const PN.Normal)
            $ result

{-
 Branch ::
    { branchLabel :: Text,
      singletonIn :: Either i (Node pi i),
      subElms :: IO [Node i o]
    } ->
    Node i o
-}
lockExecuteHook :: Logger -> Either o (Node i o ti to) -> IO (Either SomeException o)
lockExecuteHook db parent =
  eitherf
    parent
    (\o -> db "NO PARENT HOOK RETURNING VALUE" >> pure (Right o))
    ( \case
        Branch {} -> db "hook lock - Branch RETURNING parent" >> pure (Right ())
        Fixture {} -> db "hook lock - FIXTURE RETURNING PURE" >> pure (Right ())
        hk@SingletonHook
          { hookStatus,
            hookResult,
            singletonHookRelease
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

mkFixturesHooks :: Logger -> Node i o ti to -> IO ([Int -> IO PendingFixture], [HookRunTime])
mkFixturesHooks db n =
  reveseBoth <$> recurse (pure ([], [])) n
  where
    reveseBoth :: ([a], [b]) -> ([a], [b])
    reveseBoth (fxs, hks) = (reverse fxs, reverse hks)

    recurse :: IO ([Int -> IO PendingFixture], [HookRunTime]) -> Node i o ti to -> IO ([Int -> IO PendingFixture], [HookRunTime])
    recurse accum node = do
      (fxs, hks) <- accum
      case node of
        Branch {subElms} -> subElms >>= foldl' recurse accum
        SingletonHook
          { hookLabel,
            hookStatus,
            singletonHookChild 
          } -> singletonHookChild >>= recurse (pure (fxs, HookRunTime hookLabel hookStatus : hks))
        Fixture
          { fixtureLabel,
            logStart,
            singletonIn,
            iterations,
            fixStatus,
            logEnd
          } -> do
            let fx = loadFixture db fixtureLabel singletonIn iterations fixStatus logStart logEnd
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

runFixture :: Logger -> TVar ThreadStatus -> IO InitialisedFixture -> IO InitialisedFixture
runFixture
  db
  threadStatus
  ioFx =
    do
      fx@InitialisedFixture {logStart, fixStatus, iterations, activeThreads, logEnd} <- ioFx
      let runIterations = do
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
      db "RF - BODY"
      db $ "RF - BODY THREAD POINTER IS: " <> unsafeAddr threadStatus
      s <- readTVarIO threadStatus
      db $ "RF - BODY THREAD STATUS: " <> txt s
      case s of
        ThreadInitialising _ -> C.threadDelay 1_000 >> runFixture db threadStatus ioFx
        ThreadRunning -> runIterations >> pure fx
        ThreadDone -> pure fx

-- Any is a type to which any type can be safely unsafeCoerced to.

-- A datatype that has the same layout as Word and so can be casted to it.
newtype Ptr' a = Ptr' a

-- Any is a type to which any type can be safely unsafeCoerced to.
aToWord# :: GHC.Exts.Any -> Word#
aToWord# a = let !mb = Ptr' a in case unsafeCoerce# mb :: Word of W# addr -> addr

unsafeAddr :: a -> Text
unsafeAddr a = txt $ I# (word2Int# (aToWord# (unsafeCoerce# a)))

forkFixtureThread :: Logger -> TVar [RunningThread] -> TVar Int -> IO InitialisedFixture -> IO ()
forkFixtureThread
  db
  activThrds
  threadsInUse
  iofx =
    do
      db "III forkFixtureThread started"
      thrdStatus <- newTVarIO (ThreadInitialising 999)
      -- fx@InitialisedFixture {activeThreads, logEnd, releaseParentHook, fixtureLabel, fixStatus} <- iofx

      let releaseThread' :: IO ()
          releaseThread' =
            atomically $ do
              -- set this threadstatus to done
              writeTVar thrdStatus ThreadDone
              -- remove all finished threads from active threads list
              ats <- readTVar activThrds
              newAts <- removeFinishedThreads ats
              writeTVar activThrds newAts
              -- decrement global threads in use
              upre <- readTVar threadsInUse
              dbStm db "THREAD DONE"
              dbStm db $ "BEFORE DECREMENT THREAD: " <> txt upre
              releaseThread threadsInUse
              u <- readTVar threadsInUse
              dbStm db $ "AFTER DECREMENT THREAD: " <> txt u

          tfx =
            C.forkFinally
              do
                db "@@@@ FIXTURE START"
                fx <- runFixture db thrdStatus iofx
                db "@@@@@ FIXTURE END !!!!!!!!!!!!!!!!!!!"
                pure fx
              -- clean up - at this point it is assumed that run fixture handles ALL
              -- exceptions and a Left will NEVER be returned
              ( either
                  throw
                  \fx@InitialisedFixture {logEnd, releaseParentHook, fixtureLabel, fixStatus} ->
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
              )

      db "III ABove Atomically"
      atomically $ do
        dbStm db "III Read thread status"
        dbStm db "III Adding thread status"
        modifyTVar activThrds (RunningThread tfx thrdStatus :)
        sinitial <- readTVar thrdStatus
        dbStm db $ "III Thread BEFORE RUNNING SET: " <> txt sinitial
        writeTVar thrdStatus ThreadRunning
        s <- readTVar thrdStatus
        dbStm db $ "III Thread RUNNING SET I: " <> txt s
        dbStm db $ "MAIN FORK THREAD POINTER IS: " <> unsafeAddr thrdStatus
        dbStm db $ "MAIN FORK THREAD POINTER II IS: " <> unsafeAddr thrdStatus
      id' <- tfx
      db $ "III DONE forkFixtureThread: " <> txt id'

updateFixtureQus :: Int -> TVar [PendingFixture] -> TQueue InitialisedFixture -> InitialisedFixture -> STM ()
updateFixtureQus idx fixturesStartNext fixturesStarted newInitFixture = do
  nxtStart <- readTVar fixturesStartNext
  writeTVar fixturesStartNext $ filter (\PendingFixture {pIndex} -> pIndex /= idx) nxtStart
  writeTQueue fixturesStarted newInitFixture

runHooks :: PendingFixture -> TVar [RunningThread] -> IO InitialisedFixture
runHooks
  PendingFixture
    { pIndex,
      pFixtureLabel,
      pFixStatus,
      pLogStart,
      pIterations,
      pLogEnd,
      pReleaseParentHook
    }
  rts =
    do
      -- implicitly runs hyooks
      i <- pIterations
      atomically $
        -- reset the status to pending if iterations have been loaded successfully it will be set to running
        -- it will be set to running when the first iteration is run
        readTVar i >>= writeTVar pFixStatus . either (Done . Fault "Pre-hooks failed") (const Pending)
      pure $
        InitialisedFixture
          { index = pIndex,
            fixtureLabel = pFixtureLabel,
            logStart = pLogStart,
            fixStatus = pFixStatus,
            iterations = i,
            activeThreads = rts,
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
          FixPending pfx@PendingFixture {pIndex} ->
            do
              activThrds <- newTVarIO []
              let loadHook :: IO InitialisedFixture
                  loadHook = do
                    db "@@@@ Running Hooks"
                    fx <- runHooks pfx activThrds
                    atomically $ updateFixtureQus pIndex fixturesStartNext fixturesStarted fx
                    pure fx
              forkFixtureThread db activThrds threadsInUse loadHook >> recurse
          FixInitialised fxInit@InitialisedFixture {activeThreads} -> forkFixtureThread db activeThreads threadsInUse (pure fxInit) >> recurse
      )

qFixture :: TQueue PendingFixture -> (Int, Int -> IO PendingFixture) -> IO ()
qFixture q (idx, mkFix) = mkFix idx >>= atomically . writeTQueue q

executeLinked :: Logger -> Int -> NodeRoot -> IO ()
executeLinked db maxThreads NodeRoot {rootStatus, rootNode} =
  do
    db "Before fxs"
    (fxs, hks) <- mkFixturesHooks db rootNode
    db "After fks"

    -- create queue
    pendingQ <- newTQueueIO
    startNextQ <- newTVarIO []
    runningQ <- newTQueueIO

    -- load all fixtures to pending queue
    traverse_ (qFixture pendingQ) $ zip [0 ..] fxs
    initialThreadsInUse <- newTVarIO 0

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

execute :: Int -> PN.PreNodeRoot -> IO ()
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
