module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty, replicateM)
import Data.Tuple.Extra (both)
import GHC.Exts
import GHC.IO.FD (release)
import Internal.PreNode
  ( Loc (Loc),
    PreNode (..),
    PreNodeRoot,
    rootNode,
    unLoc,
  )
import LogTransformation.PrintLogDisplayElement (PrintLogDisplayElement (tstTitle))
import Polysemy.Bundle (subsumeBundle)
import Pyrelude hiding
  ( ThreadRunning,
    ThreadStatus,
    atomically,
    bracket,
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
    isEmptyTBQueue,
    newMVar,
    newTMVar,
    peekTQueue,
    pureTry,
    swapTMVar,
    tryAny,
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

data CompletionStatus
  = Normal
  | Murdered Text
  | Error Text SomeException
  deriving (Show)

data Status
  = Pending
  | HookExecuting -- only relevant to hooks
  | Running
  | FullyRunning
  | SingletonHookFinalising -- only relevant to hooks
  | Done CompletionStatus
  deriving (Show)

-- I don't want to create misleading Eq, Ord typeclasses on Status
-- where Completion status does not have a meaningful Eq, Ord
-- so use StatusEnum for comparison
data StatusEnum
  = EnumPending
  | EnumHookExecuting -- only relevant to hooks
  | EnumRunning
  | EnumFullyRunning
  | EnumSingletonHookFinalising -- only relevant to hooks
  | EnumDone
  deriving (Show, Eq, Ord)

asEnum :: Status -> StatusEnum
asEnum = \case
  Pending -> EnumPending
  HookExecuting -> EnumHookExecuting
  Running -> EnumRunning
  FullyRunning -> EnumFullyRunning
  SingletonHookFinalising -> EnumSingletonHookFinalising
  Done cs -> EnumDone

getStatusTVar :: RunGraph si so ti to -> TVar Status
getStatusTVar = \case
  RTNodeS {status} -> status
  RTNodeT {status} -> status
  RTNodeM {nStatus} -> nStatus
  RTFix {nStatus} -> nStatus

getStatus :: RunGraph si so ti to -> STM Status
getStatus = readTVar . getStatusTVar

getEnumStatus :: RunGraph si so ti to -> STM StatusEnum
getEnumStatus = fmap asEnum . getStatus

canRun :: RunGraph si so ti to -> STM Bool
canRun rg =
  canRun' <$> getStatus rg
  where
    canRun' = \case
      Pending -> True
      HookExecuting -> True
      Running -> True
      FullyRunning -> False
      SingletonHookFinalising -> False
      Done _ -> False

setStatus :: RunGraph si so ti to -> Status -> STM ()
setStatus rg = writeTVar (getStatusTVar rg)

isDone :: Status -> Bool
isDone = \case
      Pending -> False
      HookExecuting -> False
      Running -> False
      FullyRunning -> False
      SingletonHookFinalising -> False
      Done _ -> True

waitDone :: RunGraph si so ti to -> STM ()
waitDone rg = do
  s <- getStatus rg
  unless (isDone s) retry
  
data IdxLst a = IdxLst
  { maxIndex :: Int,
    lst :: [a],
    currIdx :: TVar Int
  }

mkIdxLst :: a -> STM (IdxLst a)
mkIdxLst elm = IdxLst 0 [elm] <$> newTVar 0

data RunGraph si so ti to where
  RTNodeS ::
    { label :: Loc,
      status :: TVar Status,
      sHook :: si -> IO so,
      sHookRelease :: so -> IO (),
      sHookVal :: TMVar (Either SomeException so),
      sChildNode :: RunGraph so cs ti to
    } ->
    RunGraph si so ti to
  RTNodeT ::
    { label :: Loc,
      status :: TVar Status,
      tHook :: si -> ti -> IO to,
      tHookRelease :: to -> IO (),
      tChildNode :: RunGraph si so to tc
    } ->
    RunGraph si so ti to
  RTNodeM ::
    { mlabel :: Loc,
      nStatus :: TVar Status,
      childNodes :: TQueue (RunGraph si so ti to),
      fullyRunning :: TQueue (RunGraph si so ti to)
    } ->
    RunGraph si () ti ()
  RTFix ::
    { fxlabel :: Loc,
      logStart :: IO (),
      nStatus :: TVar Status,
      iterations :: TQueue (si -> ti -> IO ()),
      logEnd :: IO ()
    } ->
    RunGraph si () ti ()

prepare :: PreNode () () () () -> IO (RunGraph () () () ())
prepare =
  prepare' (Loc "ROOT") 0
  where
    -- reverse lists that were generated with cons
    correctSubElms :: forall s so t to. RunGraph s so t to -> RunGraph s so t to
    correctSubElms = uu

    consNoMxIdx :: IdxLst a -> a -> IdxLst a
    consNoMxIdx l@IdxLst {lst} i = l {lst = i : lst}

    prepare' :: Loc -> Int -> PreNode s so t to -> IO (RunGraph s so t to)
    prepare' parentLoc subElmIdx pn = do
      ns <- newTVarIO Pending
      done :: TMVar CompletionStatus <- newEmptyTMVarIO
      case pn of
        Branch
          { bTag,
            subElms
          } -> do
            let loc = mkLoc bTag "Branch"
            idx <- newTVarIO 0
            fr <- newTQueueIO
            q <- newTQueueIO
            -- load the queue
            c <- traverse (prepare' loc 0) subElms
            atomically $ traverse_ (writeTQueue q) c
            pure $
              RTNodeM
                { mlabel = loc,
                  nStatus = ns,
                  childNodes = q,
                  fullyRunning = fr
                }
        AnyHook
          { hookTag,
            hook,
            hookChild,
            hookResult,
            hookRelease
          } -> do
            s <- newTVarIO Pending
            v <- newEmptyTMVarIO
            let loc = mkLoc hookTag "SingletonHook"
            child <- prepare' loc 0 hookChild
            pure $
              RTNodeS
                { label = loc,
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
            let loc = mkLoc threadTag "ThreadHook"
             in do
                  v <- newEmptyTMVarIO
                  sni <- newTVarIO 0
                  fxi <- newTVarIO 0
                  child <- prepare' loc 0 threadHookChild
                  pure $
                    RTNodeT
                      { label = loc,
                        status = ns,
                        tHook = threadHook loc,
                        tHookRelease = threadHookRelease loc,
                        tChildNode = child
                      }
        Fixture
          { fxTag,
            logStart,
            iterations,
            logEnd
          } ->
            do
              let loc = mkLoc fxTag "ThreadHook"
              s <- newTVarIO Pending
              q <- newTQueueIO
              atomically $ traverse_ (writeTQueue q) $ (loc &) <$> iterations
              pure $
                RTFix
                  { fxlabel = loc,
                    logStart = logStart loc,
                    -- fixStatus = s,
                    nStatus = ns,
                    iterations = q,
                    logEnd = logEnd loc
                  }
      where
        mkLoc :: Maybe Text -> Text -> Loc
        mkLoc childlabel elmType =
          Loc . ((unLoc parentLoc <> " . ") <>) $
            maybef
              childlabel
              (elmType <> "[" <> txt subElmIdx <> "]")
              id

type Logger = Text -> IO ()

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
      asEnum s == EnumPending
        ? (writeTVar hs HookExecuting >> pure Nothing)
        $ (Just <$> readTMVar hkVal)

    setHookStatus :: Either SomeException ho -> STM (Either SomeException ho)
    setHookStatus eso = do
      putTMVar hkVal eso
      eso
        & either
          (writeTVar hs . Done . Error ("Hook failed " <> unLoc loc))
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

failRecursively :: forall si so ti to. Text -> SomeException -> RunGraph si so ti to -> STM ()
failRecursively msg e = recurse
  where
    failure :: Status
    failure = Done $ Error msg e

    failQ :: TQueue (RunGraph si' so' ti' to') -> STM ()
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

    recurse :: RunGraph si' so' ti' to' -> STM ()
    recurse rg = do
      setStatus rg failure
      case rg of
        RTNodeS {sChildNode} -> recurse sChildNode
        RTNodeT {tChildNode} -> recurse tChildNode
        RTNodeM {childNodes, fullyRunning} -> do
          failQ childNodes
          failQ fullyRunning
        RTFix {iterations} -> emptyQ iterations

executeNode :: si -> IO (Either SomeException ti) -> RunGraph si so ti to -> IO ()
executeNode si ioti rg =
  do
    wantRun <- atomically $ canRun rg
    when
      wantRun
      case rg of
        RTNodeS
          { label,
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
              HookResult {hasExecuted, value = ethHkRslt} <- hookVal sHook si status sHookVal label
              if hasExecuted
                then
                  ethHkRslt -- the hook that executes waits for child completion and sets status
                    & either
                      ( \e ->
                          atomically $
                            -- status already set by hookVal
                            failRecursively ("Parent hook failed: " <> unLoc label) e sChildNode
                      )
                      ( \so ->
                          finally
                            (executeNode so ioti sChildNode)
                            ( do
                                atomically $ waitDone sChildNode
                                releaseHook so status label sHookRelease
                            )
                      )
                else
                  ethHkRslt
                    & either
                      (const $ pure ())
                      (\so -> executeNode so ioti sChildNode)
        RTNodeT
          { label,
            status,
            tHook,
            tHookRelease,
            tChildNode
          } ->
            do
              -- create a new instance of cache for every thread
              toVal <- newEmptyTMVarIO
              let ts = threadSource toVal status si ioti tHook label
              finally
                (executeNode si ts tChildNode)
                ( do
                    -- only run clean up if hook has run
                    notRun <- atomically $ isEmptyTMVar toVal
                    unless notRun $ do
                      ethHkVal <- atomically $ readTMVar toVal
                      whenRight ethHkVal $
                         \to -> releaseThredHook to status label tHookRelease

                    atomically $ do 

                      
                      
                      -- releaseHook cachedHookResult status label tHookRelease
                      -- deal with no iterations run
                      -- no need to releasew hook 
                      -- update status to done


                )
        RTNodeM
          { childNodes
          } -> uu
        --  do atomically $ do
        --   ns <- readTVar nodeStatus
        --   qmt <- isEmptyTQueue childNodesM
        --   -- assume subnodes only evicted from q when done so and
        --   -- empty q means sub nodes are done hence parent node is deemed complete
        --   qmt ? pure NodeComplete $ do
        --      -- fixtures shuffled to back of queue when started so if any subnode
        --      -- is pending the status will be pending
        --      sn <- peekTQueue childNodesM

        -- atomically $ do
        --   qmt <- atomically $ isEmptyTQueue iterations
        RTFix
          { nStatus,
            iterations
          } -> uu
  where
    releaseHook :: ho -> TVar Status -> Loc -> (ho -> IO ()) -> IO ()
    releaseHook ho ns label hkRelease =
      catchAll
        (hkRelease ho >> atomically (writeTVar ns $ Done Normal))
        $ atomically . writeTVar ns . Done . Error ("singleton after hook failed: " <> unLoc label)

    releaseThreadHook :: ho -> TVar Status -> Loc -> (ho -> IO ()) -> IO ()
    releaseThreadHook ho ns label hkRelease =
      catchAll
        (hkRelease ho) -- don't update status might be another thread running
        -- if clean up fails branch is deemed done even if other threads are running
        $ atomically . writeTVar ns . Done . Error ("thread after hook failed: " <> unLoc label) 

executeGraph :: Logger -> RunGraph s so t to -> Int -> IO ()
executeGraph db rg maxThreads = uu

execute :: Int -> PreNodeRoot -> IO ()
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
        rn <- rootNode preRoot
        runGraph <- prepare rn
        executeGraph logger runGraph maxThreads
        when wantDebug $
          db True "Execution Complete"

      wantDebug = True
   in wantDebug
        ? concurrently_ printDebugLogs linkExecute
        $ linkExecute

{-
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
-}
