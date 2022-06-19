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
  | Fault Text SomeException
  deriving (Show)

data FixtureStatus
  = Pending
  | Active
  | Locked
  | Done CompletionStatus
  deriving (Show)

data HookStatus
  = Unintialised
  | Intitialising
  | Running
  | Complete CompletionStatus
  | Finalising
  | Finalised CompletionStatus
  deriving (Show)

data NodeStatus
  = NodePending
  | NodeStarted
  | NodeFullyRunning
  | NodeFinalising
  | NodeComplete
  deriving (Show, Eq, Ord, Bounded, Enum)

getNodeStatus :: RunGraph si so ti to -> TVar NodeStatus
getNodeStatus = \case
  RTNodeS {nodeStatus} -> nodeStatus
  RTNodeT {nodeStatus} -> nodeStatus
  RTNodeM {mNodeStatus} -> mNodeStatus
  RTFix {fixNodeStatus} -> fixNodeStatus

-- cleaningUp :: HookStatus -> Bool
-- cleaningUp = \case
--   Unintialised -> False
--   Intitialising -> False
--   Running -> False
--   Complete cs -> False
--   BeingMurdered -> True
--   Finalising -> True
--   Finalised _ -> False

-- finalised :: HookStatus -> Bool
-- finalised = \case
--   Unintialised -> False
--   Intitialising -> False
--   Running -> False
--   Complete cs -> False
--   BeingMurdered -> False
--   Finalising -> False
--   Finalised _ -> True

-- complete :: HookStatus -> Bool
-- complete = \case
--   Unintialised -> False
--   Intitialising -> False
--   Running -> False
--   Complete cs -> True
--   BeingMurdered -> False
--   Finalising -> False
--   Finalised _ -> False

-- normalCompletion :: HookStatus -> Bool
-- normalCompletion = \case
--   Unintialised -> False
--   Intitialising -> False
--   Running -> False
--   Complete cs -> case cs of
--     Normal -> True
--     Fault {} -> False
--     Murdered _ -> False
--   Finalising -> False
--   BeingMurdered -> False
--   Finalised _ -> False

isUninitialised :: HookStatus -> Bool
isUninitialised = \case
  Unintialised -> True
  Intitialising -> False
  Running -> False
  Complete cs -> False
  Finalising -> False
  Finalised _ -> False

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
      status :: TVar HookStatus,
      nodeStatus :: TVar NodeStatus,
      sHook :: si -> IO so,
      sHookRelease :: so -> IO (),
      sHookVal :: TMVar (Either SomeException so),
      sChildNode :: RunGraph so cs ti to
    } ->
    RunGraph si so ti to
  RTNodeT ::
    { label :: Loc,
      nodeStatus :: TVar NodeStatus,
      tHook :: si -> ti -> IO to,
      tHookRelease :: to -> IO (),
      tChildNode :: RunGraph si so to tc
    } ->
    RunGraph si so ti to
  RTNodeM ::
    { mlabel :: Loc,
      mNodeStatus :: TVar NodeStatus,
      childNodes :: TQueue (RunGraph si so ti to),
      fullyRunning :: TQueue (RunGraph si so ti to)
    } ->
    RunGraph si () ti ()
  RTFix ::
    { fxlabel :: Loc,
      logStart :: IO (),
      fixStatus :: TVar FixtureStatus,
      fixNodeStatus :: TVar NodeStatus,
      iterations :: TQueue (si -> ti -> IO ()),
      logEnd :: IO ()
    } ->
    RunGraph si () ti ()

lockCache :: TVar HookStatus -> STM Bool
lockCache hs = do
  s <- readTVar hs
  isUninitialised s
    ? (writeTVar hs Intitialising >> pure True)
    $ pure False

getHookVal :: TVar HookStatus -> TMVar (Either SomeException s) -> IO s -> IO (Either SomeException s)
getHookVal hs mv ios = do
  locked <- atomically $ lockCache hs
  if locked
    then
      catchAll
        ( do
            s <- ios
            let r = Right s
            atomically $ putTMVar mv r
            pure r
        )
        ( \e -> do
            let r = Left e
            atomically $ putTMVar mv r
            pure r
        )
    else atomically $ readTMVar mv

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
      ns <- newTVarIO NodePending
      case pn of
        Branch
          { bTag,
            subElms
          } -> do
            let loc = mkLoc bTag "Branch"
            idx <- newTVarIO 0
            c <- traverse (prepare' loc 0) subElms
            fr <- newTQueueIO
            q <- newTQueueIO
            atomically $ traverse_ (writeTQueue q) c
            pure $
              RTNodeM
                { mlabel = loc,
                  mNodeStatus = ns,
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
            s <- newTVarIO Unintialised
            v <- newEmptyTMVarIO
            let loc = mkLoc hookTag "SingletonHook"
            child <- prepare' loc 0 hookChild
            pure $
              RTNodeS
                { label = loc,
                  status = s,
                  nodeStatus = ns,
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
                  s <- newTVarIO Unintialised
                  v <- newEmptyTMVarIO
                  sni <- newTVarIO 0
                  fxi <- newTVarIO 0
                  child <- prepare' loc 0 threadHookChild
                  pure $
                    RTNodeT
                      { label = loc,
                        status = s,
                        nodeStatus = ns,
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
                    fixStatus = s,
                    fixNodeStatus = ns,
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

{-
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
-}

-- -- unify status
-- runThread :: maxStatus -> Logger -> RunGraph s so t to -> Int -> IO ()
-- runThread maxStatus db rg maxThreads = case rg of
--   RTNodeS loc tv f g tv' rg' -> _
--   RTNodeT loc tv f g rg' -> _
--   RTNodeM loc tv il -> _
--   RTFix loc io tv tq io' -> _

data Cached = Cached | CacheMiss deriving (Eq, Show)

data HookResult so = HookResult
  { cached :: Cached,
    value :: Either SomeException so
  }

-- returns Hookresult from cache or by executing hook,
-- updating hook and node status and storing result in cache
getSHookVal :: forall si so. (si -> IO so) -> si -> TVar HookStatus -> TVar NodeStatus -> TMVar (Either SomeException so) -> Loc -> IO (HookResult so)
getSHookVal sHook si hs ns sHookVal label = do
  mCache <- atomically readOrLock
  maybef
    mCache
    ( catchAll
        (sHook si >>= atomically . recordHookCompletion . Right)
        (atomically . recordHookCompletion . Left)
        >>= pure . HookResult CacheMiss
    )
    (pure . HookResult Cached)
  where
    readOrLock :: STM (Maybe (Either SomeException so))
    readOrLock =
      do
        s <- readTVar hs
        if isUninitialised s
          then do
            writeTVar hs Intitialising
            writeTVar ns NodeStarted
            pure Nothing
          else do
            Just <$> readTMVar sHookVal

    recordHookCompletion :: Either SomeException so -> STM (Either SomeException so)
    recordHookCompletion eso = do
      putTMVar sHookVal eso
      eitherf
        eso
        ( \e -> do
            writeTVar hs . Finalised $ Fault ("singleton hook failed: " <> unLoc label) e
            writeTVar ns NodeComplete
        )
        ( \_ -> do
            writeTVar hs . Complete $ Normal
            writeTVar ns NodeStarted
        )
      pure eso

data HasExecuted = Executed | NotExecuted deriving (Eq, Show)

data CycleState = Continue | Wait | Stop deriving (Eq, Show)

data Memoized to = Memoized {
  loading :: TVar Bool,
  parentStatus :: TVar NodeStatus,
  value :: MTVar (Either SomeException to)
} 

threadSource :: Memoized to -> (si -> ti -> IO to) -> IO ti -> IO to
threadSource Memoized {loading, parentStatus, value} = do 
  here
 where
   getOrLock :: STM (Maybe (Either SomeException to))
   getOrLock = do 
     c <- tryReadMVar value
     pure $ maybef c (
       do 
        l <- readTVar loading
        if l then
          Just <$> readMTVar value
        else 
          do 
            writeTVar loading True
            pure Nothing
      ) pure
     

executeNode :: si -> IO ti -> RunGraph si so ti to -> NodeStatus -> IO HasExecuted
executeNode si ioti rg maxStatus =
  do
    let ns = getNodeStatus rg
    nsv <- atomically $ readTVar ns
    nsv > maxStatus
      ? pure NotExecuted
      $ case rg of
        RTNodeS
          { label,
            status,
            sHook,
            sHookRelease,
            sHookVal,
            sChildNode
          } -> do
            -- getSHookVal handles exceptions and updates status of node and hook
            HookResult {cached, value} <- getSHookVal sHook si status ns sHookVal label
            let hkExecuted = cached == CacheMiss
            eitherf
              value
              (const . pure $ hkExecuted ? Executed $ NotExecuted)
              \so ->
                finally
                  (executeFully (hkExecuted ? Executed $ NotExecuted) so ioti sChildNode maxStatus)
                  (when hkExecuted $ releaseHook so status ns label sHookRelease)
        RTNodeT
          { label,
            status,
            nodeStatus,
            tHook,
            tHookRelease,
            tChildNode
          } -> do 

            

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
          { fixStatus,
            iterations
          } -> uu
  where
    executeFully :: HasExecuted -> si -> IO ti -> RunGraph si so ti to -> NodeStatus -> IO HasExecuted
    executeFully alreadyExecuted si' ioti' rg' maxStatus' =
      case cycleState maxStatus' of
        Continue ->
          executeNode si' ioti' rg' maxStatus' >>= \case
            Executed -> recurse Executed maxStatus'
            NotExecuted -> recurse NotExecuted (succ maxStatus')
        Wait -> do
          -- just keep looping until node status 
          -- is at least finalising / updating on the basis
          -- of child status on each loop
          atomically $ do
            let ns = getNodeStatus rg'
            nsv <- readTVar ns
            let cs = cycleState nsv
            unless (cs == Stop) $
              updateStatusFromChild ns rg' >> retry
          pure alreadyExecuted
        Stop -> pure alreadyExecuted
      where
        recurse :: HasExecuted -> NodeStatus -> IO HasExecuted
        recurse hasEx = executeNode si' ioti' rg'

        cycleState :: NodeStatus -> CycleState
        cycleState = \case
          NodePending -> Continue
          NodeStarted -> Continue
          NodeFullyRunning -> Wait
          NodeFinalising -> Stop
          NodeComplete -> Stop

    releaseHook :: so -> TVar HookStatus -> TVar NodeStatus -> Loc -> (so -> IO ()) -> IO ()
    releaseHook so hs ns label aftrHk =
      catchAny
        ( do
            aftrHk so
            atomically $ do
              writeTVar hs . Finalised $ Normal
              writeTVar ns NodeComplete
        )
        ( \e -> do
            atomically $ do
              writeTVar hs . Finalised $ Fault ("singleton after hook failed: " <> unLoc label) e
              writeTVar ns NodeComplete
        )

    updateStatusFromChild :: TVar NodeStatus -> RunGraph si1 so1 ti1 to1 -> STM ()
    updateStatusFromChild parentStatus child = do
      cs <- readTVar $ getNodeStatus child
      let updateParentStatus ns = modifyTVar parentStatus (\s -> s < ns ? ns $ s)
      case cs of
        NodePending -> pure ()
        NodeStarted -> pure ()
        -- if children are fully running then parent is fully running
        NodeFullyRunning -> updateParentStatus NodeFullyRunning
        NodeFinalising -> pure ()
        -- if children are complete then parent is ready to be finalised but wont be
        -- be completed until finalisation
        NodeComplete -> updateParentStatus NodeFinalising

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
