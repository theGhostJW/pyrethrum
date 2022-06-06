module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty, replicateM)
import Data.Tuple.Extra (both)
import GHC.Exts
import Internal.PreNode
  (
    Loc (Loc),
    PreNode (..),
    unLoc, PreNodeRoot, rootNode,
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

data CompletionStatus
  = Normal
  | Fault Text SomeException
  | Murdered Text
  deriving (Show)

data FixtureStatus
  = Pending
  | Starting
  | Active
  | Done CompletionStatus
  | BeingKilled
  deriving (Show)
data HookStatus
  = Unintialised
  | Intitialising
  | Running
  | Complete CompletionStatus
  | BeingMurdered
  | Finalising
  | Finalised CompletionStatus
  deriving (Show)


cleaningUp :: HookStatus -> Bool
cleaningUp = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> False
  BeingMurdered -> True
  Finalising -> True
  Finalised _ -> False

finalised :: HookStatus -> Bool
finalised = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> False
  BeingMurdered -> False
  Finalising -> False
  Finalised _ -> True

complete :: HookStatus -> Bool
complete = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> True
  BeingMurdered -> False
  Finalising -> False
  Finalised _ -> False

normalCompletion :: HookStatus -> Bool
normalCompletion = \case
  Unintialised -> False
  Intitialising -> False
  Running -> False
  Complete cs -> case cs of
    Normal -> True
    Fault {} -> False
    Murdered _ -> False
  Finalising -> False
  BeingMurdered -> False
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
      sHook :: si -> IO so,
      sHookRelease :: so -> IO (),
      sHookVal :: TMVar (Either SomeException so),
      childNodeS :: RunGraph so cs ti to
    } ->
    RunGraph si so ti to
  RTNodeT ::
    { label :: Loc,
      status :: TVar HookStatus,
      tHook :: si -> ti -> IO to,
      tHookRelease :: to -> IO (),
      childNodeT :: RunGraph si so to tc
    } ->
    RunGraph si so ti to
  RTNodeM ::
    { mlabel :: Loc,
      mstatus :: TVar HookStatus,
      childNodesM :: IdxLst (RunGraph si so ti to)
    } ->
    RunGraph si () ti ()
  RTFix ::
    { fxlabel :: Loc,
      logStart :: IO (),
      fixStatus :: TVar FixtureStatus,
      iterations :: TQueue (si -> ti -> IO ()),
      logEnd :: IO ()
    } ->
    RunGraph si () ti ()

isUninitialised :: HookStatus -> Bool
isUninitialised = \case
  Unintialised -> True
  Intitialising -> False
  Running -> False
  Complete cs -> False
  Finalising -> False
  Finalised _ -> False
  BeingMurdered -> False

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
    prepare' parentLoc subElmIdx pn =
      let mkLoc :: Maybe Text -> Text -> Loc
          mkLoc childlabel elmType =
            Loc . prfx $
              maybef
                childlabel
                (elmType <> "[" <> txt subElmIdx <> "]")
                id
            where
              prfx = ((unLoc parentLoc <> " . ") <>)
       in case pn of
            Branch
              { bTag,
                subElms
              } -> do
                let loc = mkLoc bTag "Branch"
                s <- newTVarIO Unintialised
                idx <- newTVarIO 0
                c <- traverse (prepare' loc 0) subElms
                pure $
                  RTNodeM
                    { mlabel = loc,
                      mstatus = s,
                      childNodesM = IdxLst (length c - 1) c idx
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
                      sHook = hook loc,
                      sHookRelease = hookRelease loc,
                      sHookVal = v,
                      childNodeS = child
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
                            tHook = threadHook loc,
                            tHookRelease = threadHookRelease loc,
                            childNodeT = child
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
                  atomically $ traverse_ (writeTQueue q ) $ (loc &) <$> iterations
                  pure $
                    RTFix
                      { fxlabel = loc,
                        logStart = logStart loc,
                        fixStatus = s,
                        iterations = q,
                        logEnd = logEnd loc
                      }

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
        -- root <- linkParents logger preRoot
        -- executeLinked logger maxThreads root
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
