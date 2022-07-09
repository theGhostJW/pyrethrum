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
  ( Loc (Loc),
    PreNode (..),
    PreNodeRoot,
    rootNode,
    unLoc,
  )
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
    loc :: Loc,
    action :: si -> ti -> ii -> IO ()
  }

data ExeTree si so ti to ii io where
  RTNodeS ::
    { label :: Loc,
      status :: TVar Status,
      sHook :: si -> IO so,
      sHookRelease :: so -> IO (),
      sHookVal :: TMVar (Either SomeException so),
      sChildNode :: ExeTree so cs ti to ii io
    } ->
    ExeTree si so ti to ii io
  RTNodeT ::
    { label :: Loc,
      tHook :: si -> ti -> IO to,
      tHookRelease :: to -> IO (),
      tChildNode :: ExeTree si so to tc ii io
    } ->
    ExeTree si so ti to ii io
  RTNodeI ::
    { label :: Loc,
      iHook :: si -> ti -> ii -> IO io,
      iHookRelease :: io -> IO (),
      iChildNode :: ExeTree si so to tc io iic
    } ->
    ExeTree si so ti to ii io
  RTNodeM ::
    { mlabel :: Loc,
      nStatus :: TVar Status,
      childNodes :: TQueue (ExeTree si so ti to ii io),
      fullyRunning :: TQueue (ExeTree si so ti to ii io)
    } ->
    ExeTree si () ti () ii ()
  RTFix ::
    { fxlabel :: Loc,
      logStart :: IO (),
      nStatus :: TVar Status,
      iterations :: TQueue (Iteration si ti ii),
      logEnd :: IO ()
    } ->
    ExeTree si () ti () ii ()

prepare :: PreNode () () () () () () -> IO (ExeTree () () () () () ())
prepare =
  prepare' (Loc "ROOT") 0
  where
    consNoMxIdx :: IdxLst a -> a -> IdxLst a
    consNoMxIdx l@IdxLst {lst} i = l {lst = i : lst}

    prepare' :: Loc -> Int -> PreNode o oo t to i io -> IO (ExeTree o oo t to i io)
    prepare' parentLoc subElmIdx pn = do
      ns <- newTVarIO Pending
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
        OnceHook
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
            let loc = mkLoc testTag "TestHook"
            in
              do
                child <- prepare' loc 0 testHookChild
                pure $
                    RTNodeI
                      { label = loc,
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
              let loc = mkLoc fxTag "ThreadHook"
              s <- newTVarIO Pending
              q <- newTQueueIO
              atomically $ traverse_ (writeTQueue q) $ (\(id', action) -> Iteration id' loc action) <$> M.toList iterations
              pure $
                RTFix
                  { fxlabel = loc,
                    logStart = logStart loc,
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
              P.id

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

executeNode :: si -> IO (Either SomeException ti) -> ExeTree si so ti to ii io -> IO ()
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
            tHook,
            tHookRelease,
            tChildNode
          } ->
            do
              -- create a new instance of cache for every thread
              toVal <- newEmptyTMVarIO
              status <- newTVarIO Pending
              let ts = threadSource toVal status si ioti tHook label
              finally
                (executeNode si ts tChildNode)
                do
                  -- only run clean up if hook has run
                  notRun <- atomically $ isEmptyTMVar toVal
                  unless notRun $ do
                    ethHkVal <- atomically $ readTMVar toVal
                    whenRight ethHkVal $
                      \to -> releaseHook to status label tHookRelease

        RTNodeI {iChildNode} -> uu
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
      finally
        (hkRelease ho)
        (atomically $ writeTVar ns Done)

executeGraph :: Logger -> ExeTree o oo t to ii io -> Int -> IO ()
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
        exeTree <- prepare rn
        executeGraph logger exeTree maxThreads
        when wantDebug $
          db True "Execution Complete"

      wantDebug = True
   in wantDebug
        ? concurrently_ printDebugLogs linkExecute
        $ linkExecute
