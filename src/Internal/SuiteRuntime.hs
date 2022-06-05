module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty)
import Data.Tuple.Extra (both)
import GHC.Exts
import Internal.PreNode
  ( CompletionStatus (..),
    FixtureStatus (..),
    HookStatus (..),
    Loc (Loc),
    PreNode (..),
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

data IdxLst a = IdxLst
  { maxIndex :: Int,
    lst :: [a],
    currIdx :: TVar Int
  }

mkIdxLst :: a -> STM (IdxLst a)
mkIdxLst elm = IdxLst 0 [elm] <$> newTVar 0

data RTNode si so ti to where
  RTNodeS ::
    { label :: Loc,
      status :: TVar HookStatus,
      sHook :: si -> IO so,
      sHookRelease :: so -> IO (),
      sHookVal :: TMVar (Either SomeException so),
      childNodeS :: RTNode so cs ti to
    } ->
    RTNode si so ti to
  RTNodeT ::
    { label :: Loc,
      status :: TVar HookStatus,
      tHook :: si -> ti -> IO to,
      tHookRelease :: to -> IO (),
      childNodeT :: RTNode si so to tc
    } ->
    RTNode si so ti to
  RTNodeM ::
    { mlabel :: Loc,
      mstatus :: TVar HookStatus,
      childNodesM :: IdxLst (RTNode si so ti to)
    } ->
    RTNode si () ti ()
  RTFix ::
    { fxlabel :: Loc,
      logStart :: IO (),
      fixStatus :: TVar FixtureStatus,
      iterations :: [si -> ti -> IO ()],
      logEnd :: IO ()
    } ->
    RTNode si () ti ()

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

prepare :: PreNode () () () () -> IO (RTNode () () () ())
prepare =
  prepare' (Loc "ROOT") 0
  where
    -- reverse lists that were generated with cons
    correctSubElms :: forall s so t to. RTNode s so t to -> RTNode s so t to
    correctSubElms = uu

    consNoMxIdx :: IdxLst a -> a -> IdxLst a
    consNoMxIdx l@IdxLst {lst} i = l {lst = i : lst}

    prepare' :: Loc -> Int -> PreNode s so t to -> IO (RTNode s so t to)
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
            Branch {
              bTag,
              subElms} -> do 
              let loc = mkLoc bTag "Branch"
              s <- newTVarIO Unintialised
              idx <- newTVarIO  0
              c <- traverse (prepare' loc 0) subElms
              pure $  RTNodeM {
                mlabel = loc,
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
                  pure $
                    RTFix
                      { fxlabel = loc,
                        logStart = logStart loc,
                        fixStatus = s,
                        iterations = (loc &) <$> iterations,
                        logEnd = logEnd loc
                      }



execute :: Int -> PreNode () () () () -> IO ()
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
        -- root <- linkParents logger preRoot
        -- executeLinked logger maxThreads root
        when wantDebug $
          db True "Execution Complete"

      wantDebug = True
   in wantDebug
        ? concurrently_ printDebugLogs linkExecute
        $ linkExecute
