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
    PreNode (..),
    PreNodeRoot (..),
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

data RTFix s t = RTFix
  { label :: Text,
    logStart :: IO (),
    fixStatus :: TVar FixtureStatus,
    iterations :: [s -> t -> IO ()],
    logEnd :: IO ()
  }

data IdxLst a = IdxLst
  { maxIndex :: Int,
    lst :: [a],
    currIdx :: TVar Int
  }

mkIdxLst :: [a] -> STM (IdxLst a)
mkIdxLst lst = IdxLst (length lst - 1) lst <$> newTVar 0

data RTNode si so ti to = RTNode
  { label :: Text,
    status :: TVar HookStatus,
    sHook :: si -> IO so,
    sHookVal :: TVar (Either SomeException so),
    tHook :: ti -> IO to,
    fxs :: IdxLst (RTFix so to),
    subNodes :: forall cso cto. IdxLst (RTNode so cso to cto)
  }

-- data ParentInfo si so ti to = ParentInfo
--   { pilabel :: Text,
--     pistatus :: TVar HookStatus,
--     pisHook :: si -> IO so,
--     pisHookVal :: TVar (Either SomeException so),
--     pitHook :: ti -> IO to
--   }

prepare :: PreNode () () () () -> IO (RTNode () () () ())
prepare prn =
  do
    s <- newTVarIO Unintialised
    v <- newTVarIO $ Right ()
    sni <- newTVarIO 0
    fxi <- newTVarIO 0
    let r =
          RTNode
            { label = "Root",
              status = s,
              sHook = const $ pure (),
              sHookVal = v,
              tHook = const $ pure (),
              fxs = IdxLst 0 [] fxi,
              subNodes = IdxLst 0 [] sni
            }
    reverseSubElmsSetMaxIdx <$> prepare' r prn
  where
    reverseSubElmsSetMaxIdx :: RTNode a1 a2 a3 a4 -> RTNode a1 a2 a3 a4
    reverseSubElmsSetMaxIdx = uu

    consNoMxIdx :: IdxLst a -> a -> IdxLst a 
    consNoMxIdx l@IdxLst { lst } i = l { lst = i : lst }

    prepare' :: RTNode psi si pti ti -> PreNode si sci ti sco -> IO (RTNode psi si pti ti)
    prepare' rt@RTNode{fxs, subNodes} pn = case pn of
      Branch pns -> uu
      AnyHook tv f pn' tv' g -> uu
      ThreadHook f pn' g -> uu
      fx@Fixture {logStart, iterations, logEnd} -> uu
  
          -- fixsNodes :: Either (IdxLst [RTFix so to]) (IdxLst [RTNode so cso to cto])
          -- fixsNodes = \case
          --   Branch { subElms } -> uu
          --   AnyHook {hookStatus, hook, hookChild, hookResult, hookRelease} -> uu
          --   ThreadHook f pn g -> uu
          --   Fixture tv f fs g -> uu
          -- uu

--  \case
--       Branch { subElms } -> uu
--       AnyHook {hookStatus, hook, hookChild, hookResult, hookRelease} -> uu
--       ThreadHook {threadHook, threadHookChild, threadHookRelease} -> uu
--       Fixture {fixtureStatus, logStart, iterations, logEnd} -> uu

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
        -- root <- linkParents logger preRoot
        -- executeLinked logger maxThreads root
        when wantDebug $
          db True "Execution Complete"

      wantDebug = True
   in wantDebug
        ? concurrently_ printDebugLogs linkExecute
        $ linkExecute
