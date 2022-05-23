module Internal.SuiteRuntime where

import Check (CheckReport (result))
import Control.DeepSeq (NFData, deepseq, force, ($!!))
import Data.Function (const, ($), (&))
import Data.Sequence (Seq (Empty), empty)
import Data.Tuple.Extra (both)
import GHC.Exts
import Internal.PreNode
  ( CompletionStatus (..),
    HookStatus (..),
    PreNode (..),
    PreNodeRoot (..),
    FixtureStatus(..),

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


data RTFix s t = RTFix {
    fixtureLabel :: Text,
    logStart :: IO (),
    fixStatus :: TVar FixtureStatus,
    iterations :: [s -> t -> IO ()],
    logEnd :: IO ()
}

data IdxLst a = IdxLst {
  maxIndex :: Int,
  list :: [a],
  currIdx :: TVar Int
}

mkIdxLst :: [a] -> STM (IdxLst a)
mkIdxLst lst = IdxLst  (length lst - 1) lst <$> newTVar 0

data RTNode si so ti to = RTNode {
  fixtureLabel :: Text,
  status :: TVar HookStatus,
  sHook :: si -> IO so,
  sHookVal :: TVar (Either SomeException so),
  tHook:: ti -> IO to,
  fxs :: IdxLst [RTFix so to],
  subNodes :: forall cso cto. IdxLst [RTNode so cso to cto]
}

-- fixsNodes :: PreNode si so ti to -> (IdxLst [RTFix so to], IdxLst [RTNode so cso to cto])
-- fixsNodes = \case
--   Branch txt pns -> _
--   AnyHook txt tv f pn tv' g -> _
--   ThreadHook txt f pn g -> _
--   Fixture txt tv io fs io' -> _


-- prepare :: PreNode () () () () -> IO (RTNode () () () ())
-- prepare pn = 
--   do 
--     s <- newTVarIO Unintialised
--     v <- newTVarIO 
--     prepare' pn
--   where 
--     prepare' :: RTNode si so ti to -> PreNode so sci to sco -> RTNode si so ti to 
--     prepare' parent pn =  
--   \case
--   Branch txt pns -> _
--   AnyHook txt tv f pn tv' g -> _
--   ThreadHook txt f pn g -> _
--   Fixture txt tv io fs io' -> _


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
