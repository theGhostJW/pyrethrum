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
    unLoc, A,
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
  { label :: Loc,
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
    { label :: Loc,
      status :: TVar HookStatus,
      fxsM :: IdxLst (RTFix so to),
      childNodesM :: IdxLst (RTNode si so ti to)
    } ->
    RTNode si so ti to

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

prepare :: PreNode A () () () () -> IO (RTNode () () () ())
prepare =
  prepare' "ROOT" 0
  where
    -- reverse lists that were generated with cons
    correctSubElms :: forall s so t to. RTNode s so t to -> RTNode s so t to
    correctSubElms = uu

    consNoMxIdx :: IdxLst a -> a -> IdxLst a
    consNoMxIdx l@IdxLst {lst} i = l {lst = i : lst}

    prepare' :: Loc -> Int -> PreNode a s so t to -> IO (RTNode s so t to)
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
            Branch {subElms} -> uu
            AnyHook
              { hookTag,
                hook,
                hookChild,
                hookResult,
                hookRelease
              } -> do
                s <- newTVarIO Unintialised
                v <- newEmptyTMVarIO
                sni <- newTVarIO 0
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
              } -> do 
            -- let loc = mkLoc threadTag "ThreadHook"
            --  in do
            --       s <- newTVarIO Unintialised
            --       v <- newEmptyTMVarIO
            --       sni <- newTVarIO 0
            --       fxi <- newTVarIO 0
            --       let nxtSubNode :: RTNode s so t cti
            --           nxtSubNode =
            --             RTNode
            --               { label = loc,
            --                 status = s,
            --                 sHook = sHook,
            --                 sHookRelease = sHookRelease,
            --                 sHookVal = v,
            --                 tHook = \ti -> tHook ti >>= threadHook loc,
            --                 tHookRelease = threadHookRelease loc,
            --                 fxs = IdxLst 0 [] fxi,
            --                 subNodes = IdxLst 0 [] sni
            --               }
            --       {-
            --         • Couldn't match type ‘cs’ with ‘s’
            --           Expected: PreNode s so2 ct to2
            --             Actual: PreNode cs so2 ct to2
            --           ‘cs’ is a rigid type variable bound by
            --       -}
            --       -- RTNode s t -> Int -> PreNode s cs t ct -> IO (RTNode s t)
            --       -- RTNode s ct      --  PreNode cs so2 ct to2
            --       sn <- prepare' nxtSubNode (subElmIdx + 1) threadHookChild
            --       il <- atomically $ mkIdxLst sn
            --       {-
            --           • Couldn't match type ‘s’ with ‘so’
            --             Expected: IdxLst (RTNode so cs1 to ct1)
            --             Actual:   IdxLst (RTNode s so t ct)
            --       -}
            --       pure $
            --         rt
            --           { subNodes = il
            --           }
            Fixture
              { fxTag,
                logStart,
                iterations,
                logEnd
              } -> uu

-- do
-- s <- newTVarIO Pending
-- let loc = mkLoc fxTag "Fixture"
--     fxs' =
--       consNoMxIdx fxs $
--         RTFix
--           { label = loc,
--             fixStatus = s,
--             iterations = (loc &) <$> iterations,
--             logStart = logStart loc,
--             logEnd = logEnd loc
--           }
-- pure $ rt {fxs = fxs'}

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

execute :: Int -> PreNode A () () () () -> IO ()
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
