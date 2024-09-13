module HigherOrderEffectDemo where

import Effectful as EF
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    runEff,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( interpret,
    localSeqUnlift,
    localSeqUnliftIO,
    send,
  )
import GHC.Clock (getMonotonicTime)

--  ################### Profiling Example ##################

-- profiling demo (from eff docs)
-- timing steps
-- inline run with page
data Profiling :: Effect where
  Profile :: String -> m a -> Profiling m a

type instance DispatchOf Profiling = Dynamic

profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
profile label action' = send (Profile label action')

runProfiling :: (IOE :> es) => Eff (Profiling : es) a -> Eff es a
runProfiling = interpret $ \env -> \case
  Profile label action' -> localSeqUnliftIO env $ \unlift -> do
    t1 <- getMonotonicTime
    r <- unlift action'
    t2 <- getMonotonicTime
    putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
    pure r

runNoProfiling :: Eff (Profiling : es) a -> Eff es a
runNoProfiling = interpret $ \env -> \case
  Profile _label action' -> localSeqUnlift env $ \unlift -> unlift action'

actionP :: forall es. (Profiling :> es, IOE :> es) => Eff es ()
actionP = profile "greet" . liftIO $ putStrLn "Hello!"

profileAction :: IO ()
profileAction = runEff . runProfiling $ actionP

-- >>> profileAction
