module SuiteRuntimeTest where

import Check (Checks)
import DSL.Interpreter
import DSL.Logger
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import GHC.Records
import Internal.SuiteRuntime
import Polysemy
import Pyrelude (IO, Text, pure, ($))
import Pyrelude.Test
import Text.Show.Pretty
import UnliftIO.Concurrent as C
  ( ThreadId,
    forkFinally,
    forkIO,
    threadDelay, myThreadId,
  )
import UnliftIO.STM

data RunEventType
  = HookStart
  | HookEnd
  | FixtureStart
  | FixtureEnd

data RunEvent
  = Boundary RunEventType ThreadId Text
  | Message ThreadId Text

logEvent :: STM (TQueue RunEvent) -> RunEvent -> IO ()
logEvent q ev = do
  q' <- atomically q
  atomically $ writeTQueue q' ev

boundary :: RunEventType -> STM (TQueue RunEvent) -> Text -> IO ()
boundary evType q msg = do
  i <- myThreadId
  logEvent q $ Boundary evType i msg

hookStart' :: STM (TQueue RunEvent) -> Text -> IO ()
hookStart' = boundary HookStart

hookEnd' :: STM (TQueue RunEvent) -> Text -> IO ()
hookEnd' = boundary HookEnd

fixtureStart' :: STM (TQueue RunEvent) -> Text -> IO ()
fixtureStart' = boundary FixtureStart

fixtureEnd' :: STM (TQueue RunEvent) -> Text -> IO ()
fixtureEnd' = boundary FixtureEnd

unit_happy_path :: IO ()
unit_happy_path =
  let logQ = newTQueue
      hookStart = hookStart' logQ
      hookEnd = hookEnd' logQ
      fixtureStart = fixtureStart' logQ
      fixtureEnd = fixtureEnd' logQ
   in 
     pure ()

-- execute $
--   Root
--     { rootStatus = newTVarIO Pending,
--       rootChildren =
--         [
--           Hook {
--             hookParent :: Node i0 i,
--     hookStatus :: IO (TVar HookStatus),
--     hook :: i -> IO o,
--     hookResult :: IO (TMVar o),
--     hookChildren :: [Node o o2],
--     hookRelease :: Int -> o -> IO ()

--           }
--         ]
--     }
