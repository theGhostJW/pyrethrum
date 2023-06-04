module Internal.ExeNode where

import Internal.SuiteRuntime
import DSL.LoggerPsy (Logger (LogError), log, logError, logItem)
import Internal.RunnerBaseLazy
import qualified RunElementClasses as RC
import Polysemy (Member, Sem)
import Data.Set as S
import Data.Aeson as A (ToJSON (toJSON), Value (Bool))
import RunElementClasses as C
  ( Address,
    AddressElem,
    AddressElemType,
    AddressedElm (AddressedElm, element),
    Config,
    HasId,
    HasTitle,
    ItemClass,
    TestFilterResult (..),
    TestLogInfo (..),
    mkTestLogInfo,
    push,
    render,
    rootAddress,
    unAddress,
  )


{-
-- TODO - Error handling especially outside tests eg. in hooks
-- separate
loadNode ::
  forall hi e effs.
  (ToJSON e, Show e, Member (Logger e) effs) =>
  S.Set Address ->
  Address ->
  hi ->
  SuiteItem hi effs [Sem effs ()] ->
  [RunFixture effs]
loadNode includedAddresses parentAddress hi =
  let exElm' :: forall hi'. Address -> hi' -> SuiteItem hi' effs [Sem effs ()] -> [RunFixture effs]
      exElm' = loadNode includedAddresses

      hook = RC.Hook
      group' = RC.Group

      nxtAddress :: Text -> AddressElemType -> Address
      nxtAddress ttl at = push ttl at parentAddress

      exclude :: Text -> AddressElemType -> Bool
      exclude title at = S.notMember (nxtAddress title at) includedAddresses
   in --  TODO exceptions - run in terms of bracket / resource
      \case
        Tests {tests} ->
          mkRunFixture <$> tests parentAddress hi
        --
        OnceHook {title = t, bHook, aHook, hkElms} -> uu
          -- let adr = nxtAddress t hook
          --  in exclude t hook ? pure () $
          --       do
          --         logItem $ StartHook C.BeforeAll t
          --         ho <- bHook hi
          --         logItem $ EndHook C.BeforeAll t
          --         sequence_ $ exElm' adr ho <$> hkElms
          --         logItem $ StartHook C.AfterAll t
          --         aHook ho
          --         logItem $ EndHook C.AfterAll t
        --
        Group {title = t, gElms} -> uu
          -- exclude t group' ? pure () $
          --   do
          --     logItem $ StaXTGroup $ GroupTitle t
          --     sequence_ $ exElm' (nxtAddress t group') hi <$> gElms
          --     logItem $ EndGroup $ GroupTitle t
-}


