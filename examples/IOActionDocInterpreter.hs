{-# OPTIONS_GHC -Wno-redundant-constraints #-}



module IOActionDocInterpreter where

import Effectful as EF
  ( Eff,
    IOE,
    type (:>),
  )
-- import Effectful.Reader.Dynamic
import Effectful.Dispatch.Dynamic
  ( interpret, LocalEnv,
  )
import DSL.DocInterpreterUtils (docErr)
import DSL.Internal.NodeLog (NodeLog)
import DSL.OutEffect ( Out )

import IOActionEffect
  ( IOAction(..),
  )


runIOAction :: forall es a. (HasCallStack, IOE :> es, Out NodeLog :> es) => Eff (IOAction : es) a -> Eff es a
runIOAction =
   interpret handler
 where
  handler ::
    forall a' localEs.
    (HasCallStack, IOAction :> localEs) =>
    LocalEnv localEs es ->
    IOAction (Eff localEs) a' ->
    Eff es a'
  handler _env  =
      \case
        IoAction description _action ->  docErr "ioAction" description
