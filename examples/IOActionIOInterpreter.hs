module IOActionIOInterpreter where

import Effectful as EF
  ( Eff,
    IOE,
    liftIO,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( interpret,
  )
import IOActionEffect (IOAction (..))

runIOAction :: forall es a. (IOE :> es) => Eff (IOAction : es) a -> Eff es a
runIOAction =
  interpret $ \_ ->
    EF.liftIO . \case
      IoAction _description action -> action
