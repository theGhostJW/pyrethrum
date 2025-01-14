module IOActionEffect (
    IOAction(..),
    ioAction
  )
where


import Effectful as EF ( Effect, DispatchOf, Dispatch(Dynamic))
import Effectful.TH (makeEffect)

type instance DispatchOf IOAction = Dynamic

data IOAction :: Effect where
  IoAction :: Text -> IO a -> IOAction m a

makeEffect ''IOAction