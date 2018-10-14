module Test1 where

import           TestTypes
import Foundation
import           Control.Monad.Freer
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Logger


test1 :: forall effs. Members '[Logger, Ensure, FileSystem] effs => Test String (Eff effs String) String String
test1 = undefined
