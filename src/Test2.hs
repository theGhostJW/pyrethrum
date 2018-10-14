module Test2 where

import           TestTypes
import Foundation
import           Control.Monad.Freer
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Logger
import           DSL.Interpreter


test2 :: forall effs. Members '[Ensure] effs => Test Int (Eff effs String) Int Int
test2 = undefined
