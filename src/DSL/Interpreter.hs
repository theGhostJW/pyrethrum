
module DSL.Interpreter where

import           Control.Monad.Freer
import           DSL.Ensure
import           DSL.FileSystem
import           Foundation.Extended
import TestItem

type InteractorFileSystem runConfig item apState = forall effs. (Members '[Ensure, FileSystem] effs, TestItem item) => runConfig -> item -> Eff effs apState
