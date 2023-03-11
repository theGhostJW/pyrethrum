module EvalHelp (
  view,
  FixedEffs
) where
import Text.Show.Pretty
import DSL.FileSystem
import DSL.ArbitraryIO
import DSL.Logger
import Polysemy.Reader
import Polysemy.State
import DSL.LogProtocol
import DSL.CurrentTime
import DSL.Interpreter
import Polysemy

view :: Show a => a -> IO ()
view = pPrint

type FixedEffs = FileSystem ': 
                  ArbitraryIO ': 
                  Embed IO:
                  Logger Text ': 
                  Reader ThreadId ': 
                  State LogIndex ': 
                  CurrentTime ': 
                  Failure Text ': 
                  '[]