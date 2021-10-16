module EvalHelp (
  view
) where
import Pyrelude
import Text.Show.Pretty

view :: Show a => a -> IO ()
view = pPrint