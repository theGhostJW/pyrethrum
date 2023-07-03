module StepDocumenterDemo where

import DSL.FileSystemDocInterpreter
import DSL.FileSystemEffect
import DSL.Out
import Effectful ((:>), Eff)
import PyrethrumExtras (uu, Path, File, Abs, (?), toS, txt)
import Path (toFilePath, reldir, relfile)
import Data.List.Extra (isInfixOf)

-- type FSOut es = (Out Text :> es, FileSystem :> es)

demoApp :: (Out Text :> es, FileSystem :> es) => Text -> Eff es ()
demoApp txt' = do 
  s <- findFilesWith isDeleteMe [[reldir|chris|]] [relfile|foo.txt|]
  r <- test s
  out $ r ? "yes" $ "no"
 where 
  isDeleteMe :: Path Abs File -> Eff es Bool
  isDeleteMe = pure . ("deleteMe" `isInfixOf`) . toFilePath

test :: [Path Abs File] -> Eff es Bool
test _ = pure True

-- TODO => hide string based prntLn et. al.
consoleSink :: Show a => Sink a
consoleSink = Sink print