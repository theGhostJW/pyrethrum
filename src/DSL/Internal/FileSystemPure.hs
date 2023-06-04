-- remapping of Path.IO with some minor type changes to be used by both static and dynamic effects

module DSL.Internal.FileSystemPure (
  D.XdgDirectory (..),
  D.XdgDirectoryList (..),
  exeExtension,
  -- -- ** Permissions
  D.Permissions,
  D.emptyPermissions,
  D.readable,
  D.writable,
  D.executable,
  D.searchable,
  D.setOwnerReadable,
  D.setOwnerWritable,
  D.setOwnerExecutable,
  D.setOwnerSearchable,
  D.WalkAction (..),
) where

import qualified Path.IO as D
import qualified System.Directory as SD
import PyrethrumExtras (toS)

exeExtension :: Text
exeExtension = toS SD.exeExtension