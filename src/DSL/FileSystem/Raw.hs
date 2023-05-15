-- remapping of Path.IO with some minor type changes to be used by both static and dynamic effects

module DSL.FileSystem.Raw (
  -- * Actions on directories
  D.createDir,
  D.createDirIfMissing,
  D.removeDir,
  D.removeDirRecur,
  D.removePathForcibly,
  D.renameDir,
  D.listDir,

  -- ** Current working directory
  D.getCurrentDir,
  D.setCurrentDir,
  D.withCurrentDir,

  -- * Pre-defined directories
  D.getHomeDir,
  D.getXdgDir,
  D.getXdgDirList,
  getAppUserDataDir,
  D.getUserDocsDir,
  D.getTempDir,

  -- * Actions on files
  D.removeFile,
  D.renameFile,
  D.renamePath,
  D.copyFile,
  copyFileWithMetadata,
  D.getFileSize,
  D.canonicalizePath,
  D.makeAbsolute,
  D.makeRelativeToCurrentDir,

  -- * Existence tests
  D.doesPathExist,
  D.doesFileExist,
  D.doesDirExist,
  D.findExecutable,
  D.findFile,
  D.findFiles,
  -- , findFileWith
  D.findFilesWith,

  -- * Symbolic links
  D.createFileLink,
  D.createDirLink,
  D.removeDirLink,
  D.isSymlink,
  getSymlinkTarget,

  -- * Permissions
  D.getPermissions,
  D.setPermissions,
  D.copyPermissions,

  -- * Timestamps
  getAccessTime,
  getModificationTime,
  setAccessTime,
  setModificationTime,

  -- * Re-exports

  -- ** Pre-defined directories
  D.XdgDirectory (..),
  D.XdgDirectoryList (..),

  -- ** Existence tests
  exeExtension,

  -- ** Permissions
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
  -- from pathIO
  D.ensureDir,
  D.listDirRel,
  D.listDirRecur,
  D.listDirRecurRel,
  D.copyDirRecur,
  D.copyDirRecur',

  -- ** Walking directory trees
  D.WalkAction (..),
  D.walkDir,
  D.walkDirRel,
  D.walkDirAccum,
  D.walkDirAccumRel,

  -- * Path b t transformation
  resolveFile,
  resolveFile',
  resolveDir,
  resolveDir',

  -- * Temporary files and directories
  withTempFile,
  withTempDir,
  withSystemTempFile,
  withSystemTempDir,
  openTempFile,
  openBinaryTempFile,
  createTempDir,

  -- * Existence tests
  D.isLocationOccupied,
  D.forgivingAbsence,
  D.ignoringAbsence,
) where

import Data.Time (UTCTime)
import Path
import Path.IO (AnyPath (..))
import qualified Path.IO as D
import Prelude (Bool (..), IO, Integer, Maybe (..), MonadIO, Text, ($), (.), (==), (||))
import qualified Prelude as P

import PyrethrumExtras (MonadCatch, MonadMask, toS)
import qualified System.Directory as SD

exeExtension :: Text
exeExtension = toS SD.exeExtension

-- -- TODO:: hadock permissions
-- -- full integration tests demos
-- ----------------------------------------
-- -- Actions on directories
-- createDir :: (MonadIO m) => Path b Dir -> m ()
-- createDir = D.createDir

-- ensureDir :: (MonadIO m) => Path b Dir -> m ()
-- ensureDir = D.ensureDir

-- createDirIfMissing ::
--   (MonadIO m) =>
--   -- | Create its parents too?
--   Bool ->
--   -- | The path to the directory you want to make
--   Path b Dir ->
--   m ()
-- createDirIfMissing =
--   D.createDirIfMissing

-- removeDir :: (MonadIO m) => Path b Dir -> m ()
-- removeDir = D.removeDir

-- removeDirRecur :: (MonadIO m) => Path b Dir -> m ()
-- removeDirRecur = D.removeDirRecur

-- removePathForcibly :: (MonadIO m) => Path b t -> m ()
-- removePathForcibly = D.removePathForcibly

-- renameDir ::
--   (MonadIO m) =>
--   -- | Old name
--   Path b0 Dir ->
--   -- | New name
--   Path b1 Dir ->
--   m ()
-- renameDir = D.renameDir

-- listDir ::
--   (MonadIO m) =>
--   -- | Directory to list
--   Path b Dir ->
--   -- | Sub-directories and files
--   m ([Path Abs Dir], [Path Abs File])
-- listDir = D.listDir

-- ----------------------------------------
-- -- Current working directory

-- getCurrentDir :: (MonadIO m) => m (Path Abs Dir)
-- getCurrentDir = D.getCurrentDir

-- setCurrentDir :: (MonadIO m) => Path b Dir -> m ()
-- setCurrentDir = D.setCurrentDir

-- withCurrentDir ::
--   (MonadIO m, MonadMask m) =>
--   -- | Directory to execute in
--   Path b Dir ->
--   -- | Action to be executed
--   m a ->
--   m a
-- withCurrentDir = D.withCurrentDir

-- ----------------------------------------
-- -- Pre-defined directories
-- getHomeDir :: (MonadIO m) => m (Path Abs Dir)
-- getHomeDir = D.getHomeDir

-- getXdgDir ::
--   (MonadIO m) =>
--   -- | Which special directory
--   D.XdgDirectory ->
--   -- | A relative path that is appended to the path; if 'Nothing', the
--   -- base path is returned
--   Maybe (Path Rel Dir) ->
--   m (Path Abs Dir)
-- getXdgDir = D.getXdgDir

-- getXdgDirList ::
--   (MonadIO m) =>
--   -- | Which special directory list
--   D.XdgDirectoryList ->
--   m [Path Abs Dir]
-- getXdgDirList = D.getXdgDirList

getAppUserDataDir ::
  (MonadIO m) =>
  -- | Name of application (used in path construction)
  Text ->
  m (Path Abs Dir)
getAppUserDataDir = D.getAppUserDataDir . toS

-- getUserDocsDir :: (MonadIO m) => m (Path Abs Dir)
-- getUserDocsDir = D.getUserDocsDir

-- getTempDir :: (MonadIO m) => m (Path Abs Dir)
-- getTempDir = D.getTempDir

-- ----------------------------------------
-- -- Actions on files

-- removeFile :: (MonadIO m) => Path b File -> m ()
-- removeFile = D.removeFile

-- renameFile ::
--   (MonadIO m) =>
--   -- | Original location
--   Path b0 File ->
--   -- | New location
--   Path b1 File ->
--   m ()
-- renameFile = D.renameFile

-- renamePath :: (MonadIO m) => Path b0 t -> Path b1 t -> m ()
-- renamePath = D.renamePath

-- copyFile ::
--   (MonadIO m) =>
--   -- | Original location
--   Path b0 File ->
--   -- | Where to put copy
--   Path b1 File ->
--   m ()
-- copyFile = D.copyFile

copyFileWithMetadata :: Path Dir a -> Path Dir a -> IO ()
copyFileWithMetadata src dst = SD.copyFileWithMetadata (toFilePath src) (toFilePath dst)

-- getFileSize :: (MonadIO m) => Path b File -> m Integer
-- getFileSize = D.getFileSize

-- canonicalizePath :: (D.AnyPath p, MonadIO m) => p -> m (D.AbsPath p)
-- canonicalizePath = D.canonicalizePath

-- makeAbsolute ::
--   (D.AnyPath p, MonadIO m) =>
--   p ->
--   m (D.AbsPath p)
-- makeAbsolute = D.makeAbsolute

-- makeRelativeToCurrentDir ::
--  (D.AnyPath path, MonadIO m)
--   => path
--   -> m (D.RelPath path)
-- makeRelativeToCurrentDir = D.makeRelativeToCurrentDir

-- ----------------------------------------
-- -- Existence tests

-- doesPathExist :: MonadIO m => Path b t -> m Bool
-- doesPathExist = D.doesPathExist

-- doesFileExist :: MonadIO m => Path b File -> m Bool
-- doesFileExist = D.doesFileExist

-- doesDirExist :: MonadIO m => Path b Dir -> m Bool
-- doesDirExist = D.doesDirExist

-- findExecutable ::
--   (MonadIO m) =>
--   Path Rel File ->
--   m (Maybe (Path Abs File))
-- findExecutable = D.findExecutable

-- findFile ::
--   (MonadIO m) =>
--   [Path b Dir] ->
--   Path Rel File ->
--   m (Maybe (Path Abs File))
-- findFile = D.findFile

-- findFiles ::
--   (MonadIO m) =>
--   [Path b Dir] ->
--   Path Rel File ->
--   m [Path Abs File]
-- findFiles = D.findFiles

-- findFilesWith ::
--   (MonadIO m) =>
--   -- | How to test the files
--   (Path Abs File -> m Bool) ->
--   -- | Set of directories to search in
--   [Path b Dir] ->
--   -- | Filename of interest
--   Path Rel File ->
--   -- | Absolute paths to all found files
--   m [Path Abs File]
-- findFilesWith = D.findFilesWith

-- ----------------------------------------
-- -- Symbolic links
-- createFileLink ::
--   (MonadIO m) =>
--   -- | Path to the target file
--   Path b0 File ->
--   -- | Path to the link to be created
--   Path b1 File ->
--   m ()
-- createFileLink = D.createFileLink

-- createDirLink ::
--   (MonadIO m) =>
--   -- | Path to the target directory
--   Path b0 Dir ->
--   -- | Path to the link to be created
--   Path b1 Dir ->
--   m ()
-- createDirLink = D.createDirLink

-- removeDirLink ::
--   (MonadIO m) =>
--   -- | Path to the link to be removed
--   Path b Dir ->
--   m ()
-- removeDirLink = D.removeDirLink

-- isSymlink :: (MonadIO m) => Path b t -> m Bool
-- isSymlink = D.isSymlink

-- @since 1.5.0
getSymlinkTarget ::
  (MonadIO m) =>
  -- | Symlink path
  Path b t ->
  m Text
getSymlinkTarget = P.fmap toS . D.getSymlinkTarget

-- ----------------------------------------
-- -- Permissions
-- getPermissions :: (MonadIO m) => Path b t -> m D.Permissions
-- getPermissions = D.getPermissions

-- -- | Lifted 'D.setPermissions'.
-- setPermissions :: (MonadIO m) => Path b t -> D.Permissions -> m ()
-- setPermissions = D.setPermissions

-- copyPermissions ::
--   (MonadIO m) =>
--   -- | From where to copy
--   Path b0 t0 ->
--   -- | What to modify
--   Path b1 t1 ->
--   m ()
-- copyPermissions = D.copyPermissions

-- ----------------------------------------
-- Timestamps
-- TODO:: change to chronos OffsetTime - note chronos uses minutes for offset
getAccessTime :: (MonadIO m) => Path b t -> m UTCTime
getAccessTime = D.getAccessTime

getModificationTime :: (MonadIO m) => Path b t -> m UTCTime
getModificationTime = D.getModificationTime

setAccessTime :: (MonadIO m) => Path b t -> UTCTime -> m ()
setAccessTime = D.setAccessTime

setModificationTime :: (MonadIO m) => Path b t -> UTCTime -> m ()
setModificationTime = D.setModificationTime

-- ----------------------------------------
-- -- path-io only
-- listDirRel ::
--   (MonadIO m) =>
--   -- | Directory to list
--   Path b Dir ->
--   -- | Sub-directories and files
--   m ([Path Rel Dir], [Path Rel File])
-- listDirRel = D.listDirRel

-- listDirRecur ::
--   (MonadIO m) =>
--   -- | Directory to list
--   Path b Dir ->
--   -- | Sub-directories and files
--   m ([Path Abs Dir], [Path Abs File])
-- listDirRecur = D.listDirRecur

-- {- | The same as 'listDirRecur' but returns paths that are relative to the
-- given directory.
-- -}
-- listDirRecurRel ::
--   (MonadIO m) =>
--   -- | Directory to list
--   Path b Dir ->
--   -- | Sub-directories and files
--   m ([Path Rel Dir], [Path Rel File])
-- listDirRecurRel = D.listDirRecurRel

-- copyDirRecur ::
--   (MonadIO m, MonadCatch m) =>
--   -- | Source
--   Path b0 Dir ->
--   -- | Destination
--   Path b1 Dir ->
--   m ()
-- copyDirRecur = D.copyDirRecur

-- copyDirRecur' ::
--   (MonadIO m, MonadCatch m) =>
--   -- | Source
--   Path b0 Dir ->
--   -- | Destination
--   Path b1 Dir ->
--   m ()
-- copyDirRecur' = D.copyDirRecur'

-- walkDir ::
--   (MonadIO m) =>
--   -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
--   (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (WalkAction Abs)) ->
--   -- | Directory where traversal begins
--   Path b Dir ->
--   m ()
-- walkDir = D.walkDir

-- {- | The same as 'walkDir' but uses relative paths. The handler is given
-- @dir@, directory relative to the directory where traversal begins.
-- Sub-directories and files are relative to @dir@.

-- @since 1.4.2
-- -}
-- walkDirRel = D.walkDirRel

-- {- | Similar to 'walkDir' but accepts a 'Monoid'-returning output writer as
-- well. Values returned by the output writer invocations are accumulated
-- and returned.

-- Both, the descend handler as well as the output writer can be used for
-- side effects but keep in mind that the output writer runs before the
-- descend handler.
-- -}
-- walkDirAccum = D.walkDirAccum

-- {- | The same as 'walkDirAccum' but uses relative paths. The handler and
-- writer are given @dir@, directory relative to the directory where
-- traversal begins. Sub-directories and files are relative to @dir@.
-- -}
-- walkDirAccumRel = D.walkDirAccumRel

-- {- | Append Textly-typed path to an absolute path and then canonicalize
-- it.
-- -}
resolveFile :: (MonadIO m) => Path Abs Dir -> Text -> m (Path Abs File)
resolveFile b = D.resolveFile b . toS

-- | The same as 'resolveFile', but uses current working directory.
resolveFile' ::
  (MonadIO m) =>
  -- | Path to resolve
  Text ->
  m (Path Abs File)
resolveFile' = D.resolveFile' . toS

-- | The same as 'resolveFile', but for directories.
resolveDir ::
  (MonadIO m) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  Text ->
  m (Path Abs Dir)
resolveDir b = D.resolveDir b . toS

-- | The same as 'resolveDir', but uses current working directory.
resolveDir' ::
  (MonadIO m) =>
  -- | Path to resolve
  Text ->
  m (Path Abs Dir)
resolveDir' = D.resolveDir' . toS

----------------------------------------------------------------------------
-- Temporary files and directories

{- | Use a temporary file that doesn't already exist.

Creates a new temporary file inside the given directory, making use of
the template. The temporary file is deleted after use.

@since 0.2.0
-}
withTempFile ::
  (MonadIO m, MonadMask m) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> m a) ->
  m a
withTempFile path = D.withTempFile path . toS

{- | Create and use a temporary directory.

Creates a new temporary directory inside the given directory, making use
of the template. The temporary directory is deleted after use.

@since 0.2.0
-}
withTempDir ::
  (MonadIO m, MonadMask m) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> m a) ->
  m a
withTempDir path = D.withTempDir path . toS

{- | Create and use a temporary file in the system standard temporary
directory.

Behaves exactly the same as 'withTempFile', except that the parent
temporary directory will be that returned by 'getTempDir'.

@since 0.2.0
-}
withSystemTempFile ::
  (MonadIO m, MonadMask m) =>
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> m a) ->
  m a
withSystemTempFile = D.withSystemTempFile . toS

{- | Create and use a temporary directory in the system standard temporary
directory.

Behaves exactly the same as 'withTempDir', except that the parent
temporary directory will be that returned by 'getTempDir'.

@since 0.2.0
-}
withSystemTempDir ::
  (MonadIO m, MonadMask m) =>
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> m a) ->
  m a
withSystemTempDir t = D.withSystemTempDir (toS t)

openTempFile ::
  (MonadIO m) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template; if the template is "foo.ext" then the created
  -- file will be @\"fooXXX.ext\"@ where @XXX@ is some random number
  Text ->
  -- | Name of created file and its 'Handle'
  m (Path Abs File, P.Handle)
openTempFile p = D.openTempFile p . toS

{- | Like 'openTempFile', but opens the file in binary mode. On Windows,
reading a file in text mode (which is the default) will translate @CRLF@
to @LF@, and writing will translate @LF@ to @CRLF@. This is usually what
you want with text files. With binary files this is undesirable; also, as
usual under Microsoft operating systems, text mode treats control-Z as
EOF. Binary mode turns off all special treatment of end-of-line and
end-of-file characters.

@since 0.2.0
-}
openBinaryTempFile ::
  (MonadIO m) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Name of created file and its 'Handle'
  m (Path Abs File, P.Handle)
openBinaryTempFile p = D.openBinaryTempFile p . toS

{- | Create a temporary directory. The created directory isn't deleted
automatically, so you need to delete it manually.

The directory is created with permissions such that only the current user
can read\/write it.

@since 0.2.0
-}
createTempDir ::
  (MonadIO m) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Name of created temporary directory
  m (Path Abs Dir)
createTempDir p = D.createTempDir p . toS

-- --  * Existence tests

-- -- | Check if there is a file or directory on specified path.
-- isLocationOccupied = D.isLocationOccupied

-- {- | If argument of the function throws a
-- 'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
-- exceptions propagate). Otherwise the result is returned inside a 'Just'.

-- @since 0.3.0
-- -}
-- forgivingAbsence = D.forgivingAbsence

-- {- | The same as 'forgivingAbsence', but ignores result.

-- @since 0.3.1
-- -}
-- ignoringAbsence = D.ignoringAbsence
