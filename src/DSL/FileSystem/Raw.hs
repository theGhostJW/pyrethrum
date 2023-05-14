-- remapping of Path.IO with some minor type changes to be used by both static and dynamic effects

module DSL.FileSystem.Raw (
  -- * Actions on directories
  createDir,
  createDirIfMissing,
  removeDir,
  removeDirRecur,
  removePathForcibly,
  renameDir,
  listDir,

  -- ** Current working directory
  getCurrentDir,
  setCurrentDir,
  withCurrentDir,

  -- * Pre-defined directories
  getHomeDir,
  getXdgDir,
  getXdgDirList,
  getAppUserDataDir,
  getUserDocsDir,
  getTempDir,

  -- * Actions on files
  removeFile,
  renameFile,
  renamePath,
  copyFile,
  copyFileWithMetadata,
  getFileSize,
  DSL.FileSystem.Raw.canonicalizePath,
  DSL.FileSystem.Raw.makeAbsolute,
  DSL.FileSystem.Raw.makeRelativeToCurrentDir,

  -- * Existence tests
  doesPathExist,
  doesFileExist,
  doesDirExist,
  findExecutable,
  findFile,
  findFiles,
  -- , findFileWith
  findFilesWith,

  -- * Symbolic links
  createFileLink,
  createDirLink,
  removeDirLink,
  isSymlink,
  getSymlinkTarget,

  -- * Permissions
  getPermissions,
  setPermissions,
  copyPermissions,

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
  ensureDir,
  listDirRel,
  listDirRecur,
  listDirRecurRel,
  copyDirRecur,
  copyDirRecur',

  -- ** Walking directory trees
  D.WalkAction (..),
  walkDir,
  walkDirRel,
  walkDirAccum,
  walkDirAccumRel,

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
  isLocationOccupied,
  forgivingAbsence,
  ignoringAbsence,
) where

import Data.Time (UTCTime)
import Path
import Path.IO (AnyPath (..))
import qualified Path.IO as D
import Prelude (Bool (..), IO, Integer, Maybe (..), MonadIO, Text, ($), (.), (==), (||))
import qualified Prelude as P

import PyrethrumExtras (MonadMask, toS)
import qualified System.Directory as SD

exeExtension :: Text
exeExtension = toS SD.exeExtension

-- TODO:: hadock permissions
-- full integration tests demos
----------------------------------------
-- Actions on directories
createDir :: (MonadIO m) => Path b Dir -> m ()
createDir = D.createDir

ensureDir :: (MonadIO m) => Path b Dir -> m ()
ensureDir = D.ensureDir

createDirIfMissing ::
  (MonadIO m) =>
  -- | Create its parents too?
  Bool ->
  -- | The path to the directory you want to make
  Path b Dir ->
  m ()
createDirIfMissing =
  D.createDirIfMissing

removeDir :: (MonadIO m) => Path b Dir -> m ()
removeDir = D.removeDir

removeDirRecur :: (MonadIO m) => Path b Dir -> m ()
removeDirRecur = D.removeDirRecur

removePathForcibly :: (MonadIO m) => Path b t -> m ()
removePathForcibly = D.removePathForcibly

renameDir ::
  (MonadIO m) =>
  -- | Old name
  Path b0 Dir ->
  -- | New name
  Path b1 Dir ->
  m ()
renameDir = D.renameDir

listDir ::
  (MonadIO m) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  m ([Path Abs Dir], [Path Abs File])
listDir = D.listDir

----------------------------------------
-- Current working directory

getCurrentDir :: (MonadIO m) => m (Path Abs Dir)
getCurrentDir = D.getCurrentDir

setCurrentDir :: (MonadIO m) => Path b Dir -> m ()
setCurrentDir = D.setCurrentDir

withCurrentDir ::
  (MonadIO m, MonadMask m) =>
  -- | Directory to execute in
  Path b Dir ->
  -- | Action to be executed
  m a ->
  m a
withCurrentDir = D.withCurrentDir

----------------------------------------
-- Pre-defined directories
getHomeDir :: (MonadIO m) => m (Path Abs Dir)
getHomeDir = D.getHomeDir

getXdgDir ::
  (MonadIO m) =>
  -- | Which special directory
  D.XdgDirectory ->
  -- | A relative path that is appended to the path; if 'Nothing', the
  -- base path is returned
  Maybe (Path Rel Dir) ->
  m (Path Abs Dir)
getXdgDir = D.getXdgDir

getXdgDirList ::
  (MonadIO m) =>
  -- | Which special directory list
  D.XdgDirectoryList ->
  m [Path Abs Dir]
getXdgDirList = D.getXdgDirList

getAppUserDataDir ::
  (MonadIO m) =>
  -- | Name of application (used in path construction)
  Text ->
  m (Path Abs Dir)
getAppUserDataDir = D.getAppUserDataDir . toS

getUserDocsDir :: (MonadIO m) => m (Path Abs Dir)
getUserDocsDir = D.getUserDocsDir

getTempDir :: (MonadIO m) => m (Path Abs Dir)
getTempDir = D.getTempDir

----------------------------------------
-- Actions on files

removeFile :: (MonadIO m) => Path b File -> m ()
removeFile = D.removeFile

renameFile ::
  (MonadIO m) =>
  -- | Original location
  Path b0 File ->
  -- | New location
  Path b1 File ->
  m ()
renameFile = D.renameFile

renamePath :: (MonadIO m) => Path b0 t -> Path b1 t -> m ()
renamePath = D.renamePath

copyFile ::
  (MonadIO m) =>
  -- | Original location
  Path b0 File ->
  -- | Where to put copy
  Path b1 File ->
  m ()
copyFile = D.copyFile

copyFileWithMetadata :: Path Dir a -> Path Dir a -> IO ()
copyFileWithMetadata src dst = SD.copyFileWithMetadata (toFilePath src) (toFilePath dst)

getFileSize :: (MonadIO m) => Path b File -> m Integer
getFileSize = D.getFileSize

canonicalizePath :: (D.AnyPath p, MonadIO m) => p -> m (D.AbsPath p)
canonicalizePath = D.canonicalizePath

makeAbsolute ::
  (D.AnyPath p, MonadIO m) =>
  p ->
  m (D.AbsPath p)
makeAbsolute = D.makeAbsolute

makeRelativeToCurrentDir = D.makeRelativeToCurrentDir

----------------------------------------
-- Existence tests

doesPathExist = D.doesPathExist

doesFileExist = D.doesFileExist

doesDirExist = D.doesDirExist

findExecutable = D.findExecutable

findFile = D.findFile

findFiles = D.findFiles

findFilesWith p dirs ns = D.findFilesWith

----------------------------------------
-- Symbolic links

createFileLink = D.createFileLink

createDirLink = D.createDirLink

removeDirLink = D.removeDirLink

isSymlink = D.isSymlink

getSymlinkTarget = P.fmap toS . D.getSymlinkTarget

----------------------------------------
-- Permissions

getPermissions = D.getPermissions

-- | Lifted 'D.setPermissions'.
setPermissions = D.setPermissions

copyPermissions = D.copyPermissions

----------------------------------------
-- Timestamps
-- TODO:: change to chronos OffsetTime - note chronos uses minutes for offset

getAccessTime = D.getAccessTime

getModificationTime = D.getModificationTime

setAccessTime = D.setAccessTime

setModificationTime = D.setModificationTime

----------------------------------------
-- path-io only

listDirRel = D.listDirRel

listDirRecur = D.listDirRecur

{- | The same as 'listDirRecur' but returns paths that are relative to the
given directory.
-}
listDirRecurRel = D.listDirRecurRel

copyDirRecur = D.copyDirRecur

copyDirRecur' = D.copyDirRecur'

walkDir = D.walkDir

{- | The same as 'walkDir' but uses relative paths. The handler is given
@dir@, directory relative to the directory where traversal begins.
Sub-directories and files are relative to @dir@.

@since 1.4.2
-}
walkDirRel = D.walkDirRel

{- | Similar to 'walkDir' but accepts a 'Monoid'-returning output writer as
well. Values returned by the output writer invocations are accumulated
and returned.

Both, the descend handler as well as the output writer can be used for
side effects but keep in mind that the output writer runs before the
descend handler.
-}
walkDirAccum = D.walkDirAccum

{- | The same as 'walkDirAccum' but uses relative paths. The handler and
writer are given @dir@, directory relative to the directory where
traversal begins. Sub-directories and files are relative to @dir@.
-}
walkDirAccumRel = D.walkDirAccumRel

{- | Append Textly-typed path to an absolute path and then canonicalize
it.
-}
resolveFile b = D.resolveFile b . toS

-- | The same as 'resolveFile', but uses current working directory.
resolveFile' = D.resolveFile' . toS

-- | The same as 'resolveFile', but for directories.
resolveDir b = D.resolveDir b . toS

-- | The same as 'resolveDir', but uses current working directory.
resolveDir' = D.resolveDir' . toS

----------------------------------------------------------------------------
-- Temporary files and directories

{- | Use a temporary file that doesn't already exist.

Creates a new temporary file inside the given directory, making use of
the template. The temporary file is deleted after use.

@since 0.2.0
-}
withTempFile path t = D.withTempFile path (toS t)

{- | Create and use a temporary directory.

Creates a new temporary directory inside the given directory, making use
of the template. The temporary directory is deleted after use.

@since 0.2.0
-}
withTempDir path t = D.withTempDir path (toS t)

{- | Create and use a temporary file in the system standard temporary
directory.

Behaves exactly the same as 'withTempFile', except that the parent
temporary directory will be that returned by 'getTempDir'.

@since 0.2.0
-}
withSystemTempFile t = D.withSystemTempFile (toS t)

{- | Create and use a temporary directory in the system standard temporary
directory.

Behaves exactly the same as 'withTempDir', except that the parent
temporary directory will be that returned by 'getTempDir'.

@since 0.2.0
-}
withSystemTempDir t = D.withSystemTempDir (toS t)

{- | The function creates a temporary file in @rw@ mode. The created file
isn't deleted automatically, so you need to delete it manually.

The file is created with permissions such that only the current user can
read\/write it.

With some exceptions (see below), the file will be created securely in
the sense that an attacker should not be able to cause openTempFile to
overwrite another file on the filesystem using your credentials, by
putting symbolic links (on Unix) in the place where the temporary file is
to be created. On Unix the @O_CREAT@ and @O_EXCL@ flags are used to
prevent this attack, but note that @O_EXCL@ is sometimes not supported on
NFS filesystems, so if you rely on this behaviour it is best to use local
filesystems only.

@since 0.2.0
-}
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
openBinaryTempFile p = D.openBinaryTempFile p . toS

{- | Create a temporary directory. The created directory isn't deleted
automatically, so you need to delete it manually.

The directory is created with permissions such that only the current user
can read\/write it.

@since 0.2.0
-}
createTempDir p = D.createTempDir p . toS

--  * Existence tests

-- | Check if there is a file or directory on specified path.
isLocationOccupied = D.isLocationOccupied

{- | If argument of the function throws a
'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
exceptions propagate). Otherwise the result is returned inside a 'Just'.

@since 0.3.0
-}
forgivingAbsence = D.forgivingAbsence

{- | The same as 'forgivingAbsence', but ignores result.

@since 0.3.1
-}
ignoringAbsence = D.ignoringAbsence
