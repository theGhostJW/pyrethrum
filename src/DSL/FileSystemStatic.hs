{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.FileSystemStatic (
  -- * Effect
  FileSystem,

  -- ** Handlers
  runFileSystem,

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
  canonicalizePath,
  makeAbsolute,
  makeRelativeToCurrentDir,

  -- * Existence tests
  doesPathExist,
  doesFileExist,
  doesDirExist,
  findExecutable,
  findFile,
  findFiles,
  findFileWith,
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
  R.XdgDirectory (..),
  R.XdgDirectoryList (..),
  R.exeExtension,

  -- ** Permissions
  R.Permissions,
  R.emptyPermissions,
  R.readable,
  R.writable,
  R.executable,
  R.searchable,
  R.setOwnerReadable,
  R.setOwnerWritable,
  R.setOwnerExecutable,
  R.setOwnerSearchable,
  -- from pathIO
  ensureDir,
  listDirRel,
  listDirRecur,
  listDirRecurRel,
  copyDirRecur,
  copyDirRecur',

  -- ** Walking directory trees
  R.WalkAction (..),
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

import qualified DSL.FileSystem.Raw as R

import Path (Abs, Dir, File, Path, Rel, toFilePath)
import qualified Path.IO as PIO
import Prelude
import qualified Prelude as P

import Chronos (OffsetDatetime)
import DSL.FileSystem.EffectStatic (FileSystem, runFileSystem)
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Static (
  unsafeEff_,
  unsafeLiftMapIO,
  unsafeSeqUnliftIO,
 )
import Effectful.Error.Static (Error)

import qualified System.Directory as D


ensureDir :: (FileSystem :> es) => Path b Dir -> Eff es ()
ensureDir = unsafeEff_ . R.ensureDir

createDir :: (FileSystem :> es) => Path b Dir -> Eff es ()
createDir = unsafeEff_ . R.createDir

createDirIfMissing :: (FileSystem :> es) => Bool -> Path b Dir -> Eff es ()
createDirIfMissing doCreateParents =
  unsafeEff_ . R.createDirIfMissing doCreateParents

removeDir :: (FileSystem :> es) => Path b Dir -> Eff es ()
removeDir = unsafeEff_ . R.removeDir

removeDirRecur :: (FileSystem :> es) => Path b Dir -> Eff es ()
removeDirRecur = unsafeEff_ . R.removeDirRecur

removePathForcibly :: (FileSystem :> es) => Path b t -> Eff es ()
removePathForcibly = unsafeEff_ . R.removePathForcibly

renameDir :: (FileSystem :> es) => Path b Dir -> Path b Dir -> Eff es ()
renameDir old = unsafeEff_ . R.renameDir old

listDir :: (FileSystem :> es) => Path b Dir -> Eff es ([Path Abs Dir], [Path Abs File])
listDir = unsafeEff_ . R.listDir

-- Current working directory
getCurrentDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getCurrentDir = unsafeEff_ R.getCurrentDir

setCurrentDir :: (FileSystem :> es) => Path b Dir -> Eff es ()
setCurrentDir = unsafeEff_ . R.setCurrentDir

withCurrentDir :: (FileSystem :> es) => Path b Dir -> Eff es a -> Eff es a
withCurrentDir path = unsafeLiftMapIO (R.withCurrentDir path)

----------------------------------------
-- Pre-defined directories

getHomeDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getHomeDir = unsafeEff_ R.getHomeDir

getXdgDir ::
  (FileSystem :> es) =>
  R.XdgDirectory ->
  Maybe (Path Rel Dir) ->
  Eff es (Path Abs Dir)
getXdgDir xdgDir = unsafeEff_ . R.getXdgDir xdgDir

getXdgDirList ::
  (FileSystem :> es) =>
  R.XdgDirectoryList ->
  Eff es [Path Abs Dir]
getXdgDirList = unsafeEff_ . R.getXdgDirList

getAppUserDataDir :: (FileSystem :> es) => Text -> Eff es (Path Abs Dir)
getAppUserDataDir = unsafeEff_ . R.getAppUserDataDir

getUserDocsDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getUserDocsDir = unsafeEff_ R.getUserDocsDir

getTempDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getTempDir = unsafeEff_ R.getTempDir

----------------------------------------
-- Actions on files

removeFile :: (FileSystem :> es) => Path b File -> Eff es ()
removeFile = unsafeEff_ . R.removeFile

renameFile :: (FileSystem :> es) => Path b File -> Path b File -> Eff es ()
renameFile old = unsafeEff_ . R.renameFile old

renamePath :: (FileSystem :> es) => Path b t -> Path b t -> Eff es ()
renamePath old = unsafeEff_ . R.renamePath old

copyFile :: (FileSystem :> es) => Path b File -> Path b File -> Eff es ()
copyFile src = unsafeEff_ . R.copyFile src

copyFileWithMetadata :: (FileSystem :> es) => Path b File -> Path b0 File -> Eff es ()
copyFileWithMetadata src = unsafeEff_ . R.copyFileWithMetadata src

getFileSize :: (FileSystem :> es) => Path b File -> Eff es Integer
getFileSize = unsafeEff_ . R.getFileSize

canonicalizePath :: (PIO.AnyPath path, FileSystem :> es) => path -> Eff es (PIO.AbsPath path)
canonicalizePath = unsafeEff_ . R.canonicalizePath

makeAbsolute :: (PIO.AnyPath path) => (FileSystem :> es) => path -> Eff es (PIO.AbsPath path)
makeAbsolute = unsafeEff_ . R.makeAbsolute

makeRelativeToCurrentDir ::
  (PIO.AnyPath path, FileSystem :> es) =>
  path ->
  Eff es (PIO.RelPath path)
makeRelativeToCurrentDir = unsafeEff_ . R.makeRelativeToCurrentDir

----------------------------------------
-- Existence tests

doesPathExist :: (FileSystem :> es) => Path b t -> Eff es Bool
doesPathExist = unsafeEff_ . R.doesPathExist

doesFileExist :: (FileSystem :> es) => Path b File -> Eff es Bool
doesFileExist = unsafeEff_ . R.doesFileExist

doesDirExist :: (FileSystem :> es) => Path b Dir -> Eff es Bool
doesDirExist = unsafeEff_ . R.doesDirExist

findExecutable :: (FileSystem :> es) => Path Rel File -> Eff es (Maybe (Path Abs File))
findExecutable = unsafeEff_ . R.findExecutable

findFile :: (FileSystem :> es) => [Path b Dir] -> Path Rel File -> Eff es (Maybe (Path Abs File))
findFile dirs = unsafeEff_ . R.findFile dirs

findFiles :: (FileSystem :> es) => [Path b Dir] -> Path Rel File -> Eff es [Path Abs File]
findFiles dirs = unsafeEff_ . R.findFiles dirs

findFileWith ::
  (FileSystem :> es) =>
  (Path Abs File -> Eff es Bool) ->
  [Path b Dir] ->
  Path Rel File ->
  Eff es (Maybe (Path Abs File))
findFileWith p dirs ns = unsafeSeqUnliftIO $ \unlift -> R.findFileWith (unlift . p) dirs ns

findFilesWith ::
  (FileSystem :> es) =>
  (Path Abs File -> Eff es Bool) ->
  [Path b Dir] ->
  Path Rel File ->
  Eff es [Path Abs File]
findFilesWith p dirs ns = unsafeSeqUnliftIO $ \unlift -> do
  R.findFilesWith (unlift . p) dirs ns

----------------------------------------
-- Symbolic links

createFileLink :: (FileSystem :> es) => Path b File -> Path b File -> Eff es ()
createFileLink target = unsafeEff_ . R.createFileLink target

createDirLink :: (FileSystem :> es) => Path b Dir -> Path b Dir -> Eff es ()
createDirLink target = unsafeEff_ . R.createDirLink target

removeDirLink :: (FileSystem :> es) => Path b Dir -> Eff es ()
removeDirLink = unsafeEff_ . R.removeDirLink

isSymlink :: (FileSystem :> es) => Path b t -> Eff es Bool
isSymlink = unsafeEff_ . R.isSymlink

getSymlinkTarget :: (FileSystem :> es) => Path b t -> Eff es Text
getSymlinkTarget = unsafeEff_ . R.getSymlinkTarget

----------------------------------------
-- Permissions

getPermissions :: (FileSystem :> es) => Path b t -> Eff es R.Permissions
getPermissions = unsafeEff_ . R.getPermissions

setPermissions :: (FileSystem :> es) => Path b t -> R.Permissions -> Eff es ()
setPermissions path = unsafeEff_ . R.setPermissions path

copyPermissions :: (FileSystem :> es) => Path b t -> Path b t -> Eff es ()
copyPermissions src = unsafeEff_ . R.copyPermissions src

----------------------------------------
-- Timestamps

getAccessTime :: (FileSystem :> es) => Path b t -> Eff es OffsetDatetime
getAccessTime = unsafeEff_ . R.getAccessTime

getModificationTime :: (FileSystem :> es) => Path b t -> Eff es OffsetDatetime
getModificationTime = unsafeEff_ . R.getModificationTime

setAccessTime :: (FileSystem :> es) => Path b t -> OffsetDatetime -> Eff es ()
setAccessTime path = unsafeEff_ . R.setAccessTime path

setModificationTime :: (FileSystem :> es) => Path b t -> OffsetDatetime -> Eff es ()
setModificationTime path = unsafeEff_ . R.setModificationTime path

----------------------------------------
-- path-io only

listDirRel ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRel = unsafeEff_ . R.listDirRel


listDirRecur ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Abs Dir], [Path Abs File])
listDirRecur = unsafeEff_ . R.listDirRecur


listDirRecurRel ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRecurRel = unsafeEff_ . R.listDirRecurRel


copyDirRecur ::
  (FileSystem :> es, Error e :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur s = unsafeEff_ . R.copyDirRecur s

copyDirRecur' ::
  (FileSystem :> es, Error e :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur' s = unsafeEff_ . R.copyDirRecur' s

-- Walking directory trees
-- TODO Higher order effect
walkDir ::
  (IOE :> es, FileSystem :> es) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (R.WalkAction Abs)) ->
  -- | Directory where traversal begins
  Path b Dir ->
  Eff es ()
walkDir h = unsafeEff_ . R.walkDir h

-- Walking directory trees
-- TODO Higher order effect
walkDirRel ::
  (FileSystem :> es) =>
  ( Path Rel Dir ->
    [Path Rel Dir] ->
    [Path Rel File] ->
    IO (R.WalkAction Rel)
  ) ->
  Path b Dir ->
  Eff es ()
walkDirRel h = unsafeEff_ . R.walkDirRel h

-- TODO Higher order effect
walkDirAccum ::
  (FileSystem :> es, P.Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (R.WalkAction Abs)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  Eff es o
walkDirAccum h w = unsafeEff_ . R.walkDirAccum h w


walkDirAccumRel ::
  (FileSystem :> es, P.Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> IO (R.WalkAction Rel)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> IO o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  Eff es o
walkDirAccumRel h w = unsafeEff_ . R.walkDirAccumRel h w


resolveFile ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  Text ->
  Eff es (Path Abs File)
resolveFile b = unsafeEff_ . R.resolveFile b

-- | The same as 'resolveFile', but uses current working directory.
resolveFile' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  Text ->
  Eff es (Path Abs File)
resolveFile' = unsafeEff_ . R.resolveFile'

-- | The same as 'resolveFile', but for directories.
resolveDir ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  Text ->
  Eff es (Path Abs Dir)
resolveDir b = unsafeEff_ . R.resolveDir b

-- | The same as 'resolveDir', but uses current working directory.
resolveDir' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  Text ->
  Eff es (Path Abs Dir)
resolveDir' = unsafeEff_ . R.resolveDir'

----------------------------------------------------------------------------
-- Temporary files and directories

withTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> IO a) ->
  Eff es a
withTempFile path t = unsafeEff_ . R.withTempFile path t


withTempDir ::
  (FileSystem :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> IO a) ->
  Eff es a
withTempDir p t = unsafeEff_ . R.withTempDir p t

withSystemTempFile ::
  (FileSystem :> es) =>
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> IO a) ->
  Eff es a
withSystemTempFile t =
  unsafeEff_ . R.withSystemTempFile t


withSystemTempDir ::
  (FileSystem :> es) =>
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> IO a) ->
  Eff es a
withSystemTempDir t = unsafeEff_ . R.withSystemTempDir t


openTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template; if the template is "foo.ext" then the created
  -- file will be @\"fooXXX.ext\"@ where @XXX@ is some random number
  Text ->
  -- | Name of created file and its 'Handle'
  Eff es (Path Abs File, P.Handle)
openTempFile p = unsafeEff_ . R.openTempFile p


openBinaryTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Name of created file and its 'Handle'
  Eff es (Path Abs File, P.Handle)
openBinaryTempFile p = unsafeEff_ . R.openBinaryTempFile p


createTempDir ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Name of created temporary directory
  Eff es (Path Abs Dir)
createTempDir p = unsafeEff_ . R.createTempDir p

--  * Existence tests

isLocationOccupied :: (FileSystem :> es) => Path b t -> Eff es Bool
isLocationOccupied = unsafeEff_ . R.isLocationOccupied

forgivingAbsence :: (FileSystem :> es) => IO a -> Eff es (Maybe a)
forgivingAbsence = unsafeEff_ . R.forgivingAbsence

ignoringAbsence :: (FileSystem :> es) => IO a -> Eff es ()
ignoringAbsence = unsafeEff_ . R.ignoringAbsence
