module DSL.FileSystemEffect (
  -- * Effect
  FileSystem (..),
  FSException (..),
  -- -- * Actions on directories
  module FSP,
  FSP.XdgDirectory (..),
  FSP.XdgDirectoryList (..),
  {-
  createDir,
  createDirIfMissing,
  removeDir,
  removeDirRecur,
  removePathForcibly,
  renameDir,
  listDir,
  -- -- ** Current working directory
  getCurrentDir,
  setCurrentDir,
  withCurrentDir,
  -- -- * Pre-defined directories
  getHomeDir,
  getXdgDir,
  getXdgDirList,
  getAppUserDataDir,
  getUserDocsDir,
  getTempDir,
  -- -- * Actions on files
  removeFile,
  renameFile,
  renamePath,
  copyFile,
  copyFileWithMetadata,
  getFileSize,
  canonicalizePath,
  makeAbsolute,
  makeRelativeToCurrentDir,
  -- -- * Existence tests
  doesPathExist,
  doesFileExist,
  doesDirExist,
  findExecutable,
  findFile,
  findFiles,
  findFileWith,
  findFilesWith,
  -- -- * Symbolic links
  createFileLink,
  createDirLink,
  removeDirLink,
  isSymlink,
  getSymlinkTarget,
  -- -- * Permissions
  getPermissions,
  setPermissions,
  copyPermissions,
  -- -- * Timestamps
  getAccessTime,
  getModificationTime,
  setAccessTime,
  setModificationTime,
  -- -- * Re-exports

  -- from pathIO
  ensureDir,
  listDirRel,
  listDirRecur,
  listDirRecurRel,
  copyDirRecur,
  copyDirRecur',
  -- -- ** Walking directory trees
  FSP.WalkAction (..),
  walkDir,
  walkDirRel,
  -}
  walkDirAccum,
  {-
  walkDirAccumRel,
  -- -- * Path b t transformation
  resolveFile,
  resolveFile',
  resolveDir,
  resolveDir',
  -- -- * Temporary files and directories
  withTempFile,
  withTempDir,
  withSystemTempFile,
  withSystemTempDir,
  openTempFile,
  openBinaryTempFile,
  createTempDir,
  -- -- * Existence tests
  isLocationOccupied,
  forgivingAbsence,
  ignoringAbsence,
  -- from UnliftIO.IO.File
  writeBinaryFile,
  writeBinaryFileAtomic,
  writeBinaryFileDurable,
  writeBinaryFileDurableAtomic,
  withBinaryFile,
  withBinaryFileAtomic,
  withBinaryFileDurable,
  withBinaryFileDurableAtomic,
  ensureFileDurable,
  -}
) where

import DSL.Internal.FileSystemPure as FSP
import Path (Abs, Dir, File, Path, Rel)

import BasePrelude (IOException)
import Chronos (OffsetDatetime)
import Effectful as EF (
  Dispatch (Dynamic),
  DispatchOf,
  Effect,
 )
import Effectful.TH (makeEffect)
import Path.IO (AbsPath, AnyPath, RelPath)

type instance DispatchOf FileSystem = Dynamic

newtype FSException = FSException IOException
  deriving (Show, Eq, Exception)

-- instance Exception FSException

data FileSystem :: Effect where
  {-
  EnsureDir :: Path b Dir -> FileSystem m ()
  CreateDir :: Path b Dir -> FileSystem m ()
  CreateDirIfMissing :: Bool -> Path b Dir -> FileSystem m ()
  RemoveDir :: Path b Dir -> FileSystem m ()
  RemoveDirRecur :: Path b Dir -> FileSystem m ()
  RemovePathForcibly :: Path b t -> FileSystem m ()
  RenameDir :: Path b Dir -> Path b Dir -> FileSystem m ()
  ListDir :: Path b Dir -> FileSystem m ([Path Abs Dir], [Path Abs File])
  GetCurrentDir :: FileSystem m (Path Abs Dir)
  SetCurrentDir :: Path Abs Dir -> FileSystem m ()
  WithCurrentDir :: Path Abs Dir -> m a -> FileSystem m a
  GetHomeDir :: FileSystem m (Path Abs Dir)
  GetXdgDir :: FSP.XdgDirectory -> Maybe (Path Rel Dir) -> FileSystem m (Path Abs Dir)
  GetXdgDirList :: FSP.XdgDirectoryList -> FileSystem m [Path Abs Dir]
  GetAppUserDataDir :: Text -> FileSystem m (Path Abs Dir)
  GetUserDocsDir :: FileSystem m (Path Abs Dir)
  GetTempDir :: FileSystem m (Path Abs Dir)
  RemoveFile :: Path b File -> FileSystem m ()
  RenameFile :: Path b File -> Path b File -> FileSystem m ()
  RenamePath :: Path b t -> Path b t -> FileSystem m ()
  CopyFile :: Path b File -> Path b File -> FileSystem m ()
  CopyFileWithMetadata :: Path b File -> Path b File -> FileSystem m ()
  GetFileSize :: Path b File -> FileSystem m Integer
  CanonicalizePath :: (AnyPath p) => p -> FileSystem m (AbsPath p)
  MakeAbsolute :: (AnyPath p) => p -> FileSystem m (AbsPath p)
  MakeRelativeToCurrentDir :: (AnyPath p) => p -> FileSystem m (RelPath p)
  DoesPathExist :: Path b t -> FileSystem m Bool
  DoesFileExist :: Path b File -> FileSystem m Bool
  DoesDirExist :: Path b Dir -> FileSystem m Bool
  FindExecutable :: Path Rel File -> FileSystem m (Maybe (Path Abs File))
  FindFile :: [Path Abs Dir] -> Path Rel File -> FileSystem m (Maybe (Path Abs File))
  FindFiles :: [Path Abs Dir] -> Path Rel File -> FileSystem m [Path Abs File]
  FindFilesWith :: (Path Abs File -> m Bool) -> [Path a Dir] -> Path Rel File -> FileSystem m [Path Abs File]
  FindFileWith :: (Path Abs File -> m Bool) -> [Path Abs Dir] -> Path Rel File -> FileSystem m (Maybe (Path Abs File))
  CreateFileLink :: Path b File -> Path b File -> FileSystem m ()
  CreateDirLink :: Path b Dir -> Path b Dir -> FileSystem m ()
  RemoveDirLink :: Path b Dir -> FileSystem m ()
  IsSymlink :: Path b t -> FileSystem m Bool
  GetSymlinkTarget :: Path b t -> FileSystem m Text
  GetPermissions :: Path b t -> FileSystem m FSP.Permissions
  SetPermissions :: Path b t -> FSP.Permissions -> FileSystem m ()
  CopyPermissions :: Path b t -> Path b t -> FileSystem m ()
  GetAccessTime :: Path b t -> FileSystem m OffsetDatetime
  GetModificationTime :: Path b t -> FileSystem m OffsetDatetime
  SetAccessTime :: Path b t -> OffsetDatetime -> FileSystem m ()
  SetModificationTime :: Path b t -> OffsetDatetime -> FileSystem m ()
  ListDirRel :: Path Rel Dir -> FileSystem m ([Path Rel Dir], [Path Rel File])
  ListDirRecur :: Path b Dir -> FileSystem m ([Path Abs Dir], [Path Abs File])
  ListDirRecurRel :: Path Rel Dir -> FileSystem m ([Path Rel Dir], [Path Rel File])
  CopyDirRecur :: Path b Dir -> Path b Dir -> FileSystem m ()
  CopyDirRecur' :: Path b Dir -> Path b Dir -> FileSystem m ()
  WalkDir :: (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (FSP.WalkAction Abs)) -> Path b Dir -> FileSystem m ()
  WalkDirRel :: (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (FSP.WalkAction Rel)) -> Path Rel Dir -> FileSystem m ()
  -}
  
  WalkDirAccum :: (Monoid o) => Maybe (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (FSP.WalkAction Abs)) -> (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m o) -> Path b Dir -> FileSystem m o
  
  {-
  WalkDirAccumRel :: (Monoid o) => Maybe (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (FSP.WalkAction Rel)) -> (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m o) -> Path b Dir -> FileSystem m o
  ResolveFile :: Path Abs Dir -> Text -> FileSystem m (Path Abs File)
  ResolveFile' :: Text -> FileSystem m (Path Abs File)
  ResolveDir :: Path Abs Dir -> Text -> FileSystem m (Path Abs Dir)
  ResolveDir' :: Text -> FileSystem m (Path Abs Dir)
  WithTempFile :: Path Abs Dir -> Text -> (Path Abs File -> Handle -> m a) -> FileSystem m a
  WithTempDir :: Path Abs Dir -> Text -> (Path Abs Dir -> m a) -> FileSystem m a
  WithSystemTempFile :: Text -> (Path Abs File -> Handle -> m a) -> FileSystem m a
  WithSystemTempDir :: Text -> (Path Abs Dir -> m a) -> FileSystem m a
  OpenBinaryTempFile :: Path b Dir -> Text -> FileSystem m (Path Abs File, Handle)
  OpenTempFile :: Path b Dir -> Text -> FileSystem m (Path Abs File, Handle)
  CreateTempDir :: Path b Dir -> Text -> FileSystem m (Path Abs Dir)
  IsLocationOccupied :: Path b t -> FileSystem m Bool
  ForgivingAbsence :: m a -> FileSystem m (Maybe a)
  IgnoringAbsence :: m a -> FileSystem m ()
  -- from UnliftIO.IO.File
  WithBinaryFile :: Path a File -> IOMode -> (Handle -> m a) -> FileSystem m a
  WithBinaryFileAtomic :: Path a File -> IOMode -> (Handle -> m a) -> FileSystem m a
  WithBinaryFileDurable :: Path a File -> IOMode -> (Handle -> m a) -> FileSystem m a
  WithBinaryFileDurableAtomic :: Path a File -> IOMode -> (Handle -> m a) -> FileSystem m a
  EnsureFileDurable :: Path b File -> FileSystem m ()
  WriteBinaryFile :: Path b File -> ByteString -> FileSystem m ()
  WriteBinaryFileAtomic :: Path b File -> ByteString -> FileSystem m ()
  WriteBinaryFileDurable :: Path b File -> ByteString -> FileSystem m ()
  WriteBinaryFileDurableAtomic :: Path b File -> ByteString -> FileSystem m ()
  -}

-- todo: genrate splice and use makeEffect without type 
-- signatures add docs investigate renaming params
makeEffect ''FileSystem