{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.FileSystem.Effect (
  -- * Effect
  FileSystem (..),
  FSException (..),

  -- -- * Actions on directories
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

  -- -- ** Pre-defined directories
  R.XdgDirectory (..),
  R.XdgDirectoryList (..),
  R.exeExtension,
  -- -- ** Permissions
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
  -- -- ** Walking directory trees
  R.WalkAction (..),
  walkDir,
  walkDirRel,
  walkDirAccum,
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
  --
  
  --  writeBinaryFile
  -- , writeBinaryFileAtomic
  -- , writeBinaryFileDurable
  -- , writeBinaryFileDurableAtomic

  -- , withBinaryFile
  -- , withBinaryFileAtomic
  -- , withBinaryFileDurable
  -- , withBinaryFileDurableAtomic

  -- , ensureFileDurable
) where

import qualified DSL.FileSystem.IO.Raw.Internal as R
import Path (Abs, Dir, File, Path, Rel)
import Prelude (Bool (..), ByteString, Either (..), Exception, Handle, IO, IOMode, Integer, Maybe (..), Monoid, Show, Text, pure, ($), (&), (.), (<$>), (=<<), (==), (>>=), (||))
import qualified Prelude as P

import BasePrelude (IOException, last)
import Chronos (OffsetDatetime)
import Control.Monad.Catch (catch, handle)
import Effectful as EF (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  liftIO,
  type (:>),
 )
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static (unsafeLiftMapIO)
import Effectful.Error.Static as E
import Effectful.TH (makeEffect)
import Path.IO (AbsPath, AnyPath, RelPath)
import PyrethrumExtras (MonadMask, toS, txt, uu)
import qualified System.Directory as SD
import UnliftIO (UnliftIO, askUnliftIO)

-- TODO: hide relude exceptions add exceptions

data FileSystem :: Effect where
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
  GetXdgDir :: R.XdgDirectory -> Maybe (Path Rel Dir) -> FileSystem m (Path Abs Dir)
  GetXdgDirList :: R.XdgDirectoryList -> FileSystem m [Path Abs Dir]
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
  FindFilesWith :: (Path Abs File -> m Bool) -> [Path Abs Dir] -> Path Rel File -> FileSystem m [Path Abs File]
  FindFileWith :: (Path Abs File -> m Bool) -> [Path Abs Dir] -> Path Rel File -> FileSystem m (Maybe (Path Abs File))
  CreateFileLink :: Path b File -> Path b File -> FileSystem m ()
  CreateDirLink :: Path b Dir -> Path b Dir -> FileSystem m ()
  RemoveDirLink :: Path b Dir -> FileSystem m ()
  IsSymlink :: Path b t -> FileSystem m Bool
  GetSymlinkTarget :: Path b t -> FileSystem m Text
  GetPermissions :: Path b t -> FileSystem m R.Permissions
  SetPermissions :: Path b t -> R.Permissions -> FileSystem m ()
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
  WalkDir :: (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (R.WalkAction Abs)) -> Path b Dir -> FileSystem m ()
  WalkDirRel :: Path Rel Dir -> (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (R.WalkAction Rel)) -> FileSystem m ()
  WalkDirAccum :: (Monoid o) => Maybe (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (R.WalkAction Abs)) -> (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m o) -> Path b Dir -> FileSystem m o
  WalkDirAccumRel :: (Monoid o) => Maybe (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (R.WalkAction Rel)) -> (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m o) -> Path b Dir -> FileSystem m o
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

makeEffect ''FileSystem

type instance DispatchOf FileSystem = Dynamic

newtype FSException = FSException IOException
  deriving (Show)

instance Exception FSException