{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.FileSystemDynamic (
  -- * Effect
  FileSystem,
  -- -- ** Handlers
  runFileSystem,
  -- -- * Actions on directories
  createDir,
  -- createDirIfMissing,
  -- removeDir,
  -- removeDirRecur,
  -- removePathForcibly,
  -- renameDir,
  -- listDir,

  -- -- ** Current working directory
  -- getCurrentDir,
  -- setCurrentDir,
  -- withCurrentDir,

  -- -- * Pre-defined directories
  -- getHomeDir,
  -- getXdgDir,
  -- getXdgDirList,
  -- getAppUserDataDir,
  -- getUserDocsDir,
  -- getTempDir,

  -- -- * Actions on files
  -- removeFile,
  -- renameFile,
  -- renamePath,
  -- copyFile,
  -- copyFileWithMetadata,
  -- getFileSize,
  -- canonicalizePath,
  -- makeAbsolute,
  -- makeRelativeToCurrentDir,

  -- -- * Existence tests
  -- doesPathExist,
  -- doesFileExist,
  -- doesDirExist,
  -- findExecutable,
  -- findFile,
  -- findFiles,
  -- -- , findFileWith
  -- findFilesWith,

  -- -- * Symbolic links
  -- createFileLink,
  -- createDirLink,
  -- removeDirLink,
  -- isSymlink,
  -- getSymlinkTarget,

  -- -- * Permissions
  -- getPermissions,
  -- setPermissions,
  -- copyPermissions,

  -- -- * Timestamps
  -- getAccessTime,
  -- getModificationTime,
  -- setAccessTime,
  -- setModificationTime,

  -- -- * Re-exports

  -- -- ** Pre-defined directories
  -- R.XdgDirectory (..),
  -- R.XdgDirectoryList (..),
  -- R.exeExtension,

  -- -- ** Permissions
  -- R.Permissions,
  -- R.emptyPermissions,
  -- R.readable,
  -- R.writable,
  -- R.executable,
  -- R.searchable,
  -- R.setOwnerReadable,
  -- R.setOwnerWritable,
  -- R.setOwnerExecutable,
  -- R.setOwnerSearchable,
  -- -- from pathIO
  ensureDir,
  -- listDirRel,
  -- listDirRecur,
  -- listDirRecurRel,
  -- copyDirRecur,
  -- copyDirRecur',

  -- -- ** Walking directory trees
  -- R.WalkAction (..),
  -- walkDir,
  -- walkDirRel,
  -- walkDirAccum,
  -- walkDirAccumRel,

  -- -- * Path b t transformation
  -- resolveFile,
  -- resolveFile',
  -- resolveDir,
  -- resolveDir',

  -- -- * Temporary files and directories
  -- withTempFile,
  -- withTempDir,
  -- withSystemTempFile,
  -- withSystemTempDir,
  -- openTempFile,
  -- openBinaryTempFile,
  -- createTempDir,

  -- -- * Existence tests
  -- isLocationOccupied,
  -- forgivingAbsence,
  -- ignoringAbsence,
) where

import qualified DSL.FileSystem.Raw as R
import Path
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
  -- WithTempFile :: Path Abs Dir -> Text -> (Path Abs File -> Handle -> FileSystem m a) -> FileSystem m a
  -- WithTempDir :: Path Abs Dir -> Text -> (Path Abs Dir -> FileSystem m a) -> FileSystem m a
  -- WithSystemTempFile :: Text -> (Path Abs File -> Handle -> FileSystem m a) -> FileSystem m a
  -- WithSystemTempDir :: Text -> (Path Abs Dir -> FileSystem m a) -> FileSystem m a
  OpenBinaryTempFile :: Path b Dir -> Text -> FileSystem m (Path Abs File, Handle)
  OpenTempFile :: Path b Dir -> Text -> FileSystem m (Path Abs File, Handle)
  CreateTempDir :: Path b Dir -> Text -> FileSystem m (Path Abs Dir)
  IsLocationOccupied :: Path b t -> FileSystem m Bool

-- ForgiveAbsence :: FileSystem m a -> FileSystem m (Maybe a)
-- IgnoreAbsence :: FileSystem m a -> FileSystem m ()

makeEffect ''FileSystem

type instance DispatchOf FileSystem = Dynamic

newtype FSException = FSException IOException
  deriving (Show)

instance Exception FSException

adaptException :: (HasCallStack, IOE :> es, E.Error FSException :> es) => IO b -> Eff es b
adaptException m = EF.liftIO m `catch` \(e :: IOException) -> throwError . FSException $ e

{-

interpret $ \_ ->
  adaptException . \case
    EnsureDir p -> R.ensureDir p

-}

runFileSystem :: forall es a. (HasCallStack, IOE :> es, E.Error FSException :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystem =
  interpret handler
 where
  handler ::
    forall a' localEs.
    (HasCallStack, FileSystem :> localEs) =>
    LocalEnv localEs es ->
    FileSystem (Eff localEs) a' ->
    Eff es a'
  handler env fs =
    let
      hoe :: forall b. ((forall r. Eff localEs r -> IO r) -> IO b) -> Eff es b
      hoe h = handle (\(e :: IOException) -> throwError . FSException $ e) (localSeqUnliftIO env h)
     in
      case fs of
        WithCurrentDir p action -> hoe $ \ul -> R.withCurrentDir p (ul action)
        FindFilesWith f ds t -> hoe $ \ul -> R.findFilesWith (ul . f) ds t
        FindFileWith f ds t -> hoe $ \ul -> R.findFileWith (ul . f) ds t
        CopyFileWithMetadata o n -> hoe $ \ul -> R.copyFileWithMetadata o n
        WalkDir h p -> hoe $ \ul -> R.walkDir (\b drs -> ul . h b drs) p
        WalkDirRel p h -> hoe $ \ul -> R.walkDirRel (\b drs -> ul . h b drs) p
        WalkDirAccum mdh ow b -> hoe $ \ul ->
          let
            mdh' = (\dh b' drs -> ul . dh b' drs) <$> mdh
            ow' b' drs = ul . ow b' drs
           in
            R.walkDirAccum mdh' ow' b
        WalkDirAccumRel mdh ow b -> hoe $ \ul ->
          let
            mdh' = (\dh b' drs -> ul . dh b' drs) <$> mdh
            ow' b' drs = ul . ow b' drs
           in
            R.walkDirAccumRel mdh' ow' b
        -- WithTempFile d t f -> R.withTempFile d t f
        -- WithTempDir d t f -> R.withTempDir d t f
        -- WithSystemTempFile t f -> R.withSystemTempFile t f
        -- WithSystemTempDir t f -> R.withSystemTempDir t f
        -- ReadBinaryFile p -> R.readBinaryFile p
        -- ForgiveAbsence m -> R.forgivingAbsence m
        -- IgnoreAbsence m -> R.ignoreAbsence m
        _ -> adaptException $ case fs of
          EnsureDir p -> R.ensureDir p
          CreateDir d -> R.createDir d
          CreateDirIfMissing b d -> R.createDirIfMissing b d
          RemoveDir d -> R.removeDir d
          RemoveDirRecur d -> R.removeDirRecur d
          RemovePathForcibly p -> R.removePathForcibly p
          RenameDir o n -> R.renameDir o n
          ListDir d -> R.listDir d
          GetCurrentDir -> R.getCurrentDir
          SetCurrentDir d -> R.setCurrentDir d
          GetHomeDir -> R.getHomeDir
          GetXdgDir xd bd -> R.getXdgDir xd bd
          GetXdgDirList l -> R.getXdgDirList l
          GetAppUserDataDir d -> R.getAppUserDataDir d
          GetUserDocsDir -> R.getUserDocsDir
          GetTempDir -> R.getTempDir
          RemoveFile f -> R.removeFile f
          RenameFile o n -> R.renameFile o n
          RenamePath o n -> R.renamePath o n
          CopyFile o n -> R.copyFile o n
          GetFileSize f -> R.getFileSize f
          CanonicalizePath p -> R.canonicalizePath p
          MakeAbsolute p -> R.makeAbsolute p
          MakeRelativeToCurrentDir p -> R.makeRelativeToCurrentDir p
          DoesPathExist p -> R.doesPathExist p
          DoesFileExist f -> R.doesFileExist f
          DoesDirExist d -> R.doesDirExist d
          FindExecutable t -> R.findExecutable t
          FindFile ds t -> R.findFile ds t
          FindFiles ds t -> R.findFiles ds t
          CreateFileLink o n -> R.createFileLink o n
          CreateDirLink o n -> R.createDirLink o n
          RemoveDirLink d -> R.removeDirLink d
          IsSymlink p -> R.isSymlink p
          GetSymlinkTarget p -> R.getSymlinkTarget p
          GetPermissions p -> R.getPermissions p
          SetPermissions p ps -> R.setPermissions p ps
          CopyPermissions o n -> R.copyPermissions o n
          GetAccessTime p -> R.getAccessTime p
          GetModificationTime p -> R.getModificationTime p
          SetAccessTime p t -> R.setAccessTime p t
          SetModificationTime p t -> R.setModificationTime p t
          ListDirRel d -> R.listDirRel d
          ListDirRecur d -> R.listDirRecur d
          ListDirRecurRel d -> R.listDirRecurRel d
          CopyDirRecur o n -> R.copyDirRecur o n
          CopyDirRecur' o n -> R.copyDirRecur' o n
          ResolveFile ds f -> R.resolveFile ds f
          ResolveFile' f -> R.resolveFile' f
          ResolveDir ds d -> R.resolveDir ds d
          ResolveDir' f -> R.resolveDir' f

          OpenBinaryTempFile p t -> R.openBinaryTempFile p t
          OpenTempFile p t -> R.openTempFile p t
          CreateTempDir p t -> R.createTempDir p t
          IsLocationOccupied p -> R.isLocationOccupied p

----------------------------------------
-- path-io only

----------------------------------------------------------------------------
{-

{- | If argument of the function throws a
'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
exceptions propagate). Otherwise the result is returned inside a 'Just'.

@since 0.3.0
-}
forgivingAbsence :: (FileSystem :> es) => IO a -> Eff es (Maybe a)
forgivingAbsence = unsafeEff_ . R.forgivingAbsence

{- | The same as 'forgivingAbsence', but ignores result.

@since 0.3.1
-}
ignoringAbsence :: (FileSystem :> es) => IO a -> Eff es ()
ignoringAbsence = unsafeEff_ . R.ignoringAbsence

----------------------------------------
-- Current working directory

-- | Lifted 'R.withCurrentDir'.
withCurrentDir :: (FileSystem :> es) => Path b Dir -> Eff es a -> Eff es a
withCurrentDir path = unsafeLiftMapIO (R.withCurrentDir path)

-- | Lifted 'R.findFileWith'.
findFileWith ::
  (FileSystem :> es) =>
  (Path Abs File -> Eff es Bool) ->
  [Path b Dir] ->
  Path Rel File ->
  Eff es (Maybe (Path Abs File))
findFileWith p dirs ns = unsafeSeqUnliftIO $ \unlift -> R.findFileWith (unlift . p) dirs ns

-- | Lifted 'R.findFilesWith'.
findFilesWith ::
  (FileSystem :> es) =>
  (Path Abs File -> Eff es Bool) ->
  [Path b Dir] ->
  Path Rel File ->
  Eff es [Path Abs File]
findFilesWith p dirs ns = unsafeSeqUnliftIO $ \unlift -> do
  R.findFilesWith (unlift . p) dirs ns

-}
