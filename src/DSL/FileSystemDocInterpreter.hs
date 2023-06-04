{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.FileSystemDocInterpreter (
  FileSystem,
  runFileSystem,
) where

import BasePrelude (IOException)
import qualified DSL.Internal.FileSystemRawIO as R
import Control.Monad.Catch (catch, handle)
import Effectful as EF (
  Eff,
  IOE,
  liftIO,
  type (:>),
 )

import DSL.FileSystemEffect (FSException (..), FileSystem (..))
import Effectful.Dispatch.Dynamic (
  HasCallStack,
  LocalEnv,
  interpret,
  localSeqUnliftIO,
 )
import qualified Effectful.Error.Static as E

adaptException :: (HasCallStack, IOE :> es, E.Error FSException :> es) => IO b -> Eff es b
adaptException m = EF.liftIO m `catch` \(e :: IOException) -> E.throwError . FSException $ e

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
      hoe h = handle (\(e :: IOException) -> E.throwError . FSException $ e) (localSeqUnliftIO env h)
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
        WithTempFile d t f -> hoe $ \ul -> R.withTempFile d t (\p -> ul . f p)
        WithTempDir d t f -> hoe $ \ul -> R.withTempDir d t (ul . f)
        WithSystemTempFile t f -> hoe $ \ul -> R.withSystemTempFile t (\p -> ul . f p)
        WithSystemTempDir t f -> hoe $ \ul -> R.withSystemTempDir t (ul . f)
        ForgivingAbsence m -> hoe $ \ul -> R.forgivingAbsence (ul m)
        IgnoringAbsence m -> hoe $ \ul -> R.ignoringAbsence (ul m)
        WithBinaryFile p m f -> hoe $ \ul -> R.withBinaryFile p m (ul . f)
        WithBinaryFileAtomic p m f -> hoe $ \ul -> R.withBinaryFileAtomic p m (ul . f)
        WithBinaryFileDurable p m f -> hoe $ \ul -> R.withBinaryFileDurable p m (ul . f)
        WithBinaryFileDurableAtomic p m f -> hoe $ \ul -> R.withBinaryFileDurableAtomic p m (ul . f)
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
          EnsureFileDurable p -> R.ensureFileDurable p
          WriteBinaryFile p bs -> R.writeBinaryFile p bs
          WriteBinaryFileAtomic p bs -> R.writeBinaryFileAtomic p bs
          WriteBinaryFileDurable p bs -> R.writeBinaryFileDurable p bs
          WriteBinaryFileDurableAtomic p bs -> R.writeBinaryFileDurableAtomic p bs
