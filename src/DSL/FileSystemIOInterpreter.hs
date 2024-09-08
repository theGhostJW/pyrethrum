{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.FileSystemIOInterpreter (
  FileSystem,
  runFileSystem,
) where

import BasePrelude (IOException)
import Control.Monad.Catch (catch, handle)
import DSL.Internal.FileSystemRawIO qualified as R
import Effectful as EF (
  Eff,
  IOE,
  liftIO,
  type (:>),
 )

import DSL.FileSystemEffect (FSException (..), FileSystem (..))
import Effectful.Dispatch.Dynamic (
  LocalEnv,
  interpret,
  localSeqUnliftIO,
 )
import Effectful.Error.Static qualified as E

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
      withUnlifter :: forall b. ((forall r. Eff localEs r -> IO r) -> IO b) -> Eff es b
      withUnlifter h = handle (\(e :: IOException) -> E.throwError . FSException $ e) (localSeqUnliftIO env h)
     in
      case fs of
        {-
        WithCurrentDir path action -> withUnlifter $ \ul -> R.withCurrentDir path (ul action)
        FindFilesWith predicate searchDirs targetFileName -> withUnlifter $ \ul -> R.findFilesWith (ul . predicate) searchDirs targetFileName
        FindFileWith predicate searchDirs targetFileName -> withUnlifter $ \ul -> R.findFileWith (ul . predicate) searchDirs targetFileName
        CopyFileWithMetadata srcFile destFile -> withUnlifter $ const $ R.copyFileWithMetadata srcFile destFile
        WalkDir action dir -> withUnlifter $ \ul -> R.walkDir (\b drs -> ul . action b drs) dir
        WalkDirRel action dir -> withUnlifter $ \ul -> R.walkDirRel (\b drs -> ul . action b drs) dir
        -}

       {-

        WalkDirAccum descendHandler transformer startDir -> withUnlifter $ \ul ->
          let
            mdh' = (\dh b' drs -> ul . dh b' drs) <$> descendHandler
            ow' b' drs = ul . transformer b' drs
           in
            R.walkDirAccum mdh' ow' startDir
       
       
       
        
        WalkDirAccumRel descendHandler transformer startDir -> withUnlifter $ \ul ->
          let
            mdh' = (\dh b' drs -> ul . dh b' drs) <$> descendHandler
            ow' b' drs = ul . transformer b' drs
           in
            R.walkDirAccumRel mdh' ow' startDir
        WithTempFile parentDir dirTemplate action -> withUnlifter $ \ul -> R.withTempFile parentDir dirTemplate (\p -> ul . action p)
        WithTempDir parentDir dirTemplate action -> withUnlifter $ \ul -> R.withTempDir parentDir dirTemplate (ul . action)
        WithSystemTempFile dirTemplate action -> withUnlifter $ \ul -> R.withSystemTempFile dirTemplate (\p -> ul . action p)
        WithSystemTempDir dirTemplate f -> withUnlifter $ \ul -> R.withSystemTempDir dirTemplate (ul . f)
        ForgivingAbsence action -> withUnlifter $ \ul -> R.forgivingAbsence (ul action)
        IgnoringAbsence action -> withUnlifter $ \ul -> R.ignoringAbsence (ul action)
        WithBinaryFile path ioMode action -> withUnlifter $ \ul -> R.withBinaryFile path ioMode (ul . action)
        WithBinaryFileAtomic path ioMode action -> withUnlifter $ \ul -> R.withBinaryFileAtomic path ioMode (ul . action)
        WithBinaryFileDurable path ioMode action -> withUnlifter $ \ul -> R.withBinaryFileDurable path ioMode (ul . action)
        WithBinaryFileDurableAtomic path ioMode action -> withUnlifter $ \ul -> R.withBinaryFileDurableAtomic path ioMode (ul . action)
        _ -> adaptException $ case fs of
          EnsureDir dir -> R.ensureDir dir
          CreateDir dir -> R.createDir dir
          CreateDirIfMissing b dir -> R.createDirIfMissing b dir
          RemoveDir dir -> R.removeDir dir
          RemoveDirRecur dir -> R.removeDirRecur dir
          RemovePathForcibly path -> R.removePathForcibly path
          RenameDir old new -> R.renameDir old new
          ListDir dir -> R.listDir dir
          GetCurrentDir -> R.getCurrentDir
          SetCurrentDir dir -> R.setCurrentDir dir
          GetHomeDir -> R.getHomeDir
          GetXdgDir directoryType subDir -> R.getXdgDir directoryType subDir
          GetXdgDirList xdgList -> R.getXdgDirList xdgList
          GetAppUserDataDir subDir -> R.getAppUserDataDir subDir
          GetUserDocsDir -> R.getUserDocsDir
          GetTempDir -> R.getTempDir
          RemoveFile file -> R.removeFile file
          RenameFile old new -> R.renameFile old new
          RenamePath old new -> R.renamePath old new
          CopyFile old new -> R.copyFile old new
          GetFileSize file -> R.getFileSize file
          CanonicalizePath path -> R.canonicalizePath path
          MakeAbsolute path -> R.makeAbsolute path
          MakeRelativeToCurrentDir path -> R.makeRelativeToCurrentDir path
          DoesPathExist path -> R.doesPathExist path
          DoesFileExist file -> R.doesFileExist file
          DoesDirExist dir -> R.doesDirExist dir
          FindExecutable file -> R.findExecutable file
          FindFile searchDirs files -> R.findFile searchDirs files
          FindFiles searchDirs files -> R.findFiles searchDirs files
          CreateFileLink old new -> R.createFileLink old new
          CreateDirLink old new -> R.createDirLink old new
          RemoveDirLink dir -> R.removeDirLink dir
          IsSymlink path -> R.isSymlink path
          GetSymlinkTarget path -> R.getSymlinkTarget path
          GetPermissions path -> R.getPermissions path
          SetPermissions path ps -> R.setPermissions path ps
          CopyPermissions old new -> R.copyPermissions old new
          GetAccessTime path -> R.getAccessTime path
          GetModificationTime path -> R.getModificationTime path
          SetAccessTime path time -> R.setAccessTime path time
          SetModificationTime path time -> R.setModificationTime path time
          ListDirRel dir -> R.listDirRel dir
          ListDirRecur dir -> R.listDirRecur dir
          ListDirRecurRel dir -> R.listDirRecurRel dir
          CopyDirRecur old new -> R.copyDirRecur old new
          CopyDirRecur' old new -> R.copyDirRecur' old new
          ResolveFile parentDir fileName -> R.resolveFile parentDir fileName
          ResolveFile' fileName -> R.resolveFile' fileName
          ResolveDir parentDir dirName -> R.resolveDir parentDir dirName
          ResolveDir' dirName -> R.resolveDir' dirName
          OpenBinaryTempFile parentDir fileName -> R.openBinaryTempFile parentDir fileName
          OpenTempFile parentDir fileName -> R.openTempFile parentDir fileName
          CreateTempDir parentDir dirName -> R.createTempDir parentDir dirName
          IsLocationOccupied path -> R.isLocationOccupied path
          EnsureFileDurable file -> R.ensureFileDurable file
          WriteBinaryFile file byteString -> R.writeBinaryFile file byteString
          WriteBinaryFileAtomic file byteString -> R.writeBinaryFileAtomic file byteString
          WriteBinaryFileDurable file byteString -> R.writeBinaryFileDurable file byteString
          WriteBinaryFileDurableAtomic file byteString -> R.writeBinaryFileDurableAtomic file byteString
          -}
