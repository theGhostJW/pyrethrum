-- remapping of Path.IO with some minor type changes to be used by both static and dynamic effects

module DSL.FileSystem.IO.Internal.Raw (
  -- * Actions on directories
  D.createDir ,
  D.createDirIfMissing ,
  D.removeDir ,
  D.removeDirRecur ,
  D.removePathForcibly ,
  D.renameDir ,
  D.listDir ,

  -- ** Current working directory
  D.getCurrentDir ,
  D.setCurrentDir ,
  D.withCurrentDir ,

  -- * Pre-defined directories
  D.getHomeDir ,
  D.getXdgDir ,
  D.getXdgDirList ,
  getAppUserDataDir ,
  D.getUserDocsDir ,
  D.getTempDir ,

  -- * Actions on files
  D.removeFile ,
  D.renameFile ,
  D.renamePath ,
  D.copyFile ,
  copyFileWithMetadata ,
  D.getFileSize ,
  D.canonicalizePath ,
  D.makeAbsolute ,
  D.makeRelativeToCurrentDir ,

  -- * Existence tests
  D.doesPathExist ,
  D.doesFileExist ,
  D.doesDirExist ,
  D.findExecutable ,
  D.findFile ,
  D.findFiles ,
  findFileWith ,
  D.findFilesWith ,

  -- * Symbolic links
  D.createFileLink ,
  D.createDirLink ,
  D.removeDirLink ,
  D.isSymlink ,
  getSymlinkTarget ,

  -- * Permissions
  D.getPermissions ,
  D.setPermissions ,
  D.copyPermissions ,

  -- * Timestamps
  getAccessTime ,
  getModificationTime ,
  setAccessTime ,
  setModificationTime ,

  -- * Re-exports

  -- ** Pre-defined directories
  D.XdgDirectory (..) ,
  D.XdgDirectoryList (..) ,

  -- ** Existence tests
  exeExtension ,

  -- ** Permissions
  D.Permissions ,
  D.emptyPermissions ,
  D.readable ,
  D.writable ,
  D.executable ,
  D.searchable ,
  D.setOwnerReadable ,
  D.setOwnerWritable ,
  D.setOwnerExecutable ,
  D.setOwnerSearchable ,
  -- from pathIO
  D.ensureDir ,
  D.listDirRel ,
  D.listDirRecur ,
  D.listDirRecurRel ,
  D.copyDirRecur ,
  D.copyDirRecur' ,

  -- ** Walking directory trees
  D.WalkAction (..) ,
  D.walkDir ,
  D.walkDirRel ,
  D.walkDirAccum ,
  D.walkDirAccumRel ,

  -- * Path b t transformation
  resolveFile ,
  resolveFile' ,
  resolveDir ,
  resolveDir' ,

  -- * Temporary files and directories
  withTempFile ,
  withTempDir ,
  withSystemTempFile ,
  withSystemTempDir ,
  openTempFile ,
  openBinaryTempFile ,
  createTempDir ,

  -- * Existence tests
  D.isLocationOccupied ,
  D.forgivingAbsence ,
  D.ignoringAbsence ,

  -- * from UnliftIO
  writeBinaryFile  ,
  writeBinaryFileAtomic ,
  writeBinaryFileDurable ,
  writeBinaryFileDurableAtomic ,

  withBinaryFile ,
  withBinaryFileAtomic ,
  withBinaryFileDurable ,
  withBinaryFileDurableAtomic ,

  ensureFileDurable ,
) where

import Data.Time (UTCTime)
import Path
import Path.IO (AnyPath (..))
import qualified Path.IO as D
import Prelude (Bool (..),IO,Integer,Maybe (..),MonadIO,Text,fmap,($),(.),(<$>),(==),(>>=),(||), ByteString, flip, IOMode, Handle)
import qualified Prelude as P

import Chronos (OffsetDatetime)
import PyrethrumExtras (MonadCatch,MonadMask,toS)
import qualified System.Directory as SD
import TempUtils (offsetDateTimeToUtc,utcToOffsetDateTime)
import qualified UnliftIO.IO.File as ULF
import UnliftIO (MonadUnliftIO)

findFileWith ::
  (Path Abs File -> IO Bool) ->
  [Path b Dir] ->
  Path Rel File ->
  IO (Maybe (Path Abs File))
findFileWith p dirs n =
  (>>= parseAbsFile) <$> SD.findFileWith p' (toFilePath <$> dirs) (toFilePath n)
 where
  p' :: P.FilePath -> IO Bool
  p' f = parseAbsFile f >>= p

exeExtension :: Text
exeExtension = toS SD.exeExtension

getAppUserDataDir ::
  (MonadIO m) =>
  Text ->
  m (Path Abs Dir)
getAppUserDataDir = D.getAppUserDataDir . toS

copyFileWithMetadata :: Path b File -> Path b0 File -> IO ()
copyFileWithMetadata src dst = SD.copyFileWithMetadata (toFilePath src) (toFilePath dst)

getSymlinkTarget ::
  (MonadIO m) =>
  Path b t ->
  m Text
getSymlinkTarget = P.fmap toS . D.getSymlinkTarget

getAccessTime :: (MonadIO m) => Path b t -> m OffsetDatetime
getAccessTime = toOffset . D.getAccessTime

getModificationTime :: (MonadIO m) => Path b t -> m OffsetDatetime
getModificationTime = toOffset . D.getModificationTime

setAccessTime :: (MonadIO m) => Path b t -> OffsetDatetime -> m ()
setAccessTime p = D.setAccessTime p . offsetDateTimeToUtc

setModificationTime :: (MonadIO m) => Path b t -> OffsetDatetime -> m ()
setModificationTime p = D.setModificationTime p . offsetDateTimeToUtc

toOffset :: (P.Functor f) => f UTCTime -> f OffsetDatetime
toOffset = fmap utcToOffsetDateTime

resolveFile :: (MonadIO m) => Path Abs Dir -> Text -> m (Path Abs File)
resolveFile b = D.resolveFile b . toS

resolveFile' ::
  (MonadIO m) =>
  Text ->
  m (Path Abs File)
resolveFile' = D.resolveFile' . toS

resolveDir ::
  (MonadIO m) =>
  Path Abs Dir ->
  Text ->
  m (Path Abs Dir)
resolveDir b = D.resolveDir b . toS

resolveDir' ::
  (MonadIO m) =>
  Text ->
  m (Path Abs Dir)
resolveDir' = D.resolveDir' . toS

withTempFile ::
  (MonadIO m,MonadMask m) =>
  Path b Dir ->
  Text ->
  (Path Abs File -> P.Handle -> m a) ->
  m a
withTempFile path = D.withTempFile path . toS

withTempDir ::
  (MonadIO m,MonadMask m) =>
  Path b Dir ->
  Text ->
  (Path Abs Dir -> m a) ->
  m a
withTempDir path = D.withTempDir path . toS

withSystemTempFile ::
  (MonadIO m,MonadMask m) =>
  Text ->
  (Path Abs File -> P.Handle -> m a) ->
  m a
withSystemTempFile = D.withSystemTempFile . toS

withSystemTempDir ::
  (MonadIO m,MonadMask m) =>
  Text ->
  (Path Abs Dir -> m a) ->
  m a
withSystemTempDir t = D.withSystemTempDir (toS t)

openTempFile ::
  (MonadIO m) =>
  Path b Dir ->
  Text ->
  m (Path Abs File,P.Handle)
openTempFile p = D.openTempFile p . toS

openBinaryTempFile ::
  (MonadIO m) =>
  Path b Dir ->
  Text ->
  m (Path Abs File,P.Handle)
openBinaryTempFile p = D.openBinaryTempFile p . toS

createTempDir ::
  (MonadIO m) =>
  Path b Dir ->
  Text ->
  m (Path Abs Dir)
createTempDir p = D.createTempDir p . toS

-- from UnliftIO.IO.File

writeBinaryFile :: MonadIO m => Path a File -> ByteString -> m () 
writeBinaryFile = ULF.writeBinaryFile . toFilePath

writeBinaryFileAtomic :: MonadIO m => Path a File -> ByteString -> m ()
writeBinaryFileAtomic = ULF.writeBinaryFileAtomic . toFilePath

writeBinaryFileDurable :: MonadIO m => Path a File -> ByteString -> m ()
writeBinaryFileDurable = ULF.writeBinaryFileDurable . toFilePath

writeBinaryFileDurableAtomic :: MonadIO m => Path a File -> ByteString -> m ()
writeBinaryFileDurableAtomic = ULF.writeBinaryFileDurableAtomic . toFilePath

withBinaryFile :: MonadUnliftIO m  => Path a File -> IOMode -> (Handle -> m b) -> m b
withBinaryFile = ULF.withBinaryFile . toFilePath

withBinaryFileAtomic :: MonadUnliftIO m  => Path a File -> IOMode -> (Handle -> m b) -> m b
withBinaryFileAtomic = ULF.withBinaryFileAtomic . toFilePath 

withBinaryFileDurable :: MonadUnliftIO m  => Path a File -> IOMode -> (Handle -> m b) -> m b
withBinaryFileDurable = ULF.withBinaryFileDurable . toFilePath

withBinaryFileDurableAtomic :: MonadUnliftIO m  => Path a File -> IOMode -> (Handle -> m b) -> m b
withBinaryFileDurableAtomic = ULF.withBinaryFileDurableAtomic . toFilePath 

ensureFileDurable :: (MonadIO m) => Path b File -> m ()
ensureFileDurable = ULF.ensureFileDurable . toFilePath