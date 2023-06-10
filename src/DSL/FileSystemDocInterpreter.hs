{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.FileSystemDocInterpreter (
  ) where

-- FileSystem,
-- runFileSystem,

import BasePrelude (IOException)
import Control.Monad.Catch (catch, handle)
import qualified DSL.Internal.FileSystemPure as FSP
import qualified DSL.Internal.FileSystemRawIO as R
import DSL.Out
import Effectful as EF (
  Eff,
  IOE,
  liftIO,
  type (:>),
 )

import DSL.FileSystemEffect (FileSystem (..))
import DSL.Internal.ApEvent (ApEvent (..))
import Effectful.Dispatch.Dynamic (
  HasCallStack,
  LocalEnv,
  interpret,
  localSeqUnliftIO,
 )
import qualified Effectful.Error.Static as E
import GHC.TypeError (ErrorMessage (Text))
import Path.Extended (Path, toFilePath)
import PyrethrumExtras (toS, txt, uu)

data DocException = DocException Text SomeException
  deriving (Show)

instance Exception DocException

adaptException :: (HasCallStack, IOE :> es, E.Error DocException :> es) => IO b -> Eff es b
adaptException m = EF.liftIO m `catch` \(e :: SomeException) -> E.throwError . DocException "Exception thrown in documenter" $ e

runFileSystem :: forall es a. (HasCallStack, IOE :> es, Out ApEvent :> es, Out a :> es, E.Error DocException :> es) => Eff (FileSystem : es) a -> Eff es a
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
    case fs of
      -- WithCurrentDir p action -> hoe $ \ul -> R.withCurrentDir p (ul action)
      -- FindFilesWith f ds t -> hoe $ \ul -> R.findFilesWith (ul . f) ds t
      -- FindFileWith f ds t -> hoe $ \ul -> R.findFileWith (ul . f) ds t
      -- CopyFileWithMetadata o n -> hoe $ \ul -> R.copyFileWithMetadata o n
      -- WalkDir h p -> hoe $ \ul -> R.walkDir (\b drs -> ul . h b drs) p
      -- WalkDirRel p h -> hoe $ \ul -> R.walkDirRel (\b drs -> ul . h b drs) p
      -- WalkDirAccum mdh ow b -> hoe $ \ul ->
      --   let
      --     mdh' = (\dh b' drs -> ul . dh b' drs) <$> mdh
      --     ow' b' drs = ul . ow b' drs
      --    in
      --     R.walkDirAccum mdh' ow' b
      -- WalkDirAccumRel mdh ow b -> hoe $ \ul ->
      --   let
      --     mdh' = (\dh b' drs -> ul . dh b' drs) <$> mdh
      --     ow' b' drs = ul . ow b' drs
      --    in
      --     R.walkDirAccumRel mdh' ow' b
      -- WithTempFile d t f -> hoe $ \ul -> R.withTempFile d t (\p -> ul . f p)
      -- WithTempDir d t f -> hoe $ \ul -> R.withTempDir d t (ul . f)
      -- WithSystemTempFile t f -> hoe $ \ul -> R.withSystemTempFile t (\p -> ul . f p)
      -- WithSystemTempDir t f -> hoe $ \ul -> R.withSystemTempDir t (ul . f)
      -- ForgivingAbsence m -> hoe $ \ul -> R.forgivingAbsence (ul m)
      -- IgnoringAbsence m -> hoe $ \ul -> R.ignoringAbsence (ul m)
      -- WithBinaryFile p m f -> hoe $ \ul -> R.withBinaryFile p m (ul . f)
      -- WithBinaryFileAtomic p m f -> hoe $ \ul -> R.withBinaryFileAtomic p m (ul . f)
      -- WithBinaryFileDurable p m f -> hoe $ \ul -> R.withBinaryFileDurable p m (ul . f)
      -- WithBinaryFileDurableAtomic p m f -> hoe $ \ul -> R.withBinaryFileDurableAtomic p m (ul . f)
      -- GetCurrentDir -> R.getCurrentDir
      -- GetHomeDir -> R.getHomeDir
      -- GetXdgDir xd bd -> R.getXdgDir xd bd
      -- GetXdgDirList l -> R.getXdgDirList l
      -- GetAppUserDataDir d -> R.getAppUserDataDir d
      -- GetUserDocsDir -> R.getUserDocsDir
      -- GetTempDir -> R.getTempDir
      -- GetFileSize f -> R.getFileSize f
      -- DoesPathExist p -> R.doesPathExist p
      -- DoesFileExist f -> R.doesFileExist f
      -- DoesDirExist d -> R.doesDirExist d
      -- CanonicalizePath p -> R.canonicalizePath p
      -- MakeAbsolute p -> R.makeAbsolute p
      -- MakeRelativeToCurrentDir p -> R.makeRelativeToCurrentDir p
      -- FindExecutable t -> R.findExecutable t
      -- FindFile ds t -> R.findFile ds t
      -- FindFiles ds t -> R.findFiles ds t
      -- IsSymlink p -> R.isSymlink p
      -- GetSymlinkTarget p -> R.getSymlinkTarget p
      -- GetPermissions p -> R.getPermissions p
      -- GetAccessTime p -> R.getAccessTime p
      -- GetModificationTime p -> R.getModificationTime p
      -- ListDirRel d -> R.listDirRel d
      -- ListDirRecur d -> R.listDirRecur d
      -- ListDirRecurRel d -> R.listDirRecurRel d
      -- ResolveFile ds f -> R.resolveFile ds f
      -- ResolveFile' f -> R.resolveFile' f
      -- ResolveDir ds d -> R.resolveDir ds d
      -- ResolveDir' f -> R.resolveDir' f
      -- OpenBinaryTempFile p t -> R.openBinaryTempFile p t
      -- OpenTempFile p t -> R.openTempFile p t
      -- CreateTempDir p t -> R.createTempDir p t
        -- IsLocationOccupied p -> R.isLocationOccupied p
      _ -> out $ Step $ case fs of
        EnsureDir p -> dirInfo "ensure directory exists" p
        CreateDir d -> dirInfo "create directory" d
        CreateDirIfMissing _ d -> dirInfo "create directory if missing" d
        RemoveDir d -> dirInfo "remove directory" d
        RemoveDirRecur d -> dirInfo "remove directory recursively" d
        RemovePathForcibly p -> dirInfo "remove path forcibly" p
        RenameDir o n -> dirInfo' "rename directory from" o "to" n
        ListDir d -> dirInfo "list directory" d
        SetCurrentDir d -> dirInfo "set current directory" d
        RemoveFile f -> dirInfo "remove file" f
        RenameFile o n -> dirInfo' "rename file from:" o "to" n
        RenamePath o n -> dirInfo' "rename path from:" o "to" n
        CopyFile o n -> dirInfo' "copy file from:" o "to" n
        CreateFileLink o n -> dirInfo' "create file link from" o "to" n
        CreateDirLink o n -> dirInfo' "create directory link from" o "to" n
        RemoveDirLink d -> dirInfo "remove directory link" d
        SetPermissions p ps -> info' "set permissions of" p "to" ps
        CopyPermissions o n -> info' "copy permissions from" o "to" n
        SetAccessTime p t -> info' "set access time of" p "to" t
        SetModificationTime p t -> info' "set modification time of" p "to" t
        CopyDirRecur o n -> dirInfo' "copy directory recursively from" o "to" n
        CopyDirRecur' o n -> dirInfo' "copy directory recursively from" o "to" n
        EnsureFileDurable p -> dirInfo "ensure file durable (non-windows only)" p
        WriteBinaryFile p bs -> dirInfo "write binary file contents" p 
        WriteBinaryFileAtomic p bs -> dirInfo "write binary file contents atomically" p
        WriteBinaryFileDurable p bs -> dirInfo "write binary file contents" p 
        WriteBinaryFileDurableAtomic p bs -> dirInfo "write binary file contents atomically" p
   where
    -- hoe :: forall b. ((forall r. Eff localEs r -> IO r) -> IO b) -> Eff es b
    -- hoe h = uu -- handle (\(e :: IOException) -> E.throwError . FSException $ e) (localSeqUnliftIO env h)
    --
    info :: (Show o) => Text -> o -> Text
    info prefix o = prefix <> ": " <> txt o

    info' :: (Show o, Show o2) => Text -> o -> Text -> o2 -> Text
    info' prefix o sep o2 = prefix <> ": " <> txt o <> sep <> ": " <> txt o2

    dirInfo :: forall c d. Text -> Path c d -> Text
    dirInfo prefix = info prefix . toFilePath

    dirInfo' :: forall c d e f. Text -> Path c d -> Text -> Path e f -> Text
    dirInfo' prefix p sep p2 = info' prefix (toFilePath p) sep (toFilePath p2)



