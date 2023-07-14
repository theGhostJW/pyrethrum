{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.FileSystemDocInterpreter (
  runFileSystem,
  DocException,
  adaptException,
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
import qualified Data.Text as T
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

data DocException
  = DocException Text
  | DocException' Text SomeException
  deriving (Show)

instance Exception DocException

adaptException :: forall es a. (HasCallStack, IOE :> es, E.Error DocException :> es) => IO a -> Eff es a
adaptException m = EF.liftIO m `catch` \(e :: SomeException) -> E.throwError . DocException' "Exception thrown in documenter" $ e

-- TODO:
-- sort out lazy IO
-- simple console effect
--   - add deferred validation
-- read file effect
--  - repro issue
--  - solve issue
-- finish doc file system
--   - demo simple efffect app including returning a doc value exception IO and step listing

runFileSystem :: forall es a. (HasCallStack, IOE :> es, Out ApEvent :> es, E.Error DocException :> es) => Eff (FileSystem : es) a -> Eff es a
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
      -- todo: rename all variables / separate type signatures by using the other templateHaskell method
      WithCurrentDir path action -> docErr "withCurrentDir" "run action in current working directory"
      FindFilesWith predicate searchDirs targetFileName ->
        docErr4
          "findFilesWith"
          "find all files that match the file name:"
          (showPath targetFileName)
          "and satisfy the given predicate in directories:"
          (showPaths searchDirs)
      FindFileWith predicate searchDirs targetFileName ->
        docErr4
          "findFileWith"
          "find the first file that matches the file name:"
          (showPath targetFileName)
          "and satisfies the given predicate in directories:"
          (showPaths searchDirs)
      CopyFileWithMetadata srcFile destFile ->
        docErr4
          "copyFileWithMetadata"
          "copy file:"
          (showPath srcFile)
          "with metadata to:"
          (showPath destFile)
      WalkDir action dir ->
        docErr3
          "walkDir"
          "recurssively walk the directory:"
          (showPath dir)
          "performing an action on each subdirectory"
      WalkDirRel action path ->
        docErr3
          "walkDirRel"
          "recurssively walk the directory:"
          (showPath path)
          "performing an action on each subdirectory"
      WalkDirAccum descendHandler transformer startDir ->
        docErr3
          "WalkDirAccum"
          "walk:"
          (showPath startDir)
          "accumulating a result"
      WalkDirAccumRel descendHandler transformer startDir ->
        docErr3
          "WalkDirAccum"
          "walk:"
          (showPath startDir)
          "accumulating a result"
      WithTempFile parentDir fileName action ->
        docErr3
          "withTempFile"
          "create a new temporary file inside:"
          (showPath parentDir)
          "perform an action in the new file and delete after use"
      WithTempDir parentDir dirTemplate action ->
        docErr3
          "withTempDir"
          "create a new temporary directory inside:"
          (showPath parentDir)
          "perform an action in the new directory and delete after use"
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
      _ -> case fs of
        EnsureDir p -> logStep $ dirInfo "ensure directory exists" p
        _ -> uu
   where
    -- CreateDir d -> dirInfo "create directory" d
    -- CreateDirIfMissing _ d -> dirInfo "create directory if missing" d
    -- RemoveDir d -> dirInfo "remove directory" d
    -- RemoveDirRecur d -> dirInfo "remove directory recursively" d
    -- RemovePathForcibly p -> dirInfo "remove path forcibly" p
    -- RenameDir o n -> dirInfo' "rename directory from" o "to" n
    -- ListDir d -> dirInfo "list directory" d
    -- SetCurrentDir d -> dirInfo "set current directory" d
    -- RemoveFile f -> dirInfo "remove file" f
    -- RenameFile o n -> dirInfo' "rename file from:" o "to" n
    -- RenamePath o n -> dirInfo' "rename path from:" o "to" n
    -- CopyFile o n -> dirInfo' "copy file from:" o "to" n
    -- CreateFileLink o n -> dirInfo' "create file link from" o "to" n
    -- CreateDirLink o n -> dirInfo' "create directory link from" o "to" n
    -- RemoveDirLink d -> dirInfo "remove directory link" d
    -- SetPermissions p ps -> info' "set permissions of" p "to" ps
    -- CopyPermissions o n -> info' "copy permissions from" o "to" n
    -- SetAccessTime p t -> info' "set access time of" p "to" t
    -- SetModificationTime p t -> info' "set modification time of" p "to" t
    -- CopyDirRecur o n -> dirInfo' "copy directory recursively from" o "to" n
    -- CopyDirRecur' o n -> dirInfo' "copy directory recursively from" o "to" n
    -- EnsureFileDurable p -> dirInfo "ensure file durable (non-windows only)" p
    -- WriteBinaryFile p bs -> dirInfo "write binary file contents" p
    -- WriteBinaryFileAtomic p bs -> dirInfo "write binary file contents atomically" p
    -- WriteBinaryFileDurable p bs -> dirInfo "write binary file contents" p
    -- WriteBinaryFileDurableAtomic p bs -> dirInfo "write binary file contents atomically" p
    logStep :: Text -> Eff es ()
    logStep = out . Step

    showPath :: Path c d -> Text
    showPath = toS . toFilePath

    showPaths :: [Path c d] -> Text
    showPaths = toS . show . fmap toFilePath

    -- TODO: implement docVal, docHush, docVoid, docVal', or docVoid'
    docErrn :: forall a''. Text -> [Text] -> Eff es a''
    docErrn funcName dscFrags =
      do
        let funcDesc = T.intercalate " " dscFrags
        logStep funcDesc
        -- replace this later when have code to process call
        -- stack right now out of the boc call handling looks better
        -- E.throwError . DocException $
        pure . error $
          "\nException thrown in step documentation."
            <> "\n  Value forced from function: '"
            <> funcName
            <> "' in documentation mode."
            <> "\n  Use  docVal, docHush, docVoid, docVal', or docVoid "
            <> " to replace or silence this value at the call site for: '"
            <> funcName
            <> "'"

    docErr :: forall a''. Text -> Text -> Eff es a''
    docErr funcName funcDesc = docErrn funcName [funcDesc]

    docErr2 :: forall a''. Text -> Text -> Text -> Eff es a''
    docErr2 funcName funcDesc1 funcDesc2 = docErrn funcName [funcDesc1, funcDesc2]

    docErr3 :: forall a''. Text -> Text -> Text -> Text -> Eff es a''
    docErr3 funcName funcDesc1 funcDesc2 funcDesc3 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3]

    docErr4 :: forall a''. Text -> Text -> Text -> Text -> Text -> Eff es a''
    docErr4 funcName funcDesc1 funcDesc2 funcDesc3 funcDesc4 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3, funcDesc4]

    docErr5 :: forall a''. Text -> Text -> Text -> Text -> Text -> Text -> Eff es a''
    docErr5 funcName funcDesc1 funcDesc2 funcDesc3 funcDesc4 funcDesc5 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3, funcDesc4, funcDesc5]

    hoe :: forall b. ((forall r. Eff localEs r -> IO r) -> IO b) -> Eff es b
    hoe h = handle (\(e :: SomeException) -> E.throwError . DocException' "Exception genrated running step documenter" $ e) (localSeqUnliftIO env h)
    --
    info :: (Show o) => Text -> o -> Text
    info prefix o = prefix <> ": " <> txt o

    info' :: (Show o, Show o2) => Text -> o -> Text -> o2 -> Text
    info' prefix o sep o2 = prefix <> ": " <> txt o <> sep <> ": " <> txt o2

    dirInfo :: forall c d. Text -> Path c d -> Text
    dirInfo prefix = info prefix . toFilePath

    dirInfo' :: forall c d e f. Text -> Path c d -> Text -> Path e f -> Text
    dirInfo' prefix p sep p2 = info' prefix (toFilePath p) sep (toFilePath p2)
