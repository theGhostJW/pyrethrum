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

-- TODO: implement docVal, docHush, docVoid, docVal', or docVoid'

logStep :: (Out ApEvent :> es) => Text -> Eff es ()
logStep = out . Step

docErrn :: forall es a. (HasCallStack, IOE :> es, Out ApEvent :> es, E.Error DocException :> es) => Text -> [Text] -> Eff es a
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

docErr :: forall es a. (HasCallStack, IOE :> es, Out ApEvent :> es, E.Error DocException :> es) => Text -> Text -> Eff es a
docErr funcName funcDesc = docErrn funcName [funcDesc]

docErr2 :: forall es a. (HasCallStack, IOE :> es, Out ApEvent :> es, E.Error DocException :> es) => Text -> Text -> Text -> Eff es a
docErr2 funcName funcDesc1 funcDesc2 = docErrn funcName [funcDesc1, funcDesc2]

docErr3 :: forall es a. (HasCallStack, IOE :> es, Out ApEvent :> es, E.Error DocException :> es) => Text -> Text -> Text -> Text -> Eff es a
docErr3 funcName funcDesc1 funcDesc2 funcDesc3 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3]

docErr4 :: forall es a. (HasCallStack, IOE :> es, Out ApEvent :> es, E.Error DocException :> es) => Text -> Text -> Text -> Text -> Text -> Eff es a
docErr4 funcName funcDesc1 funcDesc2 funcDesc3 funcDesc4 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3, funcDesc4]

docErr5 :: forall es a. (HasCallStack, IOE :> es, Out ApEvent :> es, E.Error DocException :> es) => Text -> Text -> Text -> Text -> Text -> Text -> Eff es a
docErr5 funcName funcDesc1 funcDesc2 funcDesc3 funcDesc4 funcDesc5 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3, funcDesc4, funcDesc5]

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
          (showPathComma parentDir)
          "perform an action in the new file, and delete after use"
      WithTempDir parentDir dirTemplate action ->
        docErr3
          "withTempDir"
          "create a new temporary directory inside:"
          (showPathComma parentDir)
          "perform an action in the new directory, and delete after use"
      WithSystemTempFile dirTemplate action ->
        docErr
          "withSystemTempFile"
          "create a new temporary file inside the system temp directory, perform an action in the new file and delete after use"
      WithSystemTempDir dirTemplate action ->
        docErr
          "withSystemTempDir"
          "create a new temporary directory inside the system temp directory, perform an action in the new directory and delete after use"
      ForgivingAbsence action -> docErr "forgivingAbsence" "run an action, returning Nothing if it throws a `Does Not Exist` exception"
      IgnoringAbsence action -> docErr "ignoringAbsence" "run an action ignoring `Does Not Exist` exception"
      WithBinaryFile path ioMode action -> docErr3 "withBinaryFile" "open a binary file:" (showPath path) "perform an action in the file and close it after use"
      WithBinaryFileAtomic path ioMode action -> docErr3 "withBinaryFileAtomic" "open a binary file:" (showPath path) "perform an action in the file and close it after use"
      WithBinaryFileDurable path ioMode action -> docErr3 "withBinaryFileDurable" "open a binary file:" (showPath path) "perform an action in the file and close it after use"
      WithBinaryFileDurableAtomic path ioMode action -> docErr3 "withBinaryFileDurableAtomic" "open a binary file:" (showPath path) "perform an action in the file and close it after use"
      GetCurrentDir -> docErr "getCurrentDir" "get the current working directory"
      GetHomeDir -> docErr "getHomeDir" "get the home directory"
      GetXdgDir directoryType subDir -> docErr3 "getXdgDir" "get the XDG directory:" (txt directoryType) (maybe "" (\d -> "subDir:" <> showPath d) subDir)
      GetXdgDirList xdgList -> docErr2 "getXdgDirList" "get the XDG directories:" (txt xdgList)
      GetAppUserDataDir apName -> docErr2 "getAppUserDataDir" "get the application user data directory:" (txt apName)
      GetUserDocsDir -> docErr "getUserDocsDir" "get the user documents directory"
      GetTempDir -> docErr "getTempDir" "get the system temp directory"
      GetFileSize f -> docErr2 "getFileSize" "get the size of file:" (showPath f)
      DoesPathExist p -> docErr2 "doesPathExist" "check if path exists:" (showPath p)
      DoesFileExist f -> docErr2 "doesFileExist" "check if file exists:" (showPath f)
      DoesDirExist d -> docErr2 "doesDirExist" "check if directory exists:" (showPath d)
      CanonicalizePath p -> docErr2 "canonicalizePath" "canonicalize path:" (txt p)
      MakeAbsolute p -> docErr2 "makeAbsolute" "make path absolute:" (txt p)
      MakeRelativeToCurrentDir p -> docErr2 "makeRelativeToCurrentDir" "make path relative to current directory:" (txt p)
      FindExecutable fileName -> docErr3 "findExecutable" "find executable:" (showPath fileName) "in the directories listed in system PATH."
      FindFile searchDirs fileName -> docErr4 "findFile" "find file:" (showPath fileName) "in the directories:" (showPaths searchDirs)
      FindFiles searchDirs fileName -> docErr4 "findFiles" "find files:" (showPath fileName) "in the directories:" (showPaths searchDirs)
      IsSymlink path -> docErr2 "isSymlink" "check if path is a symlink:" (showPath path)
      GetSymlinkTarget linkPath -> docErr2 "getSymlinkTarget" "get the target of a symlink:" (showPath linkPath)
      GetPermissions filePath -> docErr2 "getPermissions" "get permissions of file:" (showPath filePath)
      GetAccessTime filePath -> docErr2 "getAccessTime" "get access time of file:" (showPath filePath)
      GetModificationTime filePath -> docErr2 "getModificationTime" "get modification time of file:" (showPath filePath)
      ListDirRel dir -> docErr2 "listDirRel" "list directory (relative paths):" (showPath dir)
      ListDirRecur dir -> docErr2 "listDirRecur" "list directory (recursive):" (showPath dir)
      ListDirRecurRel dir -> docErr2 "listDirRecurRel" "list directory (recursive, relative paths):" (showPath dir)
      ResolveFile parentDir fileName -> docErr4 "resolveFile" "resolve file:" fileName "in directory:" (showPath parentDir)
      ResolveFile' fileName -> docErr3 "resolveFile'" "resolve file:" fileName "in working directory"
      ResolveDir parentDir dirName -> docErr4 "resolveDir" "resolve directory:" dirName "in directory:" (showPath parentDir)
      ResolveDir' dirname -> docErr3 "resolveDir'" "resolve directory:" dirname "in working directory"
      OpenBinaryTempFile dir fileTemplate -> docErr4 "openBinaryTempFile" "open binary temp file with base name:" fileTemplate "in directory:" (showPath dir)
      OpenTempFile dir fileTemplate -> docErr4 "openTempFile" "open temp file with base name:" fileTemplate "in directory:" (showPath dir)
      CreateTempDir dir dirTemplate  -> docErr4 "createTempDir" "create temp directory with base name:" dirTemplate "in directory:" (showPath dir)
      IsLocationOccupied path -> docErr2 "isLocationOccupied" "check if location is occupied:" (showPath path)
      EnsureDir p -> docErr2 "ensureDir" "ensure directory exists:" (showPath p)

    -- CreateDir dir -> dirInfo "create directory" d
    -- CreateDirIfMissing _ dir -> dirInfo "create directory if missing" d
    -- RemoveDir dir -> dirInfo "remove directory" d
    -- RemoveDirRecur dir -> dirInfo "remove directory recursively" d
    -- RemovePathForcibly dir -> dirInfo "remove path forcibly" p
    -- RenameDir old new -> dirInfo' "rename directory from" o "to" n
    -- ListDir dir -> dirInfo "list directory" d
    -- SetCurrentDir dir -> dirInfo "set current directory" d
    -- RemoveFile file -> dirInfo "remove file" f
    -- RenameFile old new -> dirInfo' "rename file from:" o "to" n
    -- RenamePath  old new -> dirInfo' "rename path from:" o "to" n
    -- CopyFile src dst -> dirInfo' "copy file from:" o "to" n
    -- CreateFileLink src dst -> dirInfo' "create file link from" o "to" n
    -- CreateDirLink src dst -> dirInfo' "create directory link from" o "to" n
    -- RemoveDirLink dir -> dirInfo "remove directory link" d
    -- SetPermissions p ps -> info' "set permissions of" p "to" ps
    -- CopyPermissions src dst -> info' "copy permissions from" o "to"  old new
    -- SetAccessTime p t -> info' "set access time of" p "to" t
    -- SetModificationTime p t -> info' "set modification time of" p "to" t
    -- CopyDirRecur src dst -> dirInfo' "copy directory recursively from" o "to" n
    -- CopyDirRecur' src dst -> dirInfo' "copy directory recursively from" o "to" n
    -- EnsureFileDurable p -> dirInfo "ensure file durable (non-windows only)" p
   where
    showPath :: Path c d -> Text
    showPath = toS . toFilePath

    showPathComma :: Path c d -> Text
    showPathComma p = showPath p <> ","

    showPaths :: [Path c d] -> Text
    showPaths = toS . show . fmap toFilePath

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
