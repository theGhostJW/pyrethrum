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
import Data.Time (UTCTime)
import Path
import qualified Path.IO as PIO
import Prelude (Bool (..), Either (..), Exception, IO, Integer, Maybe (..), Show, Text, pure, ($), (.), (==), (>>=), (||))
import qualified Prelude as P

import BasePrelude (IOException)
import Chronos (OffsetDatetime)
import Control.Monad.Catch (catch)
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
import Effectful.Error.Static as E
import Effectful.TH (makeEffect)
import PyrethrumExtras (MonadMask, toS, txt, uu)
import qualified System.Directory as SD
import Effectful.Dispatch.Static (unsafeLiftMapIO)

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
  WithCurrentDir :: Path Abs Dir -> FileSystem m a -> FileSystem m a

makeEffect ''FileSystem

type instance DispatchOf FileSystem = Dynamic

newtype FSException = FSException IOException
  deriving (Show)

instance Exception FSException

adaptException :: (HasCallStack, IOE :> es, E.Error FSException :> es) => IO b -> Eff es b
adaptException m = EF.liftIO m `catch` \(e :: IOException) -> throwError . FSException $ e

runFileSystem :: forall es a. (HasCallStack, IOE :> es, E.Error FSException :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystem =
  let 
    er :: IO b -> Eff es b
    er = adaptException
  in
  interpret $ \_ -> \case
    EnsureDir p -> er $ R.ensureDir p
    CreateDir d -> er $ R.createDir d
    CreateDirIfMissing b d -> er $ R.createDirIfMissing b d
    RemoveDir d -> er $ R.removeDir d
    RemoveDirRecur d -> er $ R.removeDirRecur d
    RemovePathForcibly p -> er $ R.removePathForcibly p
    RenameDir o n -> er $ R.renameDir o n
    ListDir d -> er $ R.listDir d
    GetCurrentDir -> er R.getCurrentDir
    SetCurrentDir d -> er $ R.setCurrentDir d
    WithCurrentDir p ef -> er $ unsafeLiftMapIO (R.withCurrentDir p) ef

{-

----------------------------------------
-- Current working directory

-- | Lifted 'R.withCurrentDir'.
withCurrentDir :: (FileSystem :> es) => Path b Dir -> Eff es a -> Eff es a
withCurrentDir path = unsafeLiftMapIO (R.withCurrentDir path)

----------------------------------------
-- Pre-defined directories

-- | Lifted 'R.getHomeDir'.
getHomeDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getHomeDir = unsafeEff_ R.getHomeDir

-- | Lifted 'R.getXdgDir'.
getXdgDir ::
  (FileSystem :> es) =>
  R.XdgDirectory ->
  Maybe (Path Rel Dir) ->
  Eff es (Path Abs Dir)
getXdgDir xdgDir = unsafeEff_ . R.getXdgDir xdgDir

-- | Lifted 'R.getXdgDirList'.
getXdgDirList ::
  (FileSystem :> es) =>
  R.XdgDirectoryList ->
  Eff es [Path Abs Dir]
getXdgDirList = unsafeEff_ . R.getXdgDirList

-- | Lifted 'R.getAppUserDataDir'.
getAppUserDataDir :: (FileSystem :> es) => Text -> Eff es (Path Abs Dir)
getAppUserDataDir = unsafeEff_ . R.getAppUserDataDir . toS

-- | Lifted 'R.getUserDocsDir'.
getUserDocsDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getUserDocsDir = unsafeEff_ R.getUserDocsDir

-- | Lifted 'R.getTempDir'.
getTempDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getTempDir = unsafeEff_ R.getTempDir

----------------------------------------
-- Actions on files

-- | Lifted 'R.removeFile'.
removeFile :: (FileSystem :> es) => Path b File -> Eff es ()
removeFile = unsafeEff_ . R.removeFile

-- | Lifted 'R.renameFile'.
renameFile :: (FileSystem :> es) => Path b File -> Path b File -> Eff es ()
renameFile old = unsafeEff_ . R.renameFile old

-- | Lifted 'R.renamePath'.
renamePath :: (FileSystem :> es) => Path b t -> Path b t -> Eff es ()
renamePath old = unsafeEff_ . R.renamePath old

-- | Lifted 'R.copyFile'.
copyFile :: (FileSystem :> es) => Path b File -> Path b File -> Eff es ()
copyFile src = unsafeEff_ . R.copyFile src

-- | Lifted 'R.copyFileWithMetadata'.
copyFileWithMetadata :: (FileSystem :> es) => Path b File -> Path b File -> Eff es ()
copyFileWithMetadata src dst = unsafeEff_ $ SD.copyFileWithMetadata (toFilePath src) (toFilePath dst)

-- | Lifted 'R.getFileSize'.
getFileSize :: (FileSystem :> es) => Path b File -> Eff es Integer
getFileSize = unsafeEff_ . R.getFileSize

-- | Lifted 'R.canonicalizePath'.
canonicalizePath :: (PIO.AnyPath path, FileSystem :> es) => path -> Eff es (PIO.AbsPath path)
canonicalizePath = unsafeEff_ . R.canonicalizePath

-- | Lifted 'R.makeAbsolute'.
makeAbsolute :: (PIO.AnyPath path) => (FileSystem :> es) => path -> Eff es (PIO.AbsPath path)
makeAbsolute = unsafeEff_ . R.makeAbsolute

-- | Lifted 'R.makeRelativeToCurrentDir'.
makeRelativeToCurrentDir ::
  (PIO.AnyPath path, FileSystem :> es) =>
  path ->
  Eff es (PIO.RelPath path)
makeRelativeToCurrentDir = unsafeEff_ . R.makeRelativeToCurrentDir

----------------------------------------
-- Existence tests

-- | Lifted 'R.doesPathExist'.
doesPathExist :: (FileSystem :> es) => Path b t -> Eff es Bool
doesPathExist = unsafeEff_ . R.doesPathExist

-- | Lifted 'R.doesFileExist'.
doesFileExist :: (FileSystem :> es) => Path b File -> Eff es Bool
doesFileExist = unsafeEff_ . R.doesFileExist

-- | Lifted 'R.doesDirExist'.
doesDirExist :: (FileSystem :> es) => Path b Dir -> Eff es Bool
doesDirExist = unsafeEff_ . R.doesDirExist

-- | Lifted 'R.findExecutable'.
findExecutable :: (FileSystem :> es) => Path Rel File -> Eff es (Maybe (Path Abs File))
findExecutable = unsafeEff_ . R.findExecutable

-- | Lifted 'R.findFile'.
findFile :: (FileSystem :> es) => [Path b Dir] -> Path Rel File -> Eff es (Maybe (Path Abs File))
findFile dirs = unsafeEff_ . R.findFile dirs

-- | Lifted 'R.findFiles'.
findFiles :: (FileSystem :> es) => [Path b Dir] -> Path Rel File -> Eff es [Path Abs File]
findFiles dirs = unsafeEff_ . R.findFiles dirs

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

----------------------------------------
-- Symbolic links

-- | Lifted 'R.createFileLink'.
createFileLink :: (FileSystem :> es) => Path b File -> Path b File -> Eff es ()
createFileLink target = unsafeEff_ . R.createFileLink target

-- | Lifted 'R.createDirLink'.
createDirLink :: (FileSystem :> es) => Path b Dir -> Path b Dir -> Eff es ()
createDirLink target = unsafeEff_ . R.createDirLink target

-- | Lifted 'R.removeDirLink'.
removeDirLink :: (FileSystem :> es) => Path b Dir -> Eff es ()
removeDirLink = unsafeEff_ . R.removeDirLink

-- | Lifted 'R.isSymlink'.
isSymlink :: (FileSystem :> es) => Path b t -> Eff es Bool
isSymlink = unsafeEff_ . R.isSymlink

-- | Lifted 'R.getSymlinkTarget'.
getSymlinkTarget :: (FileSystem :> es) => Path b t -> Eff es Text
getSymlinkTarget = unsafeEff_ . P.fmap toS . R.getSymlinkTarget

----------------------------------------
-- Permissions

-- | Lifted 'R.getPermissions'.
getPermissions :: (FileSystem :> es) => Path b t -> Eff es R.Permissions
getPermissions = unsafeEff_ . R.getPermissions

-- | Lifted 'R.setPermissions'.
setPermissions :: (FileSystem :> es) => Path b t -> R.Permissions -> Eff es ()
setPermissions path = unsafeEff_ . R.setPermissions path

-- | Lifted 'R.copyPermissions'.
copyPermissions :: (FileSystem :> es) => Path b t -> Path b t -> Eff es ()
copyPermissions src = unsafeEff_ . R.copyPermissions src

----------------------------------------
-- Timestamps
-- TODO:: change to chronos OffsetTime - note chronos uses minutes for offset

-- | Lifted 'R.getAccessTime'.
getAccessTime :: (FileSystem :> es) => Path b t -> Eff es OffsetDatetime
getAccessTime = unsafeEff_ . R.getAccessTime

-- | Lifted 'R.getModificationTime'.
getModificationTime :: (FileSystem :> es) => Path b t -> Eff es OffsetDatetime
getModificationTime = unsafeEff_ . R.getModificationTime

-- | Lifted 'R.setAccessTime'.
setAccessTime :: (FileSystem :> es) => Path b t -> OffsetDatetime -> Eff es ()
setAccessTime path = unsafeEff_ . R.setAccessTime path

-- | Lifted 'R.setModificationTime'.
setModificationTime :: (FileSystem :> es) => Path b t -> OffsetDatetime -> Eff es ()
setModificationTime path = unsafeEff_ . R.setModificationTime path

----------------------------------------
-- path-io only

-- | The same as 'listDir' but returns relative paths.
listDirRel ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRel = unsafeEff_ . R.listDirRel

{- | Similar to 'listDir', but recursively traverses every sub-directory
/excluding symbolic links/, and returns all files and directories found.
This can fail with the same exceptions as 'listDir'.

__Note__: before version /1.3.0/, this function followed symlinks.
-}
listDirRecur ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Abs Dir], [Path Abs File])
listDirRecur = unsafeEff_ . R.listDirRecur

{- | The same as 'listDirRecur' but returns paths that are relative to the
given directory.
-}
listDirRecurRel ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRecurRel = unsafeEff_ . R.listDirRecurRel

{- | Copies a directory recursively. It /does not/ follow symbolic links and
preserves permissions when possible. If the destination directory already
exists, new files and sub-directories complement its structure, possibly
overwriting old files if they happen to have the same name as the new
ones.

__Note__: before version /1.3.0/, this function followed symlinks.

This function now behaves much like the @cp@ utility, not
traversing symlinked directories, but recreating symlinks in the target
directory according to their targets in the source directory.
TODO :: test including errors - static vs dynamic - 2 * 2 implementations required?
-}
copyDirRecur ::
  (FileSystem :> es, Error e :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur s = unsafeEff_ . R.copyDirRecur s

{- | The same as 'copyDirRecur', but it /does not/ preserve directory
permissions. This may be useful, for example, if the directory you want
to copy is “read-only”, but you want your copy to be editable.

@since 1.1.0

__Note__: before version /1.3.0/, this function followed symlinks.

__Note__: before version /1.6.0/, the function created empty directories
in the destination directory when the source directory contained
directory symlinks. The symlinked directories were not recursively
traversed. It also copied symlinked files creating normal regular files
in the target directory as the result. This was fixed in the version
/1.6.0/ so that the function now behaves much like the @cp@ utility, not
traversing symlinked directories, but recreating symlinks in the target
directory according to their targets in the source directory.
TODO :: test including errors - static vs dynamic - 2 * 2 implementations required?
-}
copyDirRecur' ::
  (FileSystem :> es, Error e :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur' s = unsafeEff_ . R.copyDirRecur' s

-- Walking directory trees

{- | Traverse a directory tree using depth first pre-order traversal,
calling a handler function at each directory node traversed. The absolute
paths of the parent directory, sub-directories and the files in the
directory are provided as arguments to the handler.

The function is capable of detecting and avoiding traversal loops in the
directory tree. Note that the traversal follows symlinks by default, an
appropriate traversal handler can be used to avoid that when necessary.
TODO :: higher order effect ?? - same issue applies to all effects that use IO or
return a handle
-}
walkDir ::
  (IOE :> es, FileSystem :> es) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (R.WalkAction Abs)) ->
  -- | Directory where traversal begins
  Path b Dir ->
  Eff es ()
walkDir h = unsafeEff_ . R.walkDir h

{- | The same as 'walkDir' but uses relative paths. The handler is given
@dir@, directory relative to the directory where traversal begins.
Sub-directories and files are relative to @dir@.

@since 1.4.2
-}
walkDirRel ::
  (FileSystem :> es) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  ( Path Rel Dir ->
    [Path Rel Dir] ->
    [Path Rel File] ->
    IO (R.WalkAction Rel)
  ) ->
  -- | Directory where traversal begins
  Path b Dir ->
  Eff es ()
walkDirRel h = unsafeEff_ . R.walkDirRel h

{- | Similar to 'walkDir' but accepts a 'Monoid'-returning output writer as
well. Values returned by the output writer invocations are accumulated
and returned.

Both, the descend handler as well as the output writer can be used for
side effects but keep in mind that the output writer runs before the
descend handler.
-}
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

{- | The same as 'walkDirAccum' but uses relative paths. The handler and
writer are given @dir@, directory relative to the directory where
traversal begins. Sub-directories and files are relative to @dir@.
-}
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

{- | Append Textly-typed path to an absolute path and then canonicalize
it.
-}
resolveFile ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  Text ->
  Eff es (Path Abs File)
resolveFile b = unsafeEff_ . R.resolveFile b . toS

-- | The same as 'resolveFile', but uses current working directory.
resolveFile' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  Text ->
  Eff es (Path Abs File)
resolveFile' = unsafeEff_ . R.resolveFile' . toS

-- | The same as 'resolveFile', but for directories.
resolveDir ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  Text ->
  Eff es (Path Abs Dir)
resolveDir b = unsafeEff_ . R.resolveDir b . toS

-- | The same as 'resolveDir', but uses current working directory.
resolveDir' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  Text ->
  Eff es (Path Abs Dir)
resolveDir' = unsafeEff_ . R.resolveDir' . toS

----------------------------------------------------------------------------
-- Temporary files and directories

{- | Use a temporary file that doesn't already exist.

Creates a new temporary file inside the given directory, making use of
the template. The temporary file is deleted after use.

@since 0.2.0
-}
withTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> IO a) ->
  Eff es a
withTempFile path t action = unsafeEff_ $ R.withTempFile path (toS t) action

{- | Create and use a temporary directory.

Creates a new temporary directory inside the given directory, making use
of the template. The temporary directory is deleted after use.

@since 0.2.0
-}
withTempDir ::
  (FileSystem :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> IO a) ->
  Eff es a
withTempDir path t action = unsafeEff_ $ R.withTempDir path (toS t) action

{- | Create and use a temporary file in the system standard temporary
directory.

Behaves exactly the same as 'withTempFile', except that the parent
temporary directory will be that returned by 'getTempDir'.

@since 0.2.0
-}
withSystemTempFile ::
  (FileSystem :> es) =>
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> IO a) ->
  Eff es a
withSystemTempFile t action =
  unsafeEff_ $ R.withSystemTempFile (toS t) action

{- | Create and use a temporary directory in the system standard temporary
directory.

Behaves exactly the same as 'withTempDir', except that the parent
temporary directory will be that returned by 'getTempDir'.

@since 0.2.0
-}
withSystemTempDir ::
  (FileSystem :> es) =>
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> IO a) ->
  Eff es a
withSystemTempDir t = unsafeEff_ . R.withSystemTempDir (toS t)

{- | The function creates a temporary file in @rw@ mode. The created file
isn't deleted automatically, so you need to delete it manually.

The file is created with permissions such that only the current user can
read\/write it.

With some exceptions (see below), the file will be created securely in
the sense that an attacker should not be able to cause openTempFile to
overwrite another file on the filesystem using your credentials, by
putting symbolic links (on Unix) in the place where the temporary file is
to be created. On Unix the @O_CREAT@ and @O_EXCL@ flags are used to
prevent this attack, but note that @O_EXCL@ is sometimes not supported on
NFS filesystems, so if you rely on this behaviour it is best to use local
filesystems only.

@since 0.2.0
-}
openTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template; if the template is "foo.ext" then the created
  -- file will be @\"fooXXX.ext\"@ where @XXX@ is some random number
  Text ->
  -- | Name of created file and its 'Handle'
  Eff es (Path Abs File, P.Handle)
openTempFile p = unsafeEff_ . R.openTempFile p . toS

{- | Like 'openTempFile', but opens the file in binary mode. On Windows,
reading a file in text mode (which is the default) will translate @CRLF@
to @LF@, and writing will translate @LF@ to @CRLF@. This is usually what
you want with text files. With binary files this is undesirable; also, as
usual under Microsoft operating systems, text mode treats control-Z as
EOF. Binary mode turns off all special treatment of end-of-line and
end-of-file characters.

@since 0.2.0
-}
openBinaryTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Name of created file and its 'Handle'
  Eff es (Path Abs File, P.Handle)
openBinaryTempFile p = unsafeEff_ . R.openBinaryTempFile p . toS

{- | Create a temporary directory. The created directory isn't deleted
automatically, so you need to delete it manually.

The directory is created with permissions such that only the current user
can read\/write it.

@since 0.2.0
-}
createTempDir ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Name of created temporary directory
  Eff es (Path Abs Dir)
createTempDir p = unsafeEff_ . R.createTempDir p . toS

--  * Existence tests

-- | Check if there is a file or directory on specified path.
isLocationOccupied :: (FileSystem :> es) => Path b t -> Eff es Bool
isLocationOccupied = unsafeEff_ . R.isLocationOccupied

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

-}
