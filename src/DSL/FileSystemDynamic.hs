{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- TODO :: Add missing functions added in - https://github.com/haskell-effectful/effectful/pull/151
{- this is a copy of FileSystem from the Effectful package 
adapted to use path and path-io instead of Text and directory
- functions renamed to be consistent with path-io
- types changed to path-io types
- removed 
  - getDirContents not in path-io 
  - findExecutablesInDirectories not in path-io

- added  
    ensureDir,
    listDirRel,
    listDirRecur,
    listDirRecurRel,

    copyDirRecur,
    copyDirRecur',

    -- ** Walking directory trees
    WalkAction (..),
    walkDir,
    walkDirRel,
    walkDirAccum,
    walkDirAccumRel,

    -- * Path b t transformation
    resolveFile,
    resolveFile',
    resolveDir,
    resolveDir',

    -- * Temporary files and directories
    withTempFile,
    withTempDir,
    withSystemTempFile,
    withSystemTempDir,
    openTempFile,
    openBinaryTempFile,
    createTempDir,

    -- * Existence tests
    isLocationOccupied,
    forgivingAbsence,
    ignoringAbsence,
-}
module DSL.FileSystemDynamic
  ( -- * Effect
  --   FileSystem

  --   -- ** Handlers
  -- , runFileSystem

  --   -- * Actions on directories
  -- , createDir 
  -- , createDirIfMissing
  -- , removeDir
  -- , removeDirRecur
  -- , removePathForcibly
  -- , renameDir
  -- , listDir

  --   -- ** Current working directory
  -- , getCurrentDir
  -- , setCurrentDir
  -- , withCurrentDir

  --   -- * Pre-defined directories
  -- , getHomeDir
  -- , getXdgDir
  -- , getXdgDirList
  -- , getAppUserDataDir
  -- , getUserDocsDir
  -- , getTempDir

  --   -- * Actions on files
  -- , removeFile
  -- , renameFile
  -- , renamePath
  -- , copyFile
  -- , copyFileWithMetadata
  -- , getFileSize
  -- , canonicalizePath
  -- , makeAbsolute
  -- , makeRelativeToCurrentDir

  --   -- * Existence tests
  -- , doesPathExist
  -- , doesFileExist
  -- , doesDirExist
  -- , findExecutable
  -- , findFile
  -- , findFiles
  -- -- , findFileWith
  -- , findFilesWith

  --   -- * Symbolic links
  -- , createFileLink
  -- , createDirLink
  -- , removeDirLink
  -- , isSymlink
  -- , getSymlinkTarget

  --   -- * Permissions
  -- , getPermissions
  -- , setPermissions
  -- , copyPermissions

  --   -- * Timestamps
  -- , getAccessTime
  -- , getModificationTime
  -- , setAccessTime
  -- , setModificationTime

  --   -- * Re-exports

  --   -- ** Pre-defined directories
  -- , D.XdgDirectory(..)
  -- , D.XdgDirectoryList(..)

  --   -- ** Existence tests
  -- , exeExtension

  --   -- ** Permissions
  -- , D.Permissions
  -- , D.emptyPermissions
  -- , D.readable
  -- , D.writable
  -- , D.executable
  -- , D.searchable
  -- , D.setOwnerReadable
  -- , D.setOwnerWritable
  -- , D.setOwnerExecutable
  -- , D.setOwnerSearchable

  -- -- from pathIO  
  --   , ensureDir
  --   , listDirRel
  --   , listDirRecur
  --   , listDirRecurRel

  --   , copyDirRecur
  --   , copyDirRecur'

  --   -- ** Walking directory trees
  --   , D.WalkAction (..)
  --   , walkDir
  --   , walkDirRel
  --   , walkDirAccum
  --   , walkDirAccumRel

  --   -- * Path b t transformation
  --   , resolveFile
  --   , resolveFile'

  --   , resolveDir
  --   , resolveDir'

    -- * Temporary files and directories
  --   , withTempFile
  --   , withTempDir
  --   , withSystemTempFile
  --   , withSystemTempDir
  --   , openTempFile
  --   , openBinaryTempFile
  --   , createTempDir

  --   -- * Existence tests
  --   , isLocationOccupied
  --   , forgivingAbsence
  --   , ignoringAbsence
   ) where

import Data.Time (UTCTime)
import Path 
import qualified Path.IO as D
import qualified Prelude as P
import Prelude (Bool(..), IO, Maybe(..), Text, (.), ($), (==), (||), Integer)

import Effectful
import Effectful.Dispatch.Dynamic
-- import DSL.FileSystem.EffectStatic
import qualified System.Directory as SD
import PyrethrumExtras (toS, MonadMask)
import Effectful.Error.Dynamic

exeExtension :: Text
exeExtension = toS SD.exeExtension

{-
-- data FileSystem f :: Effect where
--   CreateDir :: Path b Dir -> FileSystem f ()

  
-- | Ensure that a directory exists creating it and its parent directories
-- if necessary. This is just a handy shortcut:
--
-- > ensureDir = createDirIfMissing True
ensureDir  :: FileSystem :> es => Path b Dir -> Eff es ()
ensureDir = unsafeEff_ . D.ensureDir

-- | Lifted 'D.createDirIfMissing'.
createDirIfMissing :: FileSystem :> es => Bool -> Path b Dir -> Eff es ()
createDirIfMissing doCreateParents =
  unsafeEff_ . D.createDirIfMissing doCreateParents

-- | Lifted 'D.removeDir'.
removeDir :: FileSystem :> es => Path b Dir -> Eff es ()
removeDir = unsafeEff_ . D.removeDir

-- | Lifted 'D.removeDirRecur'.
removeDirRecur :: FileSystem :> es => Path b Dir -> Eff es ()
removeDirRecur = unsafeEff_ . D.removeDirRecur

-- | Lifted 'D.removePathForcibly'.
removePathForcibly :: FileSystem :> es => Path b t -> Eff es ()
removePathForcibly = unsafeEff_ . D.removePathForcibly

-- | Lifted 'D.renameDir'.
renameDir :: FileSystem :> es => Path b Dir -> Path b Dir -> Eff es ()
renameDir old = unsafeEff_ . D.renameDir old

-- | Lifted 'D.listDir'.
listDir :: FileSystem :> es => Path b Dir -> Eff es ([Path Abs Dir], [Path Abs File])
listDir = unsafeEff_ . D.listDir

----------------------------------------
-- Current working directory

-- | Lifted 'D.getCurrentDir'.
getCurrentDir :: FileSystem :> es => Eff es (Path Abs Dir)
getCurrentDir = unsafeEff_ D.getCurrentDir

-- | Lifted 'D.setCurrentDir'.
setCurrentDir :: FileSystem :> es => Path b Dir -> Eff es ()
setCurrentDir = unsafeEff_ . D.setCurrentDir

-- | Lifted 'D.withCurrentDir'.
withCurrentDir :: FileSystem :> es => Path b Dir -> Eff es a -> Eff es a
withCurrentDir path = unsafeLiftMapIO (D.withCurrentDir path)

----------------------------------------
-- Pre-defined directories

-- | Lifted 'D.getHomeDir'.
getHomeDir :: FileSystem :> es => Eff es (Path Abs Dir)
getHomeDir = unsafeEff_ D.getHomeDir

-- | Lifted 'D.getXdgDir'.
getXdgDir
  :: FileSystem :> es
  => D.XdgDirectory
  -> Maybe (Path Rel Dir)
  -> Eff es (Path Abs Dir)
getXdgDir xdgDir = unsafeEff_ . D.getXdgDir xdgDir

-- | Lifted 'D.getXdgDirList'.
getXdgDirList
  :: FileSystem :> es
  => D.XdgDirectoryList
  -> Eff es [Path Abs Dir]
getXdgDirList = unsafeEff_ . D.getXdgDirList

-- | Lifted 'D.getAppUserDataDir'.
getAppUserDataDir :: FileSystem :> es => Text -> Eff es (Path Abs Dir)
getAppUserDataDir = unsafeEff_ . D.getAppUserDataDir . toS

-- | Lifted 'D.getUserDocsDir'.
getUserDocsDir :: FileSystem :> es => Eff es (Path Abs Dir)
getUserDocsDir = unsafeEff_ D.getUserDocsDir

-- | Lifted 'D.getTempDir'.
getTempDir :: FileSystem :> es => Eff es (Path Abs Dir)
getTempDir = unsafeEff_ D.getTempDir

----------------------------------------
-- Actions on files

-- | Lifted 'D.removeFile'.
removeFile :: FileSystem :> es => Path b File  -> Eff es ()
removeFile = unsafeEff_ . D.removeFile

-- | Lifted 'D.renameFile'.
renameFile :: FileSystem :> es => Path b File -> Path b File -> Eff es ()
renameFile old = unsafeEff_ . D.renameFile old

-- | Lifted 'D.renamePath'.
renamePath :: FileSystem :> es => Path b t -> Path b t -> Eff es ()
renamePath old = unsafeEff_ . D.renamePath old

-- | Lifted 'D.copyFile'.
copyFile :: FileSystem :> es => Path b File -> Path b File -> Eff es ()
copyFile src = unsafeEff_ . D.copyFile src

-- | Lifted 'D.copyFileWithMetadata'.
copyFileWithMetadata :: FileSystem :> es => Path b File -> Path b File -> Eff es ()
copyFileWithMetadata src dst = unsafeEff_ $ SD.copyFileWithMetadata (toFilePath src) (toFilePath dst)

-- | Lifted 'D.getFileSize'.
getFileSize :: FileSystem :> es => Path b File -> Eff es Integer
getFileSize = unsafeEff_ . D.getFileSize

-- | Lifted 'D.canonicalizePath'.
canonicalizePath :: (D.AnyPath path, FileSystem :> es) => path -> Eff es (D.AbsPath path)
canonicalizePath = unsafeEff_ . D.canonicalizePath

-- | Lifted 'D.makeAbsolute'.
makeAbsolute  :: D.AnyPath path => FileSystem :> es => path -> Eff es (D.AbsPath path)
makeAbsolute = unsafeEff_ . D.makeAbsolute

-- | Lifted 'D.makeRelativeToCurrentDir'.
makeRelativeToCurrentDir ::
 (D.AnyPath path, FileSystem :> es) 
  => path
  -> Eff es (D.RelPath path)
makeRelativeToCurrentDir = unsafeEff_ . D.makeRelativeToCurrentDir

----------------------------------------
-- Existence tests

-- | Lifted 'D.doesPathExist'.
doesPathExist :: FileSystem :> es => Path b t -> Eff es Bool
doesPathExist = unsafeEff_ . D.doesPathExist

-- | Lifted 'D.doesFileExist'.
doesFileExist :: FileSystem :> es => Path b File -> Eff es Bool
doesFileExist = unsafeEff_ . D.doesFileExist

-- | Lifted 'D.doesDirExist'.
doesDirExist :: FileSystem :> es => Path b Dir -> Eff es Bool
doesDirExist = unsafeEff_ . D.doesDirExist

-- | Lifted 'D.findExecutable'.
findExecutable :: FileSystem :> es => Path Rel File -> Eff es (Maybe (Path Abs File))
findExecutable = unsafeEff_ . D.findExecutable

-- | Lifted 'D.findFile'.
findFile :: FileSystem :> es => [Path b Dir] -> Path Rel File -> Eff es (Maybe (Path Abs File))
findFile dirs = unsafeEff_ . D.findFile dirs

-- | Lifted 'D.findFiles'.
findFiles :: FileSystem :> es => [Path b Dir] -> Path Rel File -> Eff es [Path Abs File]
findFiles dirs = unsafeEff_ . D.findFiles dirs

-- | Lifted 'D.findFileWith'.
-- findFileWith
--   :: FileSystem :> es
--   => (Path b t -> Eff es Bool)
--   -> [Path b t]
--   -> Text
--   -> Eff es (Maybe (Path b t))
-- findFileWith p dirs n = unsafeSeqUnliftIO $ \unlift -> do
--   D.findFileWith (unlift . p) dirs n

-- | Lifted 'D.findFilesWith'.
findFilesWith
  :: FileSystem :> es
  => (Path Abs File -> Eff es Bool)
  -> [Path b Dir]
  -> Path Rel File
  -> Eff es [Path Abs File]
findFilesWith p dirs ns = unsafeSeqUnliftIO $ \unlift -> do
  D.findFilesWith (unlift . p) dirs ns

----------------------------------------
-- Symbolic links

-- | Lifted 'D.createFileLink'.
createFileLink :: FileSystem :> es => Path b File -> Path b File -> Eff es ()
createFileLink target = unsafeEff_ . D.createFileLink target

-- | Lifted 'D.createDirLink'.
createDirLink :: FileSystem :> es => Path b Dir -> Path b Dir -> Eff es ()
createDirLink target = unsafeEff_ . D.createDirLink target

-- | Lifted 'D.removeDirLink'.
removeDirLink :: FileSystem :> es => Path b Dir -> Eff es ()
removeDirLink = unsafeEff_ . D.removeDirLink

-- | Lifted 'D.isSymlink'.
isSymlink :: FileSystem :> es => Path b t -> Eff es Bool
isSymlink = unsafeEff_ . D.isSymlink

-- | Lifted 'D.getSymlinkTarget'.
getSymlinkTarget :: FileSystem :> es => Path b t -> Eff es Text
getSymlinkTarget = unsafeEff_ . P.fmap toS . D.getSymlinkTarget 

----------------------------------------
-- Permissions

-- | Lifted 'D.getPermissions'.
getPermissions :: FileSystem :> es => Path b t -> Eff es D.Permissions
getPermissions = unsafeEff_ . D.getPermissions

-- | Lifted 'D.setPermissions'.
setPermissions :: FileSystem :> es => Path b t -> D.Permissions -> Eff es ()
setPermissions path = unsafeEff_ . D.setPermissions path

-- | Lifted 'D.copyPermissions'.
copyPermissions :: FileSystem :> es => Path b t -> Path b t -> Eff es ()
copyPermissions src = unsafeEff_ . D.copyPermissions src

----------------------------------------
-- Timestamps
-- TODO:: change to chronos OffsetTime - note chronos uses minutes for offset

-- | Lifted 'D.getAccessTime'.
getAccessTime :: FileSystem :> es => Path b t -> Eff es UTCTime
getAccessTime = unsafeEff_ . D.getAccessTime

-- | Lifted 'D.getModificationTime'.
getModificationTime :: FileSystem :> es => Path b t -> Eff es UTCTime
getModificationTime = unsafeEff_ . D.getModificationTime

-- | Lifted 'D.setAccessTime'.
setAccessTime :: FileSystem :> es => Path b t -> UTCTime -> Eff es ()
setAccessTime path = unsafeEff_ . D.setAccessTime path

-- | Lifted 'D.setModificationTime'.
setModificationTime :: FileSystem :> es => Path b t -> UTCTime -> Eff es ()
setModificationTime path = unsafeEff_ . D.setModificationTime path


----------------------------------------
-- path-io only

-- | The same as 'listDir' but returns relative paths.
listDirRel ::
  FileSystem :> es =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRel = unsafeEff_ . D.listDirRel

-- | Similar to 'listDir', but recursively traverses every sub-directory
-- /excluding symbolic links/, and returns all files and directories found.
-- This can fail with the same exceptions as 'listDir'.
--
-- __Note__: before version /1.3.0/, this function followed symlinks.
listDirRecur ::
  FileSystem :> es =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Abs Dir], [Path Abs File])
listDirRecur = unsafeEff_ . D.listDirRecur

-- | The same as 'listDirRecur' but returns paths that are relative to the
-- given directory.
listDirRecurRel ::
  FileSystem :> es =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRecurRel = unsafeEff_ . D.listDirRecurRel


-- | Copies a directory recursively. It /does not/ follow symbolic links and
-- preserves permissions when possible. If the destination directory already
-- exists, new files and sub-directories complement its structure, possibly
-- overwriting old files if they happen to have the same name as the new
-- ones.
--
-- __Note__: before version /1.3.0/, this function followed symlinks.
--
-- This function now behaves much like the @cp@ utility, not
-- traversing symlinked directories, but recreating symlinks in the target
-- directory according to their targets in the source directory.
-- TODO :: test including errors - static vs dynamic - 2 * 2 implementations required?
copyDirRecur ::
  (FileSystem :> es, Error e :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur s = unsafeEff_ . D.copyDirRecur s


-- | The same as 'copyDirRecur', but it /does not/ preserve directory
-- permissions. This may be useful, for example, if the directory you want
-- to copy is “read-only”, but you want your copy to be editable.
--
-- @since 1.1.0
--
-- __Note__: before version /1.3.0/, this function followed symlinks.
--
-- __Note__: before version /1.6.0/, the function created empty directories
-- in the destination directory when the source directory contained
-- directory symlinks. The symlinked directories were not recursively
-- traversed. It also copied symlinked files creating normal regular files
-- in the target directory as the result. This was fixed in the version
-- /1.6.0/ so that the function now behaves much like the @cp@ utility, not
-- traversing symlinked directories, but recreating symlinks in the target
-- directory according to their targets in the source directory.
-- TODO :: test including errors - static vs dynamic - 2 * 2 implementations required?
copyDirRecur' ::
  (FileSystem :> es, Error e :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur' s = unsafeEff_ . D.copyDirRecur' s

-- Walking directory trees
-- | Traverse a directory tree using depth first pre-order traversal,
-- calling a handler function at each directory node traversed. The absolute
-- paths of the parent directory, sub-directories and the files in the
-- directory are provided as arguments to the handler.
--
-- The function is capable of detecting and avoiding traversal loops in the
-- directory tree. Note that the traversal follows symlinks by default, an
-- appropriate traversal handler can be used to avoid that when necessary.
-- TODO :: higher order effect ?? - same issue applies to all effects that use IO or 
-- return a handle
walkDir ::
   (IOE :> es, FileSystem :> es) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (D.WalkAction Abs)) ->
  -- | Directory where traversal begins
  Path b Dir ->
  Eff es ()
walkDir h  = unsafeEff_ . D.walkDir h 



-- | The same as 'walkDir' but uses relative paths. The handler is given
-- @dir@, directory relative to the directory where traversal begins.
-- Sub-directories and files are relative to @dir@.
--
-- @since 1.4.2
walkDirRel ::
  ( FileSystem :> es) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  ( Path Rel Dir ->
    [Path Rel Dir] ->
    [Path Rel File] ->
    IO (D.WalkAction Rel)
  ) ->
  -- | Directory where traversal begins
  Path b Dir ->
  Eff es ()
walkDirRel h = unsafeEff_ . D.walkDirRel h


-- | Similar to 'walkDir' but accepts a 'Monoid'-returning output writer as
-- well. Values returned by the output writer invocations are accumulated
-- and returned.
--
-- Both, the descend handler as well as the output writer can be used for
-- side effects but keep in mind that the output writer runs before the
-- descend handler.
walkDirAccum ::
  (FileSystem :> es, P.Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (D.WalkAction Abs)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  Eff es o
walkDirAccum h w = unsafeEff_ . D.walkDirAccum h w

-- | The same as 'walkDirAccum' but uses relative paths. The handler and
-- writer are given @dir@, directory relative to the directory where
-- traversal begins. Sub-directories and files are relative to @dir@.
walkDirAccumRel ::
  (FileSystem :> es, P.Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> IO (D.WalkAction Rel)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> IO o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  Eff es o
walkDirAccumRel h w = unsafeEff_ . D.walkDirAccumRel h w 


-- | Append Textly-typed path to an absolute path and then canonicalize
-- it.
resolveFile ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  Text ->
  Eff es (Path Abs File)
resolveFile b = unsafeEff_ . D.resolveFile b . toS

-- | The same as 'resolveFile', but uses current working directory.
resolveFile' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  Text ->
  Eff es (Path Abs File)
resolveFile' = unsafeEff_ . D.resolveFile' . toS

-- | The same as 'resolveFile', but for directories.
resolveDir ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  Text ->
  Eff es (Path Abs Dir)
resolveDir b =  unsafeEff_ . D.resolveDir b . toS

-- | The same as 'resolveDir', but uses current working directory.
resolveDir' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  Text ->
  Eff es (Path Abs Dir)
resolveDir' =  unsafeEff_ . D.resolveDir' . toS



----------------------------------------------------------------------------
-- Temporary files and directories

-- | Use a temporary file that doesn't already exist.
--
-- Creates a new temporary file inside the given directory, making use of
-- the template. The temporary file is deleted after use.
--
-- @since 0.2.0
withTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> IO a) ->
  Eff es a
withTempFile path t action = unsafeEff_ $ D.withTempFile  path (toS t) action

-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temporary directory is deleted after use.
--
-- @since 0.2.0
withTempDir ::
  (FileSystem :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> IO a) ->
  Eff es a
withTempDir path t action =  unsafeEff_ $ D.withTempDir  path (toS t) action

-- | Create and use a temporary file in the system standard temporary
-- directory.
--
-- Behaves exactly the same as 'withTempFile', except that the parent
-- temporary directory will be that returned by 'getTempDir'.
--
-- @since 0.2.0
withSystemTempFile ::
  (FileSystem :> es) =>
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> IO a) ->
  Eff es a
withSystemTempFile t action =
  unsafeEff_ $ D.withSystemTempFile (toS t) action

-- | Create and use a temporary directory in the system standard temporary
-- directory.
--
-- Behaves exactly the same as 'withTempDir', except that the parent
-- temporary directory will be that returned by 'getTempDir'.
--
-- @since 0.2.0
withSystemTempDir ::
  (FileSystem :> es) =>
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> IO a) ->
  Eff es a
withSystemTempDir t = unsafeEff_ . D.withSystemTempDir (toS t)

-- | The function creates a temporary file in @rw@ mode. The created file
-- isn't deleted automatically, so you need to delete it manually.
--
-- The file is created with permissions such that only the current user can
-- read\/write it.
--
-- With some exceptions (see below), the file will be created securely in
-- the sense that an attacker should not be able to cause openTempFile to
-- overwrite another file on the filesystem using your credentials, by
-- putting symbolic links (on Unix) in the place where the temporary file is
-- to be created. On Unix the @O_CREAT@ and @O_EXCL@ flags are used to
-- prevent this attack, but note that @O_EXCL@ is sometimes not supported on
-- NFS filesystems, so if you rely on this behaviour it is best to use local
-- filesystems only.
--
-- @since 0.2.0
openTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template; if the template is "foo.ext" then the created
  -- file will be @\"fooXXX.ext\"@ where @XXX@ is some random number
  Text ->
  -- | Name of created file and its 'Handle'
  Eff es  (Path Abs File, P.Handle)
openTempFile p = unsafeEff_ . D.openTempFile p . toS

-- | Like 'openTempFile', but opens the file in binary mode. On Windows,
-- reading a file in text mode (which is the default) will translate @CRLF@
-- to @LF@, and writing will translate @LF@ to @CRLF@. This is usually what
-- you want with text files. With binary files this is undesirable; also, as
-- usual under Microsoft operating systems, text mode treats control-Z as
-- EOF. Binary mode turns off all special treatment of end-of-line and
-- end-of-file characters.
--
-- @since 0.2.0
openBinaryTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Name of created file and its 'Handle'
  Eff es (Path Abs File, P.Handle)
openBinaryTempFile p = unsafeEff_ . D.openBinaryTempFile p . toS

-- | Create a temporary directory. The created directory isn't deleted
-- automatically, so you need to delete it manually.
--
-- The directory is created with permissions such that only the current user
-- can read\/write it.
--
-- @since 0.2.0
createTempDir ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Name of created temporary directory
  Eff es (Path Abs Dir)
createTempDir p = unsafeEff_ . D.createTempDir p . toS

--  * Existence tests

-- | Check if there is a file or directory on specified path.
isLocationOccupied :: (FileSystem :> es) => Path b t -> Eff es Bool
isLocationOccupied = unsafeEff_ . D.isLocationOccupied

-- | If argument of the function throws a
-- 'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
-- exceptions propagate). Otherwise the result is returned inside a 'Just'.
--
-- @since 0.3.0
forgivingAbsence :: (FileSystem :> es ) => IO a -> Eff es (Maybe a)
forgivingAbsence = unsafeEff_ . D.forgivingAbsence


-- | The same as 'forgivingAbsence', but ignores result.
--
-- @since 0.3.1
ignoringAbsence :: (FileSystem :> es) => IO a -> Eff es ()
ignoringAbsence = unsafeEff_ . D.ignoringAbsence

  -- Ask   :: Reader r m r
  -- Local :: (r -> r) -> m a -> Reader r m a
  -}
{-

-- TODO:: hadock permissions 
-- full integration tests demos
----------------------------------------
-- Actions on directories

-- | Lifted 'D.createDir '.
createDir  :: FileSystem :> es => Path b Dir -> Eff es ()
createDir  = unsafeEff_ . D.createDir 

-- | Ensure that a directory exists creating it and its parent directories
-- if necessary. This is just a handy shortcut:
--
-- > ensureDir = createDirIfMissing True
ensureDir  :: FileSystem :> es => Path b Dir -> Eff es ()
ensureDir = unsafeEff_ . D.ensureDir

-- | Lifted 'D.createDirIfMissing'.
createDirIfMissing :: FileSystem :> es => Bool -> Path b Dir -> Eff es ()
createDirIfMissing doCreateParents =
  unsafeEff_ . D.createDirIfMissing doCreateParents

-- | Lifted 'D.removeDir'.
removeDir :: FileSystem :> es => Path b Dir -> Eff es ()
removeDir = unsafeEff_ . D.removeDir

-- | Lifted 'D.removeDirRecur'.
removeDirRecur :: FileSystem :> es => Path b Dir -> Eff es ()
removeDirRecur = unsafeEff_ . D.removeDirRecur

-- | Lifted 'D.removePathForcibly'.
removePathForcibly :: FileSystem :> es => Path b t -> Eff es ()
removePathForcibly = unsafeEff_ . D.removePathForcibly

-- | Lifted 'D.renameDir'.
renameDir :: FileSystem :> es => Path b Dir -> Path b Dir -> Eff es ()
renameDir old = unsafeEff_ . D.renameDir old

-- | Lifted 'D.listDir'.
listDir :: FileSystem :> es => Path b Dir -> Eff es ([Path Abs Dir], [Path Abs File])
listDir = unsafeEff_ . D.listDir

----------------------------------------
-- Current working directory

-- | Lifted 'D.getCurrentDir'.
getCurrentDir :: FileSystem :> es => Eff es (Path Abs Dir)
getCurrentDir = unsafeEff_ D.getCurrentDir

-- | Lifted 'D.setCurrentDir'.
setCurrentDir :: FileSystem :> es => Path b Dir -> Eff es ()
setCurrentDir = unsafeEff_ . D.setCurrentDir

-- | Lifted 'D.withCurrentDir'.
withCurrentDir :: FileSystem :> es => Path b Dir -> Eff es a -> Eff es a
withCurrentDir path = unsafeLiftMapIO (D.withCurrentDir path)

----------------------------------------
-- Pre-defined directories

-- | Lifted 'D.getHomeDir'.
getHomeDir :: FileSystem :> es => Eff es (Path Abs Dir)
getHomeDir = unsafeEff_ D.getHomeDir

-- | Lifted 'D.getXdgDir'.
getXdgDir
  :: FileSystem :> es
  => D.XdgDirectory
  -> Maybe (Path Rel Dir)
  -> Eff es (Path Abs Dir)
getXdgDir xdgDir = unsafeEff_ . D.getXdgDir xdgDir

-- | Lifted 'D.getXdgDirList'.
getXdgDirList
  :: FileSystem :> es
  => D.XdgDirectoryList
  -> Eff es [Path Abs Dir]
getXdgDirList = unsafeEff_ . D.getXdgDirList

-- | Lifted 'D.getAppUserDataDir'.
getAppUserDataDir :: FileSystem :> es => Text -> Eff es (Path Abs Dir)
getAppUserDataDir = unsafeEff_ . D.getAppUserDataDir . toS

-- | Lifted 'D.getUserDocsDir'.
getUserDocsDir :: FileSystem :> es => Eff es (Path Abs Dir)
getUserDocsDir = unsafeEff_ D.getUserDocsDir

-- | Lifted 'D.getTempDir'.
getTempDir :: FileSystem :> es => Eff es (Path Abs Dir)
getTempDir = unsafeEff_ D.getTempDir

----------------------------------------
-- Actions on files

-- | Lifted 'D.removeFile'.
removeFile :: FileSystem :> es => Path b File  -> Eff es ()
removeFile = unsafeEff_ . D.removeFile

-- | Lifted 'D.renameFile'.
renameFile :: FileSystem :> es => Path b File -> Path b File -> Eff es ()
renameFile old = unsafeEff_ . D.renameFile old

-- | Lifted 'D.renamePath'.
renamePath :: FileSystem :> es => Path b t -> Path b t -> Eff es ()
renamePath old = unsafeEff_ . D.renamePath old

-- | Lifted 'D.copyFile'.
copyFile :: FileSystem :> es => Path b File -> Path b File -> Eff es ()
copyFile src = unsafeEff_ . D.copyFile src

-- | Lifted 'D.copyFileWithMetadata'.
copyFileWithMetadata :: FileSystem :> es => Path b File -> Path b File -> Eff es ()
copyFileWithMetadata src dst = unsafeEff_ $ SD.copyFileWithMetadata (toFilePath src) (toFilePath dst)

-- | Lifted 'D.getFileSize'.
getFileSize :: FileSystem :> es => Path b File -> Eff es Integer
getFileSize = unsafeEff_ . D.getFileSize

-- | Lifted 'D.canonicalizePath'.
canonicalizePath :: (D.AnyPath path, FileSystem :> es) => path -> Eff es (D.AbsPath path)
canonicalizePath = unsafeEff_ . D.canonicalizePath

-- | Lifted 'D.makeAbsolute'.
makeAbsolute  :: D.AnyPath path => FileSystem :> es => path -> Eff es (D.AbsPath path)
makeAbsolute = unsafeEff_ . D.makeAbsolute

-- | Lifted 'D.makeRelativeToCurrentDir'.
makeRelativeToCurrentDir ::
 (D.AnyPath path, FileSystem :> es) 
  => path
  -> Eff es (D.RelPath path)
makeRelativeToCurrentDir = unsafeEff_ . D.makeRelativeToCurrentDir

----------------------------------------
-- Existence tests

-- | Lifted 'D.doesPathExist'.
doesPathExist :: FileSystem :> es => Path b t -> Eff es Bool
doesPathExist = unsafeEff_ . D.doesPathExist

-- | Lifted 'D.doesFileExist'.
doesFileExist :: FileSystem :> es => Path b File -> Eff es Bool
doesFileExist = unsafeEff_ . D.doesFileExist

-- | Lifted 'D.doesDirExist'.
doesDirExist :: FileSystem :> es => Path b Dir -> Eff es Bool
doesDirExist = unsafeEff_ . D.doesDirExist

-- | Lifted 'D.findExecutable'.
findExecutable :: FileSystem :> es => Path Rel File -> Eff es (Maybe (Path Abs File))
findExecutable = unsafeEff_ . D.findExecutable

-- | Lifted 'D.findFile'.
findFile :: FileSystem :> es => [Path b Dir] -> Path Rel File -> Eff es (Maybe (Path Abs File))
findFile dirs = unsafeEff_ . D.findFile dirs

-- | Lifted 'D.findFiles'.
findFiles :: FileSystem :> es => [Path b Dir] -> Path Rel File -> Eff es [Path Abs File]
findFiles dirs = unsafeEff_ . D.findFiles dirs

-- | Lifted 'D.findFileWith'.
-- findFileWith
--   :: FileSystem :> es
--   => (Path b t -> Eff es Bool)
--   -> [Path b t]
--   -> Text
--   -> Eff es (Maybe (Path b t))
-- findFileWith p dirs n = unsafeSeqUnliftIO $ \unlift -> do
--   D.findFileWith (unlift . p) dirs n

-- | Lifted 'D.findFilesWith'.
findFilesWith
  :: FileSystem :> es
  => (Path Abs File -> Eff es Bool)
  -> [Path b Dir]
  -> Path Rel File
  -> Eff es [Path Abs File]
findFilesWith p dirs ns = unsafeSeqUnliftIO $ \unlift -> do
  D.findFilesWith (unlift . p) dirs ns

----------------------------------------
-- Symbolic links

-- | Lifted 'D.createFileLink'.
createFileLink :: FileSystem :> es => Path b File -> Path b File -> Eff es ()
createFileLink target = unsafeEff_ . D.createFileLink target

-- | Lifted 'D.createDirLink'.
createDirLink :: FileSystem :> es => Path b Dir -> Path b Dir -> Eff es ()
createDirLink target = unsafeEff_ . D.createDirLink target

-- | Lifted 'D.removeDirLink'.
removeDirLink :: FileSystem :> es => Path b Dir -> Eff es ()
removeDirLink = unsafeEff_ . D.removeDirLink

-- | Lifted 'D.isSymlink'.
isSymlink :: FileSystem :> es => Path b t -> Eff es Bool
isSymlink = unsafeEff_ . D.isSymlink

-- | Lifted 'D.getSymlinkTarget'.
getSymlinkTarget :: FileSystem :> es => Path b t -> Eff es Text
getSymlinkTarget = unsafeEff_ . P.fmap toS . D.getSymlinkTarget 

----------------------------------------
-- Permissions

-- | Lifted 'D.getPermissions'.
getPermissions :: FileSystem :> es => Path b t -> Eff es D.Permissions
getPermissions = unsafeEff_ . D.getPermissions

-- | Lifted 'D.setPermissions'.
setPermissions :: FileSystem :> es => Path b t -> D.Permissions -> Eff es ()
setPermissions path = unsafeEff_ . D.setPermissions path

-- | Lifted 'D.copyPermissions'.
copyPermissions :: FileSystem :> es => Path b t -> Path b t -> Eff es ()
copyPermissions src = unsafeEff_ . D.copyPermissions src

----------------------------------------
-- Timestamps
-- TODO:: change to chronos OffsetTime - note chronos uses minutes for offset

-- | Lifted 'D.getAccessTime'.
getAccessTime :: FileSystem :> es => Path b t -> Eff es UTCTime
getAccessTime = unsafeEff_ . D.getAccessTime

-- | Lifted 'D.getModificationTime'.
getModificationTime :: FileSystem :> es => Path b t -> Eff es UTCTime
getModificationTime = unsafeEff_ . D.getModificationTime

-- | Lifted 'D.setAccessTime'.
setAccessTime :: FileSystem :> es => Path b t -> UTCTime -> Eff es ()
setAccessTime path = unsafeEff_ . D.setAccessTime path

-- | Lifted 'D.setModificationTime'.
setModificationTime :: FileSystem :> es => Path b t -> UTCTime -> Eff es ()
setModificationTime path = unsafeEff_ . D.setModificationTime path


----------------------------------------
-- path-io only

-- | The same as 'listDir' but returns relative paths.
listDirRel ::
  FileSystem :> es =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRel = unsafeEff_ . D.listDirRel

-- | Similar to 'listDir', but recursively traverses every sub-directory
-- /excluding symbolic links/, and returns all files and directories found.
-- This can fail with the same exceptions as 'listDir'.
--
-- __Note__: before version /1.3.0/, this function followed symlinks.
listDirRecur ::
  FileSystem :> es =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Abs Dir], [Path Abs File])
listDirRecur = unsafeEff_ . D.listDirRecur

-- | The same as 'listDirRecur' but returns paths that are relative to the
-- given directory.
listDirRecurRel ::
  FileSystem :> es =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRecurRel = unsafeEff_ . D.listDirRecurRel


-- | Copies a directory recursively. It /does not/ follow symbolic links and
-- preserves permissions when possible. If the destination directory already
-- exists, new files and sub-directories complement its structure, possibly
-- overwriting old files if they happen to have the same name as the new
-- ones.
--
-- __Note__: before version /1.3.0/, this function followed symlinks.
--
-- This function now behaves much like the @cp@ utility, not
-- traversing symlinked directories, but recreating symlinks in the target
-- directory according to their targets in the source directory.
-- TODO :: test including errors - static vs dynamic - 2 * 2 implementations required?
copyDirRecur ::
  (FileSystem :> es, Error e :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur s = unsafeEff_ . D.copyDirRecur s


-- | The same as 'copyDirRecur', but it /does not/ preserve directory
-- permissions. This may be useful, for example, if the directory you want
-- to copy is “read-only”, but you want your copy to be editable.
--
-- @since 1.1.0
--
-- __Note__: before version /1.3.0/, this function followed symlinks.
--
-- __Note__: before version /1.6.0/, the function created empty directories
-- in the destination directory when the source directory contained
-- directory symlinks. The symlinked directories were not recursively
-- traversed. It also copied symlinked files creating normal regular files
-- in the target directory as the result. This was fixed in the version
-- /1.6.0/ so that the function now behaves much like the @cp@ utility, not
-- traversing symlinked directories, but recreating symlinks in the target
-- directory according to their targets in the source directory.
-- TODO :: test including errors - static vs dynamic - 2 * 2 implementations required?
copyDirRecur' ::
  (FileSystem :> es, Error e :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur' s = unsafeEff_ . D.copyDirRecur' s

-- Walking directory trees
-- | Traverse a directory tree using depth first pre-order traversal,
-- calling a handler function at each directory node traversed. The absolute
-- paths of the parent directory, sub-directories and the files in the
-- directory are provided as arguments to the handler.
--
-- The function is capable of detecting and avoiding traversal loops in the
-- directory tree. Note that the traversal follows symlinks by default, an
-- appropriate traversal handler can be used to avoid that when necessary.
-- TODO :: higher order effect ?? - same issue applies to all effects that use IO or 
-- return a handle
walkDir ::
   (IOE :> es, FileSystem :> es) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (D.WalkAction Abs)) ->
  -- | Directory where traversal begins
  Path b Dir ->
  Eff es ()
walkDir h  = unsafeEff_ . D.walkDir h 



-- | The same as 'walkDir' but uses relative paths. The handler is given
-- @dir@, directory relative to the directory where traversal begins.
-- Sub-directories and files are relative to @dir@.
--
-- @since 1.4.2
walkDirRel ::
  ( FileSystem :> es) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  ( Path Rel Dir ->
    [Path Rel Dir] ->
    [Path Rel File] ->
    IO (D.WalkAction Rel)
  ) ->
  -- | Directory where traversal begins
  Path b Dir ->
  Eff es ()
walkDirRel h = unsafeEff_ . D.walkDirRel h


-- | Similar to 'walkDir' but accepts a 'Monoid'-returning output writer as
-- well. Values returned by the output writer invocations are accumulated
-- and returned.
--
-- Both, the descend handler as well as the output writer can be used for
-- side effects but keep in mind that the output writer runs before the
-- descend handler.
walkDirAccum ::
  (FileSystem :> es, P.Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (D.WalkAction Abs)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  Eff es o
walkDirAccum h w = unsafeEff_ . D.walkDirAccum h w

-- | The same as 'walkDirAccum' but uses relative paths. The handler and
-- writer are given @dir@, directory relative to the directory where
-- traversal begins. Sub-directories and files are relative to @dir@.
walkDirAccumRel ::
  (FileSystem :> es, P.Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> IO (D.WalkAction Rel)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> IO o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  Eff es o
walkDirAccumRel h w = unsafeEff_ . D.walkDirAccumRel h w 


-- | Append Textly-typed path to an absolute path and then canonicalize
-- it.
resolveFile ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  Text ->
  Eff es (Path Abs File)
resolveFile b = unsafeEff_ . D.resolveFile b . toS

-- | The same as 'resolveFile', but uses current working directory.
resolveFile' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  Text ->
  Eff es (Path Abs File)
resolveFile' = unsafeEff_ . D.resolveFile' . toS

-- | The same as 'resolveFile', but for directories.
resolveDir ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  Text ->
  Eff es (Path Abs Dir)
resolveDir b =  unsafeEff_ . D.resolveDir b . toS

-- | The same as 'resolveDir', but uses current working directory.
resolveDir' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  Text ->
  Eff es (Path Abs Dir)
resolveDir' =  unsafeEff_ . D.resolveDir' . toS



----------------------------------------------------------------------------
-- Temporary files and directories

-- | Use a temporary file that doesn't already exist.
--
-- Creates a new temporary file inside the given directory, making use of
-- the template. The temporary file is deleted after use.
--
-- @since 0.2.0
withTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> IO a) ->
  Eff es a
withTempFile path t action = unsafeEff_ $ D.withTempFile  path (toS t) action

-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temporary directory is deleted after use.
--
-- @since 0.2.0
withTempDir ::
  (FileSystem :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> IO a) ->
  Eff es a
withTempDir path t action =  unsafeEff_ $ D.withTempDir  path (toS t) action

-- | Create and use a temporary file in the system standard temporary
-- directory.
--
-- Behaves exactly the same as 'withTempFile', except that the parent
-- temporary directory will be that returned by 'getTempDir'.
--
-- @since 0.2.0
withSystemTempFile ::
  (FileSystem :> es) =>
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the file
  (Path Abs File -> P.Handle -> IO a) ->
  Eff es a
withSystemTempFile t action =
  unsafeEff_ $ D.withSystemTempFile (toS t) action

-- | Create and use a temporary directory in the system standard temporary
-- directory.
--
-- Behaves exactly the same as 'withTempDir', except that the parent
-- temporary directory will be that returned by 'getTempDir'.
--
-- @since 0.2.0
withSystemTempDir ::
  (FileSystem :> es) =>
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Callback that can use the directory
  (Path Abs Dir -> IO a) ->
  Eff es a
withSystemTempDir t = unsafeEff_ . D.withSystemTempDir (toS t)

-- | The function creates a temporary file in @rw@ mode. The created file
-- isn't deleted automatically, so you need to delete it manually.
--
-- The file is created with permissions such that only the current user can
-- read\/write it.
--
-- With some exceptions (see below), the file will be created securely in
-- the sense that an attacker should not be able to cause openTempFile to
-- overwrite another file on the filesystem using your credentials, by
-- putting symbolic links (on Unix) in the place where the temporary file is
-- to be created. On Unix the @O_CREAT@ and @O_EXCL@ flags are used to
-- prevent this attack, but note that @O_EXCL@ is sometimes not supported on
-- NFS filesystems, so if you rely on this behaviour it is best to use local
-- filesystems only.
--
-- @since 0.2.0
openTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template; if the template is "foo.ext" then the created
  -- file will be @\"fooXXX.ext\"@ where @XXX@ is some random number
  Text ->
  -- | Name of created file and its 'Handle'
  Eff es  (Path Abs File, P.Handle)
openTempFile p = unsafeEff_ . D.openTempFile p . toS

-- | Like 'openTempFile', but opens the file in binary mode. On Windows,
-- reading a file in text mode (which is the default) will translate @CRLF@
-- to @LF@, and writing will translate @LF@ to @CRLF@. This is usually what
-- you want with text files. With binary files this is undesirable; also, as
-- usual under Microsoft operating systems, text mode treats control-Z as
-- EOF. Binary mode turns off all special treatment of end-of-line and
-- end-of-file characters.
--
-- @since 0.2.0
openBinaryTempFile ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  Text ->
  -- | Name of created file and its 'Handle'
  Eff es (Path Abs File, P.Handle)
openBinaryTempFile p = unsafeEff_ . D.openBinaryTempFile p . toS

-- | Create a temporary directory. The created directory isn't deleted
-- automatically, so you need to delete it manually.
--
-- The directory is created with permissions such that only the current user
-- can read\/write it.
--
-- @since 0.2.0
createTempDir ::
  (FileSystem :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  Text ->
  -- | Name of created temporary directory
  Eff es (Path Abs Dir)
createTempDir p = unsafeEff_ . D.createTempDir p . toS

--  * Existence tests

-- | Check if there is a file or directory on specified path.
isLocationOccupied :: (FileSystem :> es) => Path b t -> Eff es Bool
isLocationOccupied = unsafeEff_ . D.isLocationOccupied

-- | If argument of the function throws a
-- 'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
-- exceptions propagate). Otherwise the result is returned inside a 'Just'.
--
-- @since 0.3.0
forgivingAbsence :: (FileSystem :> es ) => IO a -> Eff es (Maybe a)
forgivingAbsence = unsafeEff_ . D.forgivingAbsence


-- | The same as 'forgivingAbsence', but ignores result.
--
-- @since 0.3.1
ignoringAbsence :: (FileSystem :> es) => IO a -> Eff es ()
ignoringAbsence = unsafeEff_ . D.ignoringAbsence
-}
