{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.FileSystemDynamic (
  -- * Effect
  FileSystem,
  -- -- ** Handlers
  -- runFileSystem,
  -- -- * Actions on directories
  -- createDir,
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
  -- ensureDir,
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
import qualified Path.IO as PIO
import Prelude (Bool (..), ByteString, Either (..), Exception, Handle, IO, IOMode, Integer, Maybe (..), Show, Text, pure, ($), (.), (=<<), (==), (>>=), (||))
import qualified Prelude as P

import BasePrelude (IOException)
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
  WithCurrentDir :: Path Abs Dir -> m a -> FileSystem m a

makeEffect ''FileSystem

type instance DispatchOf FileSystem = Dynamic

newtype FSException = FSException IOException
  deriving (Show)

instance Exception FSException

runFileSystemHOE :: forall es a. (HasCallStack, IOE :> es, E.Error FSException :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemHOE =
  interpret $ \env ->
    \case
      WithCurrentDir p action ->
        localSeqUnliftIO env (\unlift -> R.withCurrentDir p (unlift action))

runFileSystemHOE' :: forall es a. (HasCallStack, IOE :> es, E.Error FSException :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemHOE' =
  interpret $ \env ->
    \case
      WithCurrentDir p action ->
        rethrow $
          localSeqUnliftIO
            env
            ( \unlift ->
                R.withCurrentDir p (unlift action)
            )
 where
  -- catch from Control.Monad.Catch (catch) in the exceptions package
  rethrow = handle (\(e :: IOException) -> throwError . FSException $ e)

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
