{-# OPTIONS_GHC -Wno-redundant-constraints #-}


{- this is a copy of FileSystem from the Effectful package 
adapted to use path and path-io instead of filepath and directory
- functions renamed to be consistent with path-io
- removed 
  - getDirContents not in path-io 
  - findExecutablesInDirectories not in path-io

-}
module DSL.FileSystemEff
  ( -- * Effect
    FileSystem

    -- ** Handlers
  , runFileSystem

    -- * Actions on directories
  , createDir 
  , createDirIfMissing
  , removeDir
  , removeDirRecur
  , removePathForcibly
  , renameDir
  , listDir

    -- ** Current working directory
  , getCurrentDir
  , setCurrentDir
  , withCurrentDir

    -- * Pre-defined directories
  , getHomeDir
  , getXdgDir
  , getXdgDirList
  , getAppUserDataDir
  , getUserDocsDir
  , getTempDir

    -- * Actions on files
  , removeFile
  , renameFile
  , renamePath
  , copyFile
  , copyFileWithMetadata
  , getFileSize
  , canonicalizePath
  , makeAbsolute
  , makeRelativeToCurrentDir

    -- * Existence tests
  , doesPathExist
  , doesFileExist
  , doesDirExist
  , findExecutable
  , findFile
  , findFiles
  -- , findFileWith
  , findFilesWith

    -- * Symbolic links
  , createFileLink
  , createDirLink
  , removeDirLink
  , isSymlink
  , getSymlinkTarget

    -- * Permissions
  , getPermissions
  , setPermissions
  , copyPermissions

    -- * Timestamps
  , getAccessTime
  , getModificationTime
  , setAccessTime
  , setModificationTime

    -- * Re-exports

    -- ** Pre-defined directories
  , D.XdgDirectory(..)
  , D.XdgDirectoryList(..)

    -- ** Existence tests
  , exeExtension

    -- ** Permissions
  , D.Permissions
  , D.emptyPermissions
  , D.readable
  , D.writable
  , D.executable
  , D.searchable
  , D.setOwnerReadable
  , D.setOwnerWritable
  , D.setOwnerExecutable
  , D.setOwnerSearchable
  ) where

{-
module Path b t.IO
  ( -- * Actions on directories
    createDir,
    createDirIfMissing,
    ensureDir,
    removeDir,
    removeDirRecur,
    removePathForcibly,
    renameDir,
    renamePath,
    listDir,
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

    -- ** Current working directory
    getCurrentDir,
    setCurrentDir,
    withCurrentDir,

    -- * Pre-defined directories
    getHomeDir,
    getAppUserDataDir,
    getUserDocsDir,
    getTempDir,
    D.XdgDir (..),
    getXdgDir,
    D.XdgDirList (..),
    getXdgDirList,

    -- * Path b t transformation
    AnyPath (..),
    resolveFile,
    resolveFile',
    resolveDir,
    resolveDir',

    -- * Actions on files
    removeFile,
    renameFile,
    copyFile,
    getFileSize,
    findExecutable,
    findFile,
    findFiles,
    findFilesWith,

    -- * Symbolic links
    createFileLink,
    createDirLink,
    removeDirLink,
    getSymlinkTarget,
    isSymlink,

    -- * Temporary files and directories
    withTempFile,
    withTempDir,
    withSystemTempFile,
    withSystemTempDir,
    openTempFile,
    openBinaryTempFile,
    createTempDir,

    -- * Existence tests
    doesPathExist,
    doesFileExist,
    doesDirExist,
    isLocationOccupied,
    forgivingAbsence,
    ignoringAbsence,

    -- * Permissions
    D.Permissions,
    D.emptyPermissions,
    D.readable,
    D.writable,
    D.executable,
    D.searchable,
    D.setOwnerReadable,
    D.setOwnerWritable,
    D.setOwnerExecutable,
    D.setOwnerSearchable,
    getPermissions,
    setPermissions,
    copyPermissions,

    -- * Timestamps
    getAccessTime,
    setAccessTime,
    setModificationTime,
    getModificationTime,
  )


-}
import Data.Time (UTCTime)
import Path 
import qualified Path.IO as D
import qualified Prelude as P
import Prelude (Bool(..), IO, Maybe(..), Text, (.), ($), (==), (||), Integer)

import Effectful
import Effectful.Dispatch.Static
import DSL.FileSystem.EffectStatic
-- import qualified System.Directory as SD
import PyrethrumExtras (toS, uu)

exeExtension :: Text
exeExtension = uu --toS SD.exeExtension

----------------------------------------
-- Actions on directories

-- | Lifted 'D.createDir '.
createDir  :: FileSystem :> es => Path b Dir -> Eff es ()
createDir  = unsafeEff_ . D.createDir 

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
copyFileWithMetadata src dst = unsafeEff_ $ uu -- SD.copyFileWithMetadata (toFilePath src) (toFilePath dst)

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
