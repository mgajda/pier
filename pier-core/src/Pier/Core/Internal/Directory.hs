module Pier.Core.Internal.Directory
    ( forFileRecursive_
    , FileItem(..)
    , getRegularContents
    , createParentIfMissing
    , copyDirectory
    , parentDirectory
    ) where

import Control.Exception(catchJust)
import Control.Monad.IO.Class
import Development.Shake.FilePath
import GHC.IO.Exception
import System.Directory
import qualified System.Posix.Files as Posix

-- | Create recursively the parent of the given path, if it doesn't exist.
createParentIfMissing :: MonadIO m => FilePath -> m ()
createParentIfMissing
    = liftIO . createDirectoryIfMissing True . parentDirectory

-- | Get the parent of the given directory or file.
--
-- Examples:
--
-- parentDirectory "foo/bar"  == "foo"
-- parentDirectory "foo/bar/" == "foo"
-- parentDirectory "foo" == ""
parentDirectory :: FilePath -> FilePath
parentDirectory = fixPeriod . takeDirectory . dropTrailingPathSeparator
  where
    fixPeriod "." = ""
    fixPeriod x = x

data FileItem = RegularFile | DirectoryStart | DirectoryEnd | SymbolicLink

forFileRecursive_ :: (FileItem -> FilePath -> IO ()) -> FilePath -> IO ()
forFileRecursive_ act f = do
    isSymLink <- pathIsSymbolicLink f
    if isSymLink
        then act SymbolicLink f
        else do
            isDir <- doesDirectoryExist f
            if not isDir
                then act RegularFile f
                else do
                    act DirectoryStart f
                    getRegularContents f
                        >>= mapM_ (forFileRecursive_ act . (f </>))
                    act DirectoryEnd f

-- | Get the contents of this path, excluding the special files "." and ".."
getRegularContents :: FilePath -> IO [FilePath]
getRegularContents f =
    filter (not . specialFile) <$> getDirectoryContents f
  where
    specialFile "." = True
    specialFile ".." = True
    specialFile _ = False

-- | Copy the directory recursively from the source to the target location.
-- Hard-link files, and copy any symlinks.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dest = do
    createParentIfMissing dest
    forFileRecursive_ act src
  where
    act RegularFile f = linkOrCopyFile f $ dest </> makeRelative src f
    act SymbolicLink f = do
        target <- getSymbolicLinkTarget f
        let g = dest </> makeRelative src f
        createParentIfMissing g
        createFileLink target g
    act DirectoryStart f = createDirectoryIfMissing False (dest </> makeRelative src f)
    act DirectoryEnd _ = return ()

-- | Try to hard-link file, and copy it if it fails because source and destination are on different devices.
linkOrCopyFile :: FilePath -> FilePath -> IO ()
linkOrCopyFile src dst =
  catchJust
     catchUnsupportedLink
    (Posix.createLink src dst)
    (\_ -> copyFile   src dst)

-- | Check if the exception is because we tried to hard-link files on different devices
catchUnsupportedLink :: IOException -> Maybe IOException
catchUnsupportedLink e@(IOError {ioe_errno=Just 18}) = Just e
catchUnsupportedLink _                               = Nothing

