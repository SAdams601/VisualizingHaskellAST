module Paths_VisualizingHaskellAST (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Stephen/.cabal/bin"
libdir     = "/Users/Stephen/.cabal/lib/x86_64-osx-ghc-7.6.3/VisualizingHaskellAST-0.1.0.0"
datadir    = "/Users/Stephen/.cabal/share/x86_64-osx-ghc-7.6.3/VisualizingHaskellAST-0.1.0.0"
libexecdir = "/Users/Stephen/.cabal/libexec"
sysconfdir = "/Users/Stephen/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "VisualizingHaskellAST_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "VisualizingHaskellAST_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "VisualizingHaskellAST_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "VisualizingHaskellAST_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "VisualizingHaskellAST_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
