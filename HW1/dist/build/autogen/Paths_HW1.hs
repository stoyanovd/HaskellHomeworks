module Paths_HW1 (
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

bindir     = "/home/dima/.cabal/bin"
libdir     = "/home/dima/.cabal/lib/x86_64-linux-ghc-7.6.3/HW1-0.1.0.0"
datadir    = "/home/dima/.cabal/share/x86_64-linux-ghc-7.6.3/HW1-0.1.0.0"
libexecdir = "/home/dima/.cabal/libexec"
sysconfdir = "/home/dima/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HW1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HW1_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HW1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HW1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HW1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
