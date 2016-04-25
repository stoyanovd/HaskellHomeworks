module Paths_Yurasic (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dima/.cabal/bin"
libdir     = "/home/dima/.cabal/lib/x86_64-linux-ghc-7.10.3/Yurasic-0.1.0.0-1NugHgjnh78E3rIWg0yCnV"
datadir    = "/home/dima/.cabal/share/x86_64-linux-ghc-7.10.3/Yurasic-0.1.0.0"
libexecdir = "/home/dima/.cabal/libexec"
sysconfdir = "/home/dima/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Yurasic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Yurasic_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Yurasic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Yurasic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Yurasic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
