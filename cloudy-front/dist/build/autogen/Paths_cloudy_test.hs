{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_cloudy_test (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\cloudy-test-0.0.1-Hi3xKtPTvYdJc36CtpJn4f"
datadir    = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\cloudy-test-0.0.1"
libexecdir = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\cloudy-test-0.0.1-Hi3xKtPTvYdJc36CtpJn4f"
sysconfdir = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cloudy_test_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cloudy_test_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cloudy_test_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cloudy_test_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cloudy_test_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
