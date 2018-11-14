{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cloudy_front (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.2\\cloudy-front-0.0.1-6XE9x8BJEO1Hgg1ErOdl5h"
dynlibdir  = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.2"
datadir    = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.2\\cloudy-front-0.0.1"
libexecdir = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\cloudy-front-0.0.1-6XE9x8BJEO1Hgg1ErOdl5h\\x86_64-windows-ghc-8.4.2\\cloudy-front-0.0.1"
sysconfdir = "C:\\Users\\Home\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cloudy_front_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cloudy_front_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cloudy_front_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cloudy_front_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cloudy_front_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cloudy_front_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
