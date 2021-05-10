{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw2_Jorres (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jorres/work/uni-codebase/functional/hw2-Jorres/.stack-work/install/x86_64-linux/b605073424cdf601563dc8347ff8140cdb83ff0f21a604f73c62350122925c1a/8.8.3/bin"
libdir     = "/home/jorres/work/uni-codebase/functional/hw2-Jorres/.stack-work/install/x86_64-linux/b605073424cdf601563dc8347ff8140cdb83ff0f21a604f73c62350122925c1a/8.8.3/lib/x86_64-linux-ghc-8.8.3/hw2-Jorres-0.1.0.0-2618nXVuqjXJjn6ypDKaLa-hw2-Jorres-test"
dynlibdir  = "/home/jorres/work/uni-codebase/functional/hw2-Jorres/.stack-work/install/x86_64-linux/b605073424cdf601563dc8347ff8140cdb83ff0f21a604f73c62350122925c1a/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/jorres/work/uni-codebase/functional/hw2-Jorres/.stack-work/install/x86_64-linux/b605073424cdf601563dc8347ff8140cdb83ff0f21a604f73c62350122925c1a/8.8.3/share/x86_64-linux-ghc-8.8.3/hw2-Jorres-0.1.0.0"
libexecdir = "/home/jorres/work/uni-codebase/functional/hw2-Jorres/.stack-work/install/x86_64-linux/b605073424cdf601563dc8347ff8140cdb83ff0f21a604f73c62350122925c1a/8.8.3/libexec/x86_64-linux-ghc-8.8.3/hw2-Jorres-0.1.0.0"
sysconfdir = "/home/jorres/work/uni-codebase/functional/hw2-Jorres/.stack-work/install/x86_64-linux/b605073424cdf601563dc8347ff8140cdb83ff0f21a604f73c62350122925c1a/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw2_Jorres_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw2_Jorres_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw2_Jorres_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw2_Jorres_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw2_Jorres_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw2_Jorres_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
