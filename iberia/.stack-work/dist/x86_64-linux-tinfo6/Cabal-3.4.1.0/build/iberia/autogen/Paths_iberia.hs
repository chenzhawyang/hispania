{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_iberia (
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

bindir     = "/home/czy/impl/hispania/iberia/.stack-work/install/x86_64-linux-tinfo6/da3c6eb2bc1ea936b6534a287b98f2262ab317b3a962e44d0632a54ba55f2b8f/9.0.2/bin"
libdir     = "/home/czy/impl/hispania/iberia/.stack-work/install/x86_64-linux-tinfo6/da3c6eb2bc1ea936b6534a287b98f2262ab317b3a962e44d0632a54ba55f2b8f/9.0.2/lib/x86_64-linux-ghc-9.0.2/iberia-0.1.0.0-E5gAUv5eKN7iowv1bM4oK-iberia"
dynlibdir  = "/home/czy/impl/hispania/iberia/.stack-work/install/x86_64-linux-tinfo6/da3c6eb2bc1ea936b6534a287b98f2262ab317b3a962e44d0632a54ba55f2b8f/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/czy/impl/hispania/iberia/.stack-work/install/x86_64-linux-tinfo6/da3c6eb2bc1ea936b6534a287b98f2262ab317b3a962e44d0632a54ba55f2b8f/9.0.2/share/x86_64-linux-ghc-9.0.2/iberia-0.1.0.0"
libexecdir = "/home/czy/impl/hispania/iberia/.stack-work/install/x86_64-linux-tinfo6/da3c6eb2bc1ea936b6534a287b98f2262ab317b3a962e44d0632a54ba55f2b8f/9.0.2/libexec/x86_64-linux-ghc-9.0.2/iberia-0.1.0.0"
sysconfdir = "/home/czy/impl/hispania/iberia/.stack-work/install/x86_64-linux-tinfo6/da3c6eb2bc1ea936b6534a287b98f2262ab317b3a962e44d0632a54ba55f2b8f/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "iberia_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "iberia_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "iberia_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "iberia_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "iberia_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "iberia_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
