{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ex3code (
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

bindir     = "/Users/stianismar/Dropbox/gitProjects/TDT4165-Programming-languages-Haskell-Scala-Prolog/Haskell/ex3code/.stack-work/install/x86_64-osx/lts-11.16/8.2.2/bin"
libdir     = "/Users/stianismar/Dropbox/gitProjects/TDT4165-Programming-languages-Haskell-Scala-Prolog/Haskell/ex3code/.stack-work/install/x86_64-osx/lts-11.16/8.2.2/lib/x86_64-osx-ghc-8.2.2/ex3code-0.1.0.0-JonQ4Y5qAO3HI0Q4YbRCrX-ex3code-test"
dynlibdir  = "/Users/stianismar/Dropbox/gitProjects/TDT4165-Programming-languages-Haskell-Scala-Prolog/Haskell/ex3code/.stack-work/install/x86_64-osx/lts-11.16/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/stianismar/Dropbox/gitProjects/TDT4165-Programming-languages-Haskell-Scala-Prolog/Haskell/ex3code/.stack-work/install/x86_64-osx/lts-11.16/8.2.2/share/x86_64-osx-ghc-8.2.2/ex3code-0.1.0.0"
libexecdir = "/Users/stianismar/Dropbox/gitProjects/TDT4165-Programming-languages-Haskell-Scala-Prolog/Haskell/ex3code/.stack-work/install/x86_64-osx/lts-11.16/8.2.2/libexec/x86_64-osx-ghc-8.2.2/ex3code-0.1.0.0"
sysconfdir = "/Users/stianismar/Dropbox/gitProjects/TDT4165-Programming-languages-Haskell-Scala-Prolog/Haskell/ex3code/.stack-work/install/x86_64-osx/lts-11.16/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ex3code_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ex3code_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ex3code_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ex3code_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ex3code_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ex3code_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
