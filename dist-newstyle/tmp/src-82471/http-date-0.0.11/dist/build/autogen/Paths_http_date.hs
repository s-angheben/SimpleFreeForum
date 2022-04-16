{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_http_date (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,0,11] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/sam/.cabal/store/ghc-9.0.2/http-date-0.0.11-46e4867dc323767d98f7ae0567bb70082c3452f8835bff3092d169d632e6d26e/bin"
libdir     = "/home/sam/.cabal/store/ghc-9.0.2/http-date-0.0.11-46e4867dc323767d98f7ae0567bb70082c3452f8835bff3092d169d632e6d26e/lib"
dynlibdir  = "/home/sam/.cabal/store/ghc-9.0.2/http-date-0.0.11-46e4867dc323767d98f7ae0567bb70082c3452f8835bff3092d169d632e6d26e/lib"
datadir    = "/home/sam/.cabal/store/ghc-9.0.2/http-date-0.0.11-46e4867dc323767d98f7ae0567bb70082c3452f8835bff3092d169d632e6d26e/share"
libexecdir = "/home/sam/.cabal/store/ghc-9.0.2/http-date-0.0.11-46e4867dc323767d98f7ae0567bb70082c3452f8835bff3092d169d632e6d26e/libexec"
sysconfdir = "/home/sam/.cabal/store/ghc-9.0.2/http-date-0.0.11-46e4867dc323767d98f7ae0567bb70082c3452f8835bff3092d169d632e6d26e/etc"

getBinDir     = catchIO (getEnv "http_date_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "http_date_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "http_date_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "http_date_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "http_date_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "http_date_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
