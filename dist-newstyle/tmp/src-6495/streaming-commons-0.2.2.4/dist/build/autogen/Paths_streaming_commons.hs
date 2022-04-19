{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_streaming_commons (
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
version = Version [0,2,2,4] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/sam/.cabal/store/ghc-9.0.2/streaming-commons-0.2.2.4-ac693a894c91c0c2c7db0104e51914a2270e1e67f4277d85d9b2058a18d7686e/bin"
libdir     = "/home/sam/.cabal/store/ghc-9.0.2/streaming-commons-0.2.2.4-ac693a894c91c0c2c7db0104e51914a2270e1e67f4277d85d9b2058a18d7686e/lib"
dynlibdir  = "/home/sam/.cabal/store/ghc-9.0.2/streaming-commons-0.2.2.4-ac693a894c91c0c2c7db0104e51914a2270e1e67f4277d85d9b2058a18d7686e/lib"
datadir    = "/home/sam/.cabal/store/ghc-9.0.2/streaming-commons-0.2.2.4-ac693a894c91c0c2c7db0104e51914a2270e1e67f4277d85d9b2058a18d7686e/share"
libexecdir = "/home/sam/.cabal/store/ghc-9.0.2/streaming-commons-0.2.2.4-ac693a894c91c0c2c7db0104e51914a2270e1e67f4277d85d9b2058a18d7686e/libexec"
sysconfdir = "/home/sam/.cabal/store/ghc-9.0.2/streaming-commons-0.2.2.4-ac693a894c91c0c2c7db0104e51914a2270e1e67f4277d85d9b2058a18d7686e/etc"

getBinDir     = catchIO (getEnv "streaming_commons_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "streaming_commons_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "streaming_commons_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "streaming_commons_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "streaming_commons_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "streaming_commons_sysconfdir") (\_ -> return sysconfdir)




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
