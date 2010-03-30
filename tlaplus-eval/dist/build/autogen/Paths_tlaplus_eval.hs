module Paths_tlaplus_eval (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/reto/.cabal/bin"
libdir     = "/Users/reto/.cabal/lib/tlaplus-eval-0.1/ghc-6.10.3"
datadir    = "/Users/reto/.cabal/share/tlaplus-eval-0.1"
libexecdir = "/Users/reto/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "tlaplus_eval_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "tlaplus_eval_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "tlaplus_eval_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "tlaplus_eval_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
