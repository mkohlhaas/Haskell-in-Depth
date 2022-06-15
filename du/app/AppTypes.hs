{-# LANGUAGE RecordWildCards #-}

module AppTypes where

import System.PosixCompat.Files (FileStatus, getFileStatus, getSymbolicLinkStatus)

-- read-only (defined by command line params provided by the user)
data AppConfig = AppConfig
  { basePath :: FilePath,
    maxDepth :: Int,
    extension :: Maybe String,
    followSymlinks :: Bool
  }

-- read-write (run-time info)
data AppEnv = AppEnv
  { cfg :: AppConfig,
    path :: FilePath, -- current path
    depth :: Int, -- current depth
    fileStatus :: FilePath -> IO FileStatus
  }

initialEnv :: AppConfig -> AppEnv
initialEnv config@AppConfig {..} =
  AppEnv
    { cfg = config,
      path = basePath,
      depth = 0,
      fileStatus =
        if followSymlinks
          then getFileStatus -- Querying the file status: https://hackage.haskell.org/package/unix-2.7.2.2/docs/System-Posix-Files.html#g:6
          else getSymbolicLinkStatus -- FileStatus information of the symbolic link itself is returned instead of that of the file it points to.
    }
