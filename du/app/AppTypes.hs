{-# LANGUAGE RecordWildCards #-}

module AppTypes where

import System.PosixCompat.Files (FileStatus, getFileStatus, getSymbolicLinkStatus)

-- read-only (defined via command line params provided by the user)
data AppConfig = AppConfig
  { basePath ∷ !FilePath,
    maxDepth ∷ !Int,
    extension ∷ !(Maybe String),
    followSymlinks ∷ !Bool
  }

-- read-write (run-time info)
data AppEnv = AppEnv
  { cfg ∷ !AppConfig,
    path ∷ !FilePath, -------------------------- current file path
    depth ∷ !Int, ------------------------------ current directory depth
    fileStatusFn ∷ !(FilePath → IO FileStatus)
  }

initialEnv ∷ AppConfig → AppEnv
initialEnv config@AppConfig {..} =
  AppEnv
    { cfg = config,
      path = basePath,
      depth = 0,
      fileStatusFn = if followSymlinks then getFileStatus else getSymbolicLinkStatus
    }
