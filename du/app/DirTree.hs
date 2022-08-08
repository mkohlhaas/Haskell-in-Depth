{-# LANGUAGE RecordWildCards #-}

module DirTree where

import App (AppConfig (maxDepth), AppEnv (AppEnv, cfg, depth, fileStatus, path), MonadReader (ask), MonadWriter (tell), MyApp, isDirectory, takeBaseName, when)
import Utils (currentPathStatus, traverseDirectoryWith)

type Depth = Int

dirTree ∷ MyApp (FilePath, Depth) s ()
dirTree = do
  AppEnv {..} ← ask
  fs ← currentPathStatus
  when (isDirectory fs && depth <= maxDepth cfg) $ do
    tell [(takeBaseName path, depth)]
    traverseDirectoryWith dirTree
