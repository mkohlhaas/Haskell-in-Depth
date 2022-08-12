{-# LANGUAGE RecordWildCards #-}

module DirTree where

import App
import Utils (currentPathStatus, traverseDirectoryWith)

type Depth = Int

dirTree ∷ MyApp (FilePath, Depth) s ()
dirTree = do
  AppEnv {..} ← ask
  fileStatus ← currentPathStatus
  when (isDirectory fileStatus && depth <= maxDepth cfg) $ do
    tell [(takeBaseName path, depth)]
    traverseDirectoryWith dirTree
