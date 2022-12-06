{-# LANGUAGE RecordWildCards #-}

module DirTree where

import App
import Utils (currentFileStatus, traverseDirectoryWith)

type Depth = Int

dirTree ∷ MyApp (FilePath, Depth) state ()
dirTree = do
  AppEnv {..} ← ask
  fileStatus ← currentFileStatus
  when (isDirectory fileStatus && depth <= maxDepth cfg) $ do
    when (depth /= 0) (tell [(takeBaseName path, depth)])
    traverseDirectoryWith dirTree
