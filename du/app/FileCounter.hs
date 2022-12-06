{-# LANGUAGE RecordWildCards #-}

module FileCounter (fileCount) where

import App
import Slist (filter, slist)
import System.Directory.Extra (listFiles)
import Utils (checkExtension, currentFileStatus, traverseDirectoryWith)
import Prelude hiding (filter)

type NumberOfFiles = Int

fileCount ∷ MyApp (FilePath, NumberOfFiles) s ()
fileCount = do
  AppEnv {..} ← ask
  fileStatus ← currentFileStatus
  when (isDirectory fileStatus && depth <= maxDepth cfg) $ do
    files ← liftIO $ slist <$> listFiles path
    tell [(path, length $ filter (checkExtension cfg) files)]
    traverseDirectoryWith fileCount
