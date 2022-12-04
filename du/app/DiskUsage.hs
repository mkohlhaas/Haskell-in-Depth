{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module DiskUsage (diskUsage) where

import App
import Utils (checkExtension, currentPathStatus, traverseDirectoryWith)

-- To compute the total space used by some directory, we have to find the difference between the total space
-- used after leaving the directory and before entering it. NOTE: This task requires traversing the whole
-- directory tree, no matter the maximum depth given!
data DUEntryAction
  = TraverseDir {dirpath ∷ !FilePath, requireReporting ∷ !Bool}
  | RecordFileSize {fsize ∷ !FileOffset}
  | None

type FileSize = FileOffset

type TotalSize = FileOffset

-- liftM2 decide ask currentPathStatus ≅ decide <$> ask <*> currentPathStatus
diskUsage ∷ MyApp (FilePath, FileSize) TotalSize ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
  where
    decide AppEnv {..} fileStatus
      | isDirectory fileStatus =
        TraverseDir path (depth <= maxDepth cfg)
      | isRegularFile fileStatus && checkExtension cfg path =
        RecordFileSize (fileSize fileStatus)
      | otherwise = None

    processEntry None = pure ()
    processEntry RecordFileSize {fsize} = modify (+ fsize)
    processEntry TraverseDir {..} = do
      usageOnEntry ← get
      traverseDirectoryWith diskUsage
      when requireReporting $ do
        usageOnExit ← get
        tell [(dirpath, usageOnExit - usageOnEntry)]
