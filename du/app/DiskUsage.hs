{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module DiskUsage (diskUsage) where

import App
  ( AppConfig (maxDepth),
    AppEnv (AppEnv, cfg, depth, fileStatus, path),
    FileOffset,
    MonadReader (ask),
    MonadState (get),
    MonadWriter (tell),
    MyApp,
    fileSize,
    isDirectory,
    isRegularFile,
    liftM2,
    modify,
    when,
  )
import Utils (checkExtension, currentPathStatus, traverseDirectoryWith)

-- To compute the total space used by some directory, we have to find the difference between the total space
-- used after leaving the directory and before entering it. NOTE: This task requires traversing the whole
-- directory tree, no matter the maximum depth given!
data DUEntryAction
  = TraverseDir {dirpath :: FilePath, requireReporting :: Bool}
  | RecordFileSize {fsize :: FileOffset}
  | None

type FileSize = FileOffset

type TotalSize = FileOffset

-- Monadic                             ≅ Applicative
-- liftM2 decide ask currentPathStatus ≅ decide <$> ask <*> currentPathStatus
diskUsage :: MyApp (FilePath, FileSize) TotalSize ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
  where
    -- decide :: AppEnv -> FileStatus -> DUEntryActionn
    decide AppEnv {..} fs
      | isDirectory fs =
        TraverseDir path (depth <= maxDepth cfg)
      | isRegularFile fs && checkExtension cfg path =
        RecordFileSize (fileSize fs)
      | otherwise = None

    processEntry TraverseDir {..} = do
      usageOnEntry <- get
      traverseDirectoryWith diskUsage
      when requireReporting $ do
        usageOnExit <- get
        tell [(dirpath, usageOnExit - usageOnEntry)]
    processEntry RecordFileSize {fsize} = modify (+ fsize)
    processEntry None = pure ()
