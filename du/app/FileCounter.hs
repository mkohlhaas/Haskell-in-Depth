module FileCounter (fileCount) where

import App
  ( AppConfig (maxDepth),
    AppEnv (AppEnv, cfg, depth, fileStatus, path),
    MonadIO (liftIO),
    MonadReader (ask),
    MonadWriter (tell),
    MyApp,
    isDirectory,
    when,
  )
import System.Directory.Extra (listFiles)
import Utils
  ( checkExtension,
    currentPathStatus,
    traverseDirectoryWith,
  )

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
  AppEnv {..} <- ask
  fs <- currentPathStatus
  when (isDirectory fs && depth <= maxDepth cfg) $ do
    traverseDirectoryWith fileCount
    files <- liftIO $ listFiles path
    tell [(path, length $ filter (checkExtension cfg) files)]
