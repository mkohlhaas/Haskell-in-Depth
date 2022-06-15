{-# LANGUAGE NamedFieldPuns #-}

module Utils where

import App
  ( AppConfig (extension),
    AppEnv (AppEnv, depth, fileStatus, path),
    FileStatus,
    MonadIO (liftIO),
    MonadReader (ask, local),
    MyApp,
    asks,
    isExtensionOf,
    (</>),
  )
import Data.Foldable (traverse_)
import System.Directory (listDirectory)

traverseDirectoryWith :: MyApp le s () -> MyApp le s ()
traverseDirectoryWith app = do
  curPath <- asks path
  content <- liftIO $ listDirectory curPath -- https://hackage.haskell.org/package/directory-1.3.7.0/docs/System-Directory.html#v:listDirectory
  traverse_ go content
  where
    go name = flip local app $
      \env ->
        env
          { path = path env </> name,
            depth = depth env + 1
          }

traverseDirectoryWith' :: MyApp le s () -> MyApp le s ()
traverseDirectoryWith' app =
  asks path >>= liftIO . listDirectory >>= traverse_ go
  where
    go name = flip local app $
      \env ->
        env
          { path = path env </> name,
            depth = depth env + 1
          }

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
  AppEnv {fileStatus, path} <- ask
  liftIO $ fileStatus path

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg fp =
  maybe True (`isExtensionOf` fp) (extension cfg) -- https://hackage.haskell.org/package/filepath-1.4.2.2/docs/System-FilePath.html#v:isExtensionOf
