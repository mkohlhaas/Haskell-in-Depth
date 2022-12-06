{-# LANGUAGE NamedFieldPuns #-}

module Utils where

import App
import Data.Foldable (traverse_)
import System.Directory (listDirectory)

traverseDirectoryWith ∷ MyApp le s () → MyApp le s ()
traverseDirectoryWith app =
  asks path >>= liftIO . listDirectory >>= traverse_ go
  where
    go fpath = flip local app $
      \env →
        env
          { path = path env </> fpath,
            depth = depth env + 1
          }

traverseDirectoryWith' ∷ MyApp le s () → MyApp le s ()
traverseDirectoryWith' app = do
  curPath ← asks path
  fPaths ← liftIO $ listDirectory curPath
  traverse_ go fPaths
  where
    go fpath = flip local app $
      \env →
        env
          { path = path env </> fpath,
            depth = depth env + 1
          }

currentPathStatus ∷ MyApp l s FileStatus
currentPathStatus = do
  AppEnv {fileStatusFn, path} ← ask
  liftIO $ fileStatusFn path

checkExtension ∷ AppConfig → FilePath → Bool
checkExtension cfg fp = maybe True (`isExtensionOf` fp) (extension cfg)
