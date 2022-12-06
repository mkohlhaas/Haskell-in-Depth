{-# LANGUAGE NamedFieldPuns #-}

module Utils where

import App
import Data.Foldable (traverse_)
import System.Directory (listDirectory)

-- monadic style without do notation
traverseDirectoryWith ∷ MyApp logentry state () → MyApp logentry state ()
traverseDirectoryWith app =
  asks path >>= liftIO . listDirectory >>= traverse_ go
  where
    go fpath = flip local app $
      \env →
        env
          { path = path env </> fpath,
            depth = depth env + 1
          }

-- monadic style with do notation
traverseDirectoryWith' ∷ MyApp logentry state () → MyApp logentry state ()
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

currentFileStatus ∷ MyApp logentry state FileStatus
currentFileStatus = do
  AppEnv {fileStatusFn, path} ← ask
  liftIO $ fileStatusFn path

checkExtension ∷ AppConfig → FilePath → Bool
checkExtension cfg fp = maybe True (`isExtensionOf` fp) (extension cfg)
