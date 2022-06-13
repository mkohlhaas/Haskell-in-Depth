import Control.Monad.Extra (ifM, whenM, zipWithM_) -- Tip: Extra package by Neil Mitchell: https://hackage.haskell.org/package/extra
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Directory.Extra (doesDirectoryExist, listContents, listFilesRecursive)
import System.Environment (getArgs)

fileCount :: FilePath -> IO Int
fileCount fpath = do
  counter <- newIORef 0
  whenM (doesDirectoryExist fpath) $ go counter fpath
  readIORef counter
  where
    go cnt fp = listContents fp >>= traverse_ (processEntry cnt)
    processEntry cnt fp = ifM (doesDirectoryExist fp) (go cnt fp) (inc cnt)
    inc cnt = modifyIORef' cnt (+ 1)

fileCount' :: FilePath -> IO Int
fileCount' fp = length <$> listFilesRecursive fp

main :: IO ()
main = do
  args <- getArgs
  xs <- traverse fileCount args
  zipWithM_ printEntry args xs
  where
    printEntry fp n = putStrLn (show n ++ "\t" ++ fp)
