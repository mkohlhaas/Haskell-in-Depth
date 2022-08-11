import Control.Monad.Extra (ifM, whenM, zipWithM_)
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Directory.Extra (doesDirectoryExist, listContents, listFilesRecursive)
import System.Environment (getArgs)

fileCount ∷ FilePath → IO Int
fileCount fpath = do
  counter ← newIORef 0
  whenM (doesDirectoryExist fpath) $ go counter fpath
  readIORef counter
  where
    go counter fp = listContents fp >>= traverse_ (processEntry counter)
    processEntry counter fp = ifM (doesDirectoryExist fp) (go counter fp) (inc counter)
    inc counter = modifyIORef' counter (+ 1)

fileCount' ∷ FilePath → IO Int
fileCount' fp = length <$> listFilesRecursive fp

main ∷ IO ()
main = do
  args ← getArgs
  xs ← traverse fileCount args
  zipWithM_ printEntry args xs
  where
    printEntry fp n = putStrLn (show n ++ "\t" ++ fp)
