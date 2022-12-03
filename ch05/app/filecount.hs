import Control.Monad.Extra (ifM, whenM, zipWithM_)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import System.Directory.Extra (doesDirectoryExist, listContents, listFilesRecursive)
import System.Environment (getArgs)

fileCount ∷ FilePath → IO Int
fileCount fpath = do
  counter ← newIORef 0
  whenM (doesDirectoryExist fpath) $ go counter fpath
  readIORef counter
  where
    go ∷ Num a ⇒ IORef a → FilePath → IO ()
    go counter fp = listContents fp >>= mapM_ (processEntry counter)
    processEntry ∷ Num a ⇒ IORef a → FilePath → IO ()
    processEntry counter fp = ifM (doesDirectoryExist fp) (go counter fp) (inc counter)
    inc ∷ Num a ⇒ IORef a → IO ()
    inc counter = modifyIORef' counter (+ 1)

-- >>> n ← fileCount "."
-- >>> n
-- 144

-- using library provided function ;-)
fileCount' ∷ FilePath → IO Int
fileCount' fp = length <$> listFilesRecursive fp

-- >>> n ← fileCount' "."
-- >>> n
-- 144

main ∷ IO ()
main = do
  filePaths ← getArgs
  xs ← mapM fileCount filePaths
  zipWithM_ printEntry filePaths xs
  where
    printEntry ∷ Show a ⇒ String → a → IO ()
    printEntry fp n = putStrLn (show n ++ "\t" ++ fp)
