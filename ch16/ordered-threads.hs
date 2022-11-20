import Control.Concurrent.Async (async)
import Control.Concurrent.STM (STM, TVar, atomically, check, newTVarIO, readTVar, writeTVar)
import Control.Monad (forM_, forever, when)
import Text.Read (readMaybe)

maxThread ∷ Int
maxThread = 5

waitUnless ∷ TVar Int → Int → STM ()
waitUnless tv n = do
  n' ← readTVar tv
  check $ n == n'

hello ∷ TVar Int → Int → IO ()
hello tv n = forever $ do
  atomically $ waitUnless tv n
  putStrLn $ "Hello from thread " ++ show n
  atomically $ writeTVar tv 0

userLoop ∷ TVar Int → IO ()
userLoop tv = do
  atomically $ waitUnless tv 0
  putStrLn $ "Enter thread number (1.." ++ show maxThread ++ "):"
  n' ← readMaybe <$> getLine
  case n' of
    Nothing → pure ()
    Just n → when (1 <= n && n <= maxThread) $ do
      atomically $ writeTVar tv n
      userLoop tv

main ∷ IO ()
main = do
  tv ← newTVarIO 0
  forM_ [1 .. maxThread] $ async . hello tv
  userLoop tv
