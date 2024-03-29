import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import System.Environment

oneSec ∷ Int
oneSec = 1000000

printDots ∷ IO ()
printDots = forever $ do
  putStr "."
  tv ← registerDelay oneSec
  atomically $ readTVar tv >>= check

main ∷ IO ()
main = do
  [sec] ← getArgs
  withAsync printDots $ \_ → do
    tv ← registerDelay $ oneSec * read sec
    atomically $ readTVar tv >>= check
  putStrLn "\nAlarm!"
