import Control.Concurrent
import System.Random

oneSec ∷ Int
oneSec = 1000000

randomDelay ∷ IO ()
randomDelay = do
  secs ← getStdRandom (uniformR (1, 5))
  putStrLn $ "Waiting for " ++ show secs ++ "sec."
  threadDelay $ secs * oneSec

-- main ∷ IO ()
-- main = do
--    forkIO randomDelay
--    pure ()

main ∷ IO ()
main = do
  start ← newEmptyMVar
  fin ← newEmptyMVar
  forkFinally (takeMVar start >> randomDelay) (const $ putMVar fin ())
  threadDelay oneSec
  putMVar start ()
  takeMVar fin
  putStrLn "Bye bye ..."
