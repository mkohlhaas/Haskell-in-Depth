import Control.Concurrent
import Control.Monad

oneSec ∷ Int
oneSec = 1000000

printHello ∷ IO ()
printHello = forever $ do
  putStr "Hello "
  putStrLn "world"

main ∷ IO ()
main = do
  replicateM_ 5 (forkIO printHello)
  threadDelay oneSec
