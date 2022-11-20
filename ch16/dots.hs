import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forever)
import System.IO (BufferMode (..), hSetBuffering, stdout)

oneSec ∷ Int
oneSec = 1000000

doSomethingUseful ∷ IO ()
doSomethingUseful = do
  threadDelay $ 5 * oneSec
  putStrLn "\nAll done."

printDots ∷ Int → IO ()
printDots msec = forever $ do
  putStr "."
  threadDelay msec

main ∷ IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Start doing something useful"
  dotsPrinter ← forkIO (printDots oneSec)
  doSomethingUseful
  killThread dotsPrinter
  putStrLn "Bye bye ..."
