import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad (forever)
import System.IO (BufferMode (..), hSetBuffering, stdout)

oneSec ∷ Int
oneSec = 1000000

doSomethingUseful ∷ IO ()
doSomethingUseful = do
  threadDelay $ 5 * oneSec
  putStrLn "All done"

printDots ∷ Int → IO ()
printDots msec = forever $ do
  putStr "."
  threadDelay msec

main ∷ IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Start doing something useful"
  withAsync (printDots oneSec) $ const doSomethingUseful
  putStrLn "Bye bye ..."
