import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (forever)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Random

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
  withAsync (printDots oneSec) $ \_ →
    withAsync doSomethingUseful $ \useful → do
      threadDelay (2 * oneSec)
      interrupt ← getStdRandom uniform
      if interrupt then cancel useful else wait useful >> pure ()
  putStrLn "Bye bye ..."
