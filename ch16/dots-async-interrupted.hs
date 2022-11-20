import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
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

waitEnter ∷ IO ()
waitEnter = getLine >> pure ()

main ∷ IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Start doing something useful"
  race_ (printDots oneSec) $ race_ waitEnter doSomethingUseful
  {-
    withAsync (printDots oneSec) $ \_ →
      withAsync waitEnter $ \enter →
        withAsync doSomethingUseful $ \useful →
          waitEither_ enter useful
  -}
  putStrLn "Bye bye ..."
