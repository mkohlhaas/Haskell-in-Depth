import Control.Concurrent (Chan, newChan, readChan, threadDelay, writeChan)
import Control.Concurrent.Async (async, wait)
import Control.Monad (forM_)

oneSec ∷ Int
oneSec = 1000000

-- producer
sendNumbers ∷ [Int] → Chan (Maybe Int) → IO ()
sendNumbers xs ch = do
  forM_ xs $ \x → do
    writeChan ch (Just x)
    threadDelay oneSec
  writeChan ch Nothing

-- consumer
sumNumbers ∷ Chan (Maybe Int) → IO Int
sumNumbers ch = loop 0
  where
    loop acc = do
      next ← readChan ch
      case next of
        Nothing → do
          putStrLn "There are no more numbers."
          pure acc
        Just n → do
          putStrLn $ "We've got a number: " ++ show n
          loop (acc + n)

main ∷ IO ()
main = do
  ch ← newChan
  async (sendNumbers [1 .. 5] ch) --- one thread sends numbers (producer)
  summator ← async (sumNumbers ch) -- another thread sums those numbers (consumer)
  res ← wait summator
  putStrLn $ "Sum is: " ++ show res
