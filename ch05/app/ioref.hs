import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Text.Read (readMaybe)

sumNumbers ∷ IO Int
sumNumbers = do
  acc ← newIORef 0
  go acc
  where
    go ∷ IORef Int → IO Int
    go acc = readNumber >>= processNumber acc

    readNumber ∷ IO (Maybe Int)
    readNumber = do
      putStrLn "Put integer number (not a number to finish): "
      readMaybe <$> getLine

    processNumber ∷ IORef Int → Maybe Int → IO Int
    processNumber acc Nothing = readIORef acc
    processNumber acc (Just n) = modifyIORef' acc (+ n) >> go acc

main ∷ IO ()
main = do
  s ← sumNumbers
  putStrLn $ "Your sum is: " <> show s
