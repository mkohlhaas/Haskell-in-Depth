import Data.IORef (modifyIORef', newIORef, readIORef)
import Text.Read (readMaybe)

sumNumbers :: IO Int
sumNumbers = do
  acc <- newIORef 0 -- we store our acc in an IORef
  go acc
  where
    go acc = readNumber >>= processNumber acc

    readNumber = do
      putStr "Put integer number (not a number to finish): "
      readMaybe <$> getLine

    processNumber acc Nothing = readIORef acc
    processNumber acc (Just n) = modifyIORef' acc (+ n) >> go acc

main :: IO ()
main = do
  s <- sumNumbers
  putStr "Your sum is: "
  print s
