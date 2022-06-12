import Control.Monad.State
  ( MonadState (get, put),
    State,
    execState,
    modify',
  )
import Data.Foldable (traverse_)

addItem :: Integer -> State Integer ()
addItem n = do
  s <- get
  put (s + n)

addItem' :: Integer -> State Integer ()
addItem' n = modify' (+ n)

sumList :: [Integer] -> State Integer ()
-- sumList xs = traverse_ addItem xs
sumList = traverse_ addItem

answer :: Integer
answer = execState (sumList [1 .. 100]) 0

main :: IO ()
main = print answer
