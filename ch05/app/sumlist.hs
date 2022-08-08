import Control.Monad.State (MonadState (get, put), State, execState, modify' {- -- avoid excessive thunking -})
import Data.Foldable (traverse_)

type IntS = State Integer

addItem ∷ Integer → IntS ()
addItem n = do
  s ← get
  put $ s + n

addItem' ∷ Integer → IntS ()
addItem' n = modify' (+ n)

sumList ∷ [Integer] → IntS ()
sumList = traverse_ addItem'

answer ∷ Integer
answer = execState (sumList [1 .. 100]) 0

main ∷ IO ()
main = print answer
