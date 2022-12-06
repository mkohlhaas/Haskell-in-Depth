module EvalRPN where

import Control.Monad.State (MonadState (get, put, state), State, evalState, modify)
import Data.Foldable (traverse_)
import Data.Function ((&))

type Stack = [Integer]

type EvalM = State Stack

push ∷ Integer → EvalM ()
push x = modify (x :)

pop ∷ EvalM Integer
pop = do
  xs ← get
  put $ tail xs
  pure $ head xs

pop' ∷ EvalM Integer
pop' = state $ \(x : xs) → (x, xs)

evalRPN ∷ String → Integer
evalRPN expr = evalState evalRPN' []
  where
    evalRPN' ∷ State Stack Integer
    evalRPN' = traverse_ step (words expr) >> pop
    step ∷ String → State Stack ()
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t = read t & push
    processTops ∷ (Integer → Integer → Integer) → State Stack ()
    processTops op = flip op <$> pop <*> pop >>= push

-- >>> evalRPN "2 3 + 6 *"
-- 30

-- >>> evalRPN "2 3"
-- 3

-- >>> evalRPN "2 +"
-- Prelude.head: empty list

