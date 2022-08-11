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
pop' = state $ \(x:xs) → (x, xs)

evalRPN ∷ String → Integer
evalRPN expr = evalState evalRPN' []
  where
    evalRPN' = traverse_ step (words expr) >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t = read t & push
    processTops op = flip op <$> pop <*> pop >>= push
