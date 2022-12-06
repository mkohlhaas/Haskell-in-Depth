module EvalRPNTrans2 where

import Control.Applicative (Alternative (empty))
import Control.Monad.State (MonadState (get, put), State, evalState, guard, modify, gets)
import MyMaybeT (MaybeT (runMaybeT))
import Text.Read (readMaybe)
import Data.Foldable (traverse_)

type Stack = [Integer]

type EvalM = MaybeT (State Stack)

push ∷ Integer → EvalM ()
push x = modify (x :)

pop' ∷ EvalM Integer
pop' = do
  xs ← get
  guard (not $ null xs)
  put (tail xs)
  pure (head xs)

pop ∷ EvalM Integer
pop = do
  (x : xs) ← get
  put xs
  pure x

oneElementOnStack ∷ EvalM ()
oneElementOnStack = do
  l ← gets length
  guard (l == 1)

readSafe ∷ (Read a, Alternative m) ⇒ String → m a
readSafe str = maybe empty pure (readMaybe str)

evalRPN ∷ String → Maybe Integer
evalRPN str = evalState (runMaybeT evalRPN') []
  where
    evalRPN' ∷ EvalM Integer
    evalRPN' = traverse_ step (words str) >> oneElementOnStack >> pop
    step ∷ String → EvalM ()
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t = readSafe t >>= push
    processTops ∷ (Integer → Integer → Integer) → EvalM ()
    processTops op = flip op <$> pop <*> pop >>= push

-- >>> evalRPN "2 3 + 6 *"
-- Just 30
--
-- >>> evalRPN "2 3"
-- Nothing
--
-- >>> evalRPN "2 +"
-- Nothing

