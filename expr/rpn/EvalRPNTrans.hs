module EvalRPNTrans where

import Control.Applicative (Alternative (empty))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, evalStateT, gets, guard, modify, when)
import Data.Foldable (traverse_)
import Text.Read (readMaybe)

type Stack = [Integer]

type EvalM = StateT Stack Maybe

push ∷ Integer → EvalM ()
push x = modify (x :)

pop' ∷ EvalM Integer
pop' = do
  xs ← get
  when (null xs) $ lift Nothing -- we are in the State monad; lift Maybe into State
  put $ tail xs
  pure $ head xs

pop'' ∷ EvalM Integer
pop'' = do
  xs ← get
  guard (not $ null xs) -- another way to convey an error
  put $ tail xs
  pure $ head xs

pop ∷ EvalM Integer
pop = do
  (x : xs) ← get -- if pattern matchting fails, we get `Nothing` b/c of do block MonadFail desugaring (p. 177)
  put xs
  pure x

oneElementOnStack ∷ EvalM ()
oneElementOnStack = do
  l ← gets length
  guard (l == 1)

oneElementOnStack' ∷ EvalM ()
oneElementOnStack' = do
  [x] ← get
  pure ()

readSafe ∷ (Alternative f, Read a) ⇒ String → f a
readSafe str = maybe empty pure (readMaybe str)

evalRPN ∷ String → Maybe Integer
evalRPN str = evalStateT evalRPN' []
  where
    evalRPN' = traverse_ step (words str) >> oneElementOnStack' >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push
