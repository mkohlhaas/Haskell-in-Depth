module EvalRPNTrans2 where

import Control.Applicative (Alternative (empty))
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    guard,
    modify,
  )
import MyMaybeT (MaybeT (runMaybeT))
import Text.Read (readMaybe)

type Stack = [Integer]

type EvalM = MaybeT (State Stack)

push :: Integer -> EvalM ()
push x = modify (x :)

pop'' :: EvalM Integer
pop'' = do
  xs <- get
  guard (not $ null xs)
  put (tail xs)
  pure (head xs)

pop :: EvalM Integer
pop = do
  (x : xs) <- get
  put xs
  pure x

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- length <$> get
  guard (l == 1)

readSafe :: (Read a, Alternative m) => String -> m a
readSafe str =
  case readMaybe str of
    Nothing -> empty
    Just n -> pure n

evalRPN :: String -> Maybe Integer
evalRPN str = evalState (runMaybeT evalRPN') []
  where
    evalRPN' = traverse step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push
