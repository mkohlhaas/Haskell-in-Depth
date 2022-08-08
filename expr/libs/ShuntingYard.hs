module ShuntingYard (convertToExpr) where

import Control.Monad.State (MonadState (get, put), State, execState, gets, modify, when)
import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.Foldable (traverse_)
import Data.List (groupBy)
import Expr (Expr (..))

type Token = String

type Stack = [Token]

type Output = [Expr Integer]

type SYState = State (Stack, Output)

isEmpty ∷ SYState Bool
isEmpty = gets $ null . fst

notEmpty ∷ SYState Bool
notEmpty = not <$> isEmpty

top ∷ SYState Token
top = gets $ head . fst -- let it crash on empty stack

pop ∷ SYState Token
pop = do
  (s, es) ← get
  put (tail s, es) -- let it crash on empty stack
  pure $ head s

pop_ ∷ SYState () -- let it crash on empty stack
pop_ = modify $ first tail

push ∷ Token → SYState ()
push t = modify $ first (t :)

whileNotEmptyAnd ∷ (Token → Bool) → SYState () → SYState ()
whileNotEmptyAnd predicate m = go
  where
    go = do
      b1 ← notEmpty
      when b1 $ do
        b2 ← predicate <$> top
        when b2 (m >> go) -- TODO

output ∷ Token → SYState ()
output t = modify (builder t <$>) -- TODO: exploiting the Functor instance for pairs, which processes the second component of a pair (an Output in this case)
  where
    builder "+" (e1 : e2 : es) = Add e1 e2 : es
    builder "*" (e1 : e2 : es) = Mult e1 e2 : es
    builder n es = Lit (read n) : es -- let it crash on not a number

isOp ∷ String → Bool
isOp "+" = True
isOp "*" = True
isOp _ = False

precedence ∷ String → Int
precedence "*" = 2
precedence "+" = 1
precedence _ = 0

precGTE ∷ String → String → Bool
t1 `precGTE` t2 = precedence t1 >= precedence t2

convertToExpr ∷ String → Expr Integer
convertToExpr str = head $ snd $ execState shuntingYard ([], [])
  where
    tokenize = groupBy (\a b → isDigit a && isDigit b) . filter (not . isSpace)

    tokens = reverse $ tokenize str

    shuntingYard = traverse_ processToken tokens >> transferRest

    transfer = pop >>= output

    transferWhile predicate = whileNotEmptyAnd predicate transfer

    transferRest = transferWhile $ const True

    processToken ")" = push ")"
    processToken "(" = transferWhile (/= ")") >> pop_
    processToken t
      | isOp t = transferWhile (`precGTE` t) >> push t
      | otherwise = output t -- number
