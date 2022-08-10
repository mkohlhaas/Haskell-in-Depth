{--
  apps/prefix-postfix.hs uses ShuntingYard
  cabal run prefix-postfix
--}

module ShuntingYard (convertToExpr) where

import Control.Monad.State (MonadState (get, put), State, execState, gets, modify, when)
import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (groupBy)
import Expr (Expr (..))

type Token = String

type Stack = [Token]

type Output = [Expr Integer]

type SYState = State (Stack, Output)

isEmpty ∷ SYState Bool
isEmpty = gets (null . fst)

notEmpty ∷ SYState Bool
notEmpty = not <$> isEmpty

top ∷ SYState Token
top = gets (head . fst) -- let it crash on empty stack

pop ∷ SYState Token
pop = do
  (tokens, es) ← get
  put (tail tokens, es) -- let it crash on empty stack
  pure $ head tokens

-- let it crash on empty stack
pop_ ∷ SYState ()
pop_ = modify (first tail)

push ∷ Token → SYState ()
push = modify . first . (:)

-- m is a computation in the State monad
-- whileNotEmptyAnd a certain condition holds, do `m`
whileNotEmptyAnd ∷ (Token → Bool) → SYState () → SYState ()
whileNotEmptyAnd predicate m = go
  where
    go = do
      b1 ← notEmpty
      when b1 $ do
        b2 ← predicate <$> top
        when b2 (m >> go) -- execute monadic action (transfer token to output as defined later)

-- while moving tokens from Stack to Output transform Token's into Expr's
output ∷ Token → SYState ()
output t = modify (builder t <$>) -- exploiting the Functor instance for pairs, which processes the second component of a pair (an Output in this case)
  where
    builder "+" (e1 : e2 : es) = Add e1 e2 : es
    builder "*" (e1 : e2 : es) = Mult e1 e2 : es
    builder n es = Lit (read n) : es -- let it crash on not-a-number

isOp ∷ String → Bool
isOp "+" = True
isOp "*" = True
isOp _ = False

precedence ∷ String → Int
precedence "*" = 2
precedence "+" = 1
precedence _ = 0

-- precedence greater than or equal
precGTE ∷ String → String → Bool
t1 `precGTE` t2 = precedence t1 >= precedence t2

convertToExpr ∷ String → Expr Integer
convertToExpr str = execState shuntingYard ([], []) & snd & head
  where
    tokenize = groupBy (\a b → isDigit a && isDigit b) . filter (not . isSpace) -- find numbers
    tokens = tokenize str & reverse

    shuntingYard = traverse_ processToken tokens >> transferRest

    transferToOutput = pop >>= output

    transferWhile predicate = whileNotEmptyAnd predicate transferToOutput

    transferRest = transferWhile (const True)

    processToken ")" = push ")" -- we reversed the original list: '(' and ')' come in reverse order (see Fig. 5.2, page 153)
    processToken "(" = transferWhile (/= ")") >> pop_
    processToken t
      | isOp t = transferWhile (\t' -> (t' `precGTE` t) && (t' /= ")")) >> push t
      | otherwise = output t -- token t is a number and goes straight to Output
