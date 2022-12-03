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

----------------------
-- Stack Operations --
----------------------

isEmpty ∷ SYState Bool
isEmpty = gets (null . fst)

notEmpty ∷ SYState Bool
notEmpty = not <$> isEmpty

top ∷ SYState Token
top = gets (head . fst) -- let it crash on empty stack

pop ∷ SYState Token
pop = do
  (stack, output) ← get
  put (tail stack, output) -- let it crash on empty stack
  pure $ head stack

-- let it crash on empty stack
pop_ ∷ SYState ()
pop_ = modify (first tail)

push ∷ Token → SYState ()
push = modify . first . (:)

-- TODO: use `whileM` and `andM` (or similar) from the monad-loops package
-- m is a computation in the State monad
-- whileNotEmptyAnd a certain condition holds, do `m`
whileNotEmptyAnd ∷ (Token → Bool) → SYState () → SYState ()
whileNotEmptyAnd predicate m = go
  where
    go ∷ State (Stack, Output) ()
    go = do
      b1 ← notEmpty
      when b1 $ do
        b2 ← predicate <$> top
        when b2 (m >> go) -- execute monadic action (transfer token to output as defined later)

------------
-- Output --
------------

-- while moving tokens from Stack to Output transform Token's into Expr's
output ∷ Token → SYState ()
output t = modify (builder t <$>) -- exploiting the Functor instance for pairs, processing the second component (= `Output`).
  where
    builder ∷ (Read a) ⇒ String → [Expr a] → [Expr a]
    builder "+" (e1 : e2 : es) = Add e1 e2 : es
    builder "*" (e1 : e2 : es) = Mult e1 e2 : es
    builder a es = Lit (read a) : es -- let it crash on not-a-number

-------------
-- Helpers --
-------------

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

---------------------
-- Central routine --
---------------------

convertToExpr ∷ String → Expr Integer
convertToExpr str = head . snd $ execState shuntingYard ([], [])
  where
    tokenize ∷ String → [String]
    tokenize = groupBy (\a b → isDigit a && isDigit b) . filter (not . isSpace) -- find numbers

    tokens ∷ [String]
    tokens = tokenize str & reverse

    shuntingYard ∷ State (Stack, Output) ()
    shuntingYard = traverse_ processToken tokens >> transferRest

    transferToOutput ∷ State (Stack, Output) ()
    transferToOutput = pop >>= output

    transferWhile ∷ (Token → Bool) → SYState ()
    transferWhile predicate = whileNotEmptyAnd predicate transferToOutput

    transferRest ∷ SYState ()
    transferRest = transferWhile (const True)

    processToken ∷ String → SYState ()
    processToken ")" = push ")" -- we reversed the original list: '(' and ')' come in reverse order. See doc test for `tokens` below.
    processToken "(" = transferWhile (/= ")") >> pop_
    processToken t
      | isOp t = transferWhile (\t' → (t' `precGTE` t) && (t' /= ")")) >> push t
      | otherwise = output t -- token t is a number and goes straight to Output

-- >>> convertToExpr "(22+33*43)*55"
-- Mult (Add (Lit 22) (Mult (Lit 33) (Lit 43))) (Lit 55)

tokenize ∷ String → [String]
tokenize = groupBy (\a b → isDigit a && isDigit b) . filter (not . isSpace) -- find numbers

-- >>> tokenize "(22+33*43)*55"
-- ["(","22","+","33","*","43",")","*","55"]

tokens ∷ String → [String]
tokens str = tokenize str & reverse

-- >>> tokens "(22 + 33 * 43) * 55"
-- ["55","*",")","43","*","33","+","22","("]

