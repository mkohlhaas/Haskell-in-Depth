{-# LANGUAGE UnicodeSyntax #-}

import Data.Deque as D (empty, front, popFront, pushFront)
import Data.List (foldl', unfoldr)
import Data.Stack as S (empty, pop, push, top)
import System.TimeIt (timeItNamed)

sumAll ∷ Num a ⇒ t → (t → Maybe a) → (t → t) → a
sumAll s view remove = sum $ unfoldr iter s
  where
    iter s = view s >>= \x → Just (x, remove s)

fill ∷ (Num a, Enum a) ⇒ a → (a → b → b) → b → b
fill n insert s = foldl' (flip insert) s [1 .. n]

main ∷ IO ()
main = do
  let n = 10 ^ 6
  timeItNamed "Stack" $ print $ sumAll (fill n push S.empty) top pop
  timeItNamed "Deque" $ print $ sumAll (fill n pushFront D.empty) front popFront
