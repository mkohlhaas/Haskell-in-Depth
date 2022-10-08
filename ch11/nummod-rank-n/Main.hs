import NumUtils (NumModifier (..))

processInts ∷ NumModifier → [Int] → [Int]
processInts nm = map $ run nm

-- |
-- >>> processInts (NumModifier (+ 1)) [1, 2, 3]
-- [2,3,4]

main ∷ IO ()
-- this is exactly the type we need, no more, no less.
-- (+) ∷ ∀ a. Num a ⇒ a → a → a
main = print $ processInts (NumModifier (+ 1)) [1, 2, 3] -- [2, 3, 4]
