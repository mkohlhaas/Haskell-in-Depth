import NumUtils (NumModifier (..))

processInts ∷ NumModifier → [Int] → [Int]
processInts nm = map $ run nm

main ∷ IO ()
-- (+) ∷ ∀ a. Num a ⇒ a → a → a
main = print $ processInts (NumModifier (+ 1)) [1, 2, 3] -- [2, 3, 4]
