{-# OPTIONS_GHC -fprint-explicit-foralls #-}

import NumUtils (NumModifier (..))

processInts ∷ NumModifier → [Int] → [Int]
processInts nm = map $ run nm

-- >>> :type (+)
-- (+) ∷ ∀ {a}. Num a ⇒ a → a → a

-- >>> processInts (NumModifier (+ 1)) [1, 2, 3]
-- [2,3,4]

main ∷ IO ()
main = print $ processInts (NumModifier (+ 1)) [1, 2, 3]
