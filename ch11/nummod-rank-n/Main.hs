{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fprint-explicit-foralls #-}

import NumUtils (NumModifier (..))

processInts ∷ NumModifier → [Int] → [Int]
processInts nm = map $ run nm

-- >>> :type (+)
-- (+) ∷ ∀ {a}. Num a ⇒ a → a → a
--
-- (+) has EXACTLY the type necessary for `processInts`
--
-- >>> processInts (NumModifier (+ 1)) [1, 2, 3]
-- [2,3,4]

---------------------------
-- Example from the Book --
---------------------------

incG ∷ Num a ⇒ a → a
incG = (+ 1)

incI ∷ Int → Int
incI = (+ 1)

processInts' ∷ (Int → Int) → [Int] → [Int]
processInts' = map

-- Generic incG works, too!
--
-- >>> processInts' incG [1..4]
-- [2,3,4,5]
--
-- >>> processInts' incI [1..4]
-- [2,3,4,5]

-- Both inc functions work. But what we actually want is a processInts function that only takes functions like incG.

-- No compiler error. But we want to work on [Int] not [a] and it allows again both inc functions.
processInts'' ∷ Num a ⇒ (a → a) → [a] → [a]
processInts'' = map

-- >>> processInts'' incG [1..4]
-- [2,3,4,5]
--
-- >>> processInts'' incI [1..4]
-- [2,3,4,5]

-- Compiler Error: Couldn't match type ‘a’ with ‘Int’.
-- processInts''' ∷ Num a ⇒ (a → a) → [Int] → [Int]
-- processInts''' = map

-- Solution: RankNTypes!
processInts'''' ∷ (∀ a. Num a ⇒ a → a) → [Int] → [Int]
processInts'''' = map

-- Now only the generic inc function works (incG) as demanded.
-- The only argument to processInts'''' allowed is any function that uses `Num a`.
--
-- >>> processInts'''' incG [1..4]
-- [2,3,4,5]
--
-- >>> processInts'''' incI [1..4]
-- Couldn't match type ‘a’ with ‘Int’
-- ‘a’ is a rigid type variable bound by
--   a type expected by the context:
--     ∀ a. Num a ⇒ a → a
--   at /home/schmidh/Gitrepos/Haskell-in-Depth/ch11/nummod-rank-n/Main.hs:56:18-21
-- Expected type: a → a
--   Actual type: Int → Int

-- If some function takes a rank-2 polymorphic function as an argument, then it's counted as rank-3 polymorphic, and so on.
--
-- (Int → Int) → [Int] → [Int]                      Rank 0, nonpolymorphic function
-- ∀ a. Num a ⇒ a → a → [Int] → [Int]               Rank 1
-- (∀ a. Num a ⇒ a → a) → [Int] → [Int]             Rank 2
-- ((∀ a.Num a ⇒ a → Int) → Int) → [Int] → [Int]    Rank 3

--------------
-- ST Trick --
--------------

-- newSTRef ∷ a → ST s (STRef s a)
-- runST ∷ (∀ s. ST s a) → a

-- Just filling in the types for running an ST:
-- runST (newSTRef True) :: (∀ s. ST s (STRef s Bool)) → STRef s Bool
--                             |                               |
--              type variable `s` is introduced and scoped     |
--                                                             |
--                                                 later `s` is referenced
--                                         At this point the type no longer exists!

main ∷ IO ()
main = print $ processInts (NumModifier (+ 1)) [1, 2, 3]
