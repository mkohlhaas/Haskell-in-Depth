{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Monad (replicateM, unless)
import Data.List (nub, sort)
import Radar (Direction(..), Turn (..), every, orient, orientMany, rotateMany, rotateMany', rotateManySteps)
import System.Exit (exitFailure)
import System.Random (Uniform, UniformRange, getStdRandom, uniform)
import System.Random.Stateful (StatefulGen, uniformM, uniformRM)

instance UniformRange Turn where
  uniformRM ∷ StatefulGen g m ⇒ (Turn, Turn) → g → m Turn
  uniformRM (lo, hi) rng = toEnum <$> uniformRM (fromEnum lo, fromEnum hi) rng

instance Uniform Turn where
  uniformM ∷ StatefulGen g m ⇒ g → m Turn
  uniformM = uniformRM (minBound, maxBound)

instance UniformRange Direction where
  uniformRM ∷ StatefulGen g m ⇒ (Direction, Direction) → g → m Direction
  uniformRM (lo, hi) rng = toEnum <$> uniformRM (fromEnum lo, fromEnum hi) rng

instance Uniform Direction where
  uniformM ∷ StatefulGen g m ⇒ g → m Direction
  uniformM = uniformRM (minBound, maxBound)

uniformIO ∷ Uniform a ⇒ IO a
uniformIO = getStdRandom uniform

-- >>> random ← uniformIO ∷ IO Turn
-- >>> random
-- TNone

-- >>> random ← uniformIO ∷ IO Direction
-- >>> random
-- South

uniformsIO ∷ Uniform a ⇒ Int → IO [a]
uniformsIO = flip replicateM uniformIO

randomTurns ∷ Int → IO [Turn]
randomTurns = uniformsIO

-- >>> turns ← randomTurns 20
-- >>> turns
-- [TRight,TNone,TAround,TRight,TRight,TAround,TAround,TRight,TAround,TRight,TLeft,TNone,TRight,TRight,TRight,TLeft,TAround,TLeft,TRight,TNone]

randomDirections ∷ Int → IO [Direction]
randomDirections = uniformsIO

-- >>> directions ← randomDirections 20
-- >>> directions
-- [East,West,East,South,South,North,South,South,East,West,North,West,West,East,South,South,South,West,East,North]

writeRandomFile ∷ (Uniform a, Show a) ⇒ (Int → IO [a]) → FilePath → Int → IO ()
writeRandomFile gen fname n = gen n >>= writeFile fname . unlines . map show

-- do notation:
-- writeRandomFile gen fname n = do
--   xs ← gen n
--   writeFile fname $ unlines $ map show xs

writeRandomTurns ∷ Int → IO ()
writeRandomTurns = writeRandomFile randomTurns "turns.txt"

-- >>> writeRandomTurns 17

writeRandomDirections ∷ Int → IO ()
writeRandomDirections = writeRandomFile randomDirections "dirs.txt"

-- >>> writeRandomDirections 17

deriving instance Ord Turn

test_allTurnsInUse ∷ Bool
test_allTurnsInUse = sort (nub [orient d1 d2 | d1 ← every, d2 ← every]) == every

-- >>> test_allTurnsInUse
-- True

test_rotationsMonoidAgree ∷ [Turn] → Bool
test_rotationsMonoidAgree ts = and [rotateMany d ts == rotateMany' d ts | d ← every]

-- >>> test_rotationsMonoidAgree [TRight, TAround, TLeft]
-- True

test_orientRotateAgree ∷ [Direction] → Bool
test_orientRotateAgree [] = True
test_orientRotateAgree ds@(d : _) = ds == rotateManySteps d (orientMany ds)

-- >>> test_orientRotateAgree [North, East, South, West]
-- True

main ∷ IO ()
main = do
  ds ← randomDirections 1000
  ts ← randomTurns 1000
  unless (test_allTurnsInUse && test_orientRotateAgree ds && test_rotationsMonoidAgree ts) exitFailure
