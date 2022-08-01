{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Monad (replicateM, unless)
import Data.List (nub, sort)
import Radar (Direction, Turn (..), every, orient, orientMany, rotateMany, rotateMany', rotateManySteps)
import System.Exit (exitFailure)
import System.Random (Uniform, UniformRange, getStdRandom, uniform)
import System.Random.Stateful (uniformM, uniformRM)

instance UniformRange Turn where
  uniformRM (lo, hi) rng = toEnum <$> uniformRM (fromEnum lo, fromEnum hi) rng

instance UniformRange Direction where
  uniformRM (lo, hi) rng = toEnum <$> uniformRM (fromEnum lo, fromEnum hi) rng

instance Uniform Turn where
  uniformM rng = uniformRM (minBound, maxBound) rng

instance Uniform Direction where
  uniformM rng = uniformRM (minBound, maxBound) rng

uniformIO ∷ Uniform a ⇒ IO a
uniformIO = getStdRandom uniform

uniformsIO ∷ Uniform a ⇒ Int → IO [a]
uniformsIO n = replicateM n uniformIO

randomTurns ∷ Int → IO [Turn]
randomTurns = uniformsIO

randomDirections ∷ Int → IO [Direction]
randomDirections = uniformsIO

writeRandomFile ∷ (Uniform a, Show a) ⇒ (Int → IO [a]) → FilePath → Int → IO ()
writeRandomFile gen fname n = do
  xs ← gen n
  writeFile fname $ unlines $ map show xs

writeRandomTurns ∷ Int → IO ()
writeRandomTurns = writeRandomFile randomTurns "turns.txt"

writeRandomDirections ∷ Int → IO ()
writeRandomDirections = writeRandomFile randomDirections "dirs.txt"

deriving instance Ord Turn

test_allTurnsInUse ∷ Bool
test_allTurnsInUse = sort (nub [orient d1 d2 | d1 ← every, d2 ← every]) == every

test_rotationsMonoidAgree ∷ [Turn] → Bool
test_rotationsMonoidAgree ts = and [rotateMany d ts == rotateMany' d ts | d ← every]

test_orientRotateAgree ∷ [Direction] → Bool
test_orientRotateAgree [] = True
test_orientRotateAgree ds@(d : _) = ds == rotateManySteps d (orientMany ds)

main ∷ IO ()
main = do
  ds ← randomDirections 1000
  ts ← randomTurns 1000
  unless (test_allTurnsInUse && test_orientRotateAgree ds && test_rotationsMonoidAgree ts) exitFailure
