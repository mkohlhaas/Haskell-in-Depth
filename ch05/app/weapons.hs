{-# LANGUAGE InstanceSigs #-}

import Control.Category
import Control.Monad.State (MonadState (state), State, evalState, replicateM)
import Data.Function ((&))
import Data.List (group, sort)
import System.Random (StdGen, Uniform, UniformRange, newStdGen, uniform)
import System.Random.Stateful (StatefulGen, uniformM, uniformRM)

data Weapon = Rock | Paper | Scissors
  deriving (Show, Bounded, Enum, Eq)

data Winner = PlayerOne | PlayerTwo | Draw
  deriving (Show, Eq, Ord)

winner ∷ (Weapon, Weapon) → Winner
winner (Paper, Rock) = PlayerOne
winner (Scissors, Paper) = PlayerOne
winner (Rock, Scissors) = PlayerOne
winner (w1, w2)
  | w1 == w2 = Draw
  | otherwise = PlayerTwo

instance UniformRange Weapon where
  uniformRM ∷ StatefulGen g m ⇒ (Weapon, Weapon) → g → m Weapon
  uniformRM (lo, hi) rng = toEnum <$> uniformRM (fromEnum lo, fromEnum hi) rng

instance Uniform Weapon where
  uniformM ∷ StatefulGen g m ⇒ g → m Weapon
  uniformM = uniformRM (minBound, maxBound)

type StateStdGen = State StdGen

-- Create a State monad from `uniform` (which has exactly the demanding type)!!!
randomWeapon ∷ StateStdGen Weapon
randomWeapon = state uniform

-- >>> g ← newStdGen
-- >>> evalState randomWeapon g
-- Paper

-- >>> g ← newStdGen
-- >>> evalState (replicateM 10 randomWeapon) g
-- [Paper,Paper,Paper,Scissors,Scissors,Paper,Paper,Rock,Paper,Rock]

gameRound ∷ StateStdGen (Weapon, Weapon)
gameRound = (,) <$> randomWeapon <*> randomWeapon

-- >>> g ← newStdGen
-- >>> evalState (replicateM 5 gameRound) g
-- [(Paper,Paper),(Scissors,Scissors),(Paper,Paper),(Scissors,Rock),(Paper,Rock)]

-- >>> g ← newStdGen
-- >>> winner <$> evalState (replicateM 10 gameRound) g
-- [PlayerTwo,PlayerTwo,PlayerTwo,PlayerTwo,Draw,Draw,PlayerOne,Draw,Draw,PlayerOne]

-- >>> g ← newStdGen
-- >>> winner <$> evalState (replicateM 10 gameRound) g & sort & group
-- [[PlayerOne],[PlayerTwo,PlayerTwo,PlayerTwo,PlayerTwo],[Draw,Draw,Draw,Draw,Draw]]

game ∷ Int → StateStdGen [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound)
  where
    counts xs = xs & sort & group & map headLength
    headLength xs@(x : _) = (x, length xs)
    headLength [] = error "unexpected"

-- >>> g ← newStdGen
-- >>> evalState (game 100) g
-- [(PlayerOne,31),(PlayerTwo,32),(Draw,37)]

main ∷ IO ()
main = do
  g ← newStdGen -------------------- get a StdGen!!!
  print $ evalState (game 100) g --- e.g. [(PlayerOne,35),(PlayerTwo,33),(Draw,32)]
