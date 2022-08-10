import Control.Monad.State (MonadState (state), State, evalState, replicateM)
import Data.Function ((&))
import Data.List (group, sort)
import System.Random (StdGen, Uniform, UniformRange, newStdGen, uniform)
import System.Random.Stateful (uniformM, uniformRM)

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
  uniformRM (lo, hi) rng = toEnum <$> uniformRM (fromEnum lo, fromEnum hi) rng

instance Uniform Weapon where
  uniformM = uniformRM (minBound, maxBound)

-- StdGen is the state!!!
type StdGenS = State StdGen

-- Create a State monad from `uniform` (which has exactly the demanding type)!!!
randomWeapon ∷ StdGenS Weapon
randomWeapon = state uniform

gameRound ∷ StdGenS (Weapon, Weapon)
gameRound = (,) <$> randomWeapon <*> randomWeapon

game ∷ Int → StdGenS [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound)
  where
    counts xs = xs & sort & group & map headLength
    headLength xs@(x : _) = (x, length xs)
    headLength [] = error "unexpected"

main ∷ IO ()
main = do
  g ← newStdGen -------------------- get a StdGen!!!
  print $ evalState (game 100) g --- e.g. [(PlayerOne,35),(PlayerTwo,33),(Draw,32)]
