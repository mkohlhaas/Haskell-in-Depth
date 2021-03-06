import Control.Monad.State (MonadState (state), State, evalState, replicateM)
import Data.List (group, sort)
import System.Random (StdGen, Uniform, UniformRange, newStdGen, uniform)
import System.Random.Stateful (uniformM, uniformRM)

data Weapon = Rock | Paper | Scissors
  deriving (Show, Bounded, Enum, Eq)

data Winner = First | Second | Draw
  deriving (Show, Eq, Ord)

winner :: (Weapon, Weapon) -> Winner
winner (Paper, Rock) = First
winner (Scissors, Paper) = First
winner (Rock, Scissors) = First
winner (w1, w2)
  | w1 == w2 = Draw
  | otherwise = Second

instance UniformRange Weapon where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
    pure $ toEnum res

instance Uniform Weapon where
  uniformM rng = uniformRM (minBound, maxBound) rng

type StdGenS = State StdGen

randomWeapon :: StdGenS Weapon
randomWeapon = state uniform

gameRound :: StdGenS (Weapon, Weapon)
gameRound = (,) <$> randomWeapon <*> randomWeapon

game :: Int -> StdGenS [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound)
  where
    counts xs = map headLength $ group $ sort xs
    headLength xs@(x : _) = (x, length xs)
    headLength [] = error "unexpected"

main :: IO ()
main = do
  g <- newStdGen
  let r = evalState (game 10) g
  print r
