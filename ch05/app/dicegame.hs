import Control.Monad.RWS (MonadReader (ask), MonadState (get, put, state), MonadWriter (tell), RWS, evalRWS, replicateM)
import System.Random (StdGen, newStdGen, uniformR)

type Dice = Int

type From = Int

type To = Int

type DiceGame = RWS (From, To) [Dice] StdGen

dice ∷ DiceGame Dice
dice = do
  bs ← ask
  g ← get
  let (r, g') = uniformR bs g
  put g'
  tell [r]
  pure r

dice' ∷ DiceGame Dice
dice' = do
  bs ← ask
  r ← state $ uniformR bs
  tell [r]
  pure r

dice'' ∷ DiceGame Dice
dice'' = ask >>= state . uniformR >>= \r → tell [r] >> pure r

doubleDice ∷ DiceGame (Dice, Dice)
doubleDice = (,) <$> dice <*> dice

dices ∷ Int → DiceGame [Dice]
dices n = replicateM n dice

-- creates 24 dices in writer's log: 1 + 5 + (2 * 3) + 10 + 2
diceGame ∷ DiceGame (Dice, Dice)
diceGame = dice >> dices 5 >> replicateM 2 (dices 3) >> dices 10 >> doubleDice

main ∷ IO ()
main = newStdGen >>= print . evalRWS diceGame (1, 6)
