import Control.Monad.RWS (MonadReader (ask), MonadState (get, put, state), MonadWriter (tell), RWS, evalRWS, replicateM)
import System.Random (StdGen, newStdGen, uniformR)

type Dice = Int

type From = Int

type To = Int

type DiceGame = RWS (From, To) [Dice] StdGen

dice ∷ DiceGame Dice
dice = do
  ft ← ask -- ft = (from, to)
  stdGen ← get
  let (dice, newStdGen) = uniformR ft stdGen
  put newStdGen
  tell [dice]
  pure dice

dice' ∷ DiceGame Dice
dice' = do
  ft ← ask
  dice ← state $ uniformR ft -- using the State `constructor`; no need for explicit get's and put's
  tell [dice]
  pure dice

dice'' ∷ DiceGame Dice
dice'' = ask >>= state . uniformR >>= \dice → tell [dice] >> pure dice

doubleDice ∷ DiceGame (Dice, Dice)
doubleDice = (,) <$> dice <*> dice

dices ∷ Int → DiceGame [Dice]
dices n = replicateM n dice

-- 26 dices in Writer's log: 1 + 1 + 1 + 5 + (2 * 3) + 10 + 2
diceGame ∷ DiceGame (Dice, Dice)
diceGame = dice >> dice' >> dice'' >> dices 5 >> replicateM 2 (dices 3) >> dices 10 >> doubleDice

main ∷ IO ()
main = do
  stdGen ← newStdGen
  let (a, log) = evalRWS diceGame (1, 6) stdGen
  print a ------------------------------------------------ eg. (2,6)
  print log ---------------------------------------------- eg. [4,6,5,3,6,4,5,6,4,4,3,4,3,1,4,4,3,3,1,5,5,4,1,4,2,6]
  print $ length log ------------------------------------- 26
  newStdGen >>= print . evalRWS diceGame (1, 10) --------- eg. ((3,10),[10,4,4,4,2,8,3,4,4,9,6,10,9,6,5,3,9,7,10,3,3,4,5,1,3,10])
