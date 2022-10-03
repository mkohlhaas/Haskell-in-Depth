import Criterion.Main (bench, defaultMain, whnf)
import qualified IsPrime as IP
import qualified IsPrimeUnfolded as IPU

isPrime ∷ Integer → Bool
isPrime n = all notDividedBy [2 .. n `div` 2]
  where
    notDividedBy m = n `mod` m /= 0

primeNumber ∷ Integer
primeNumber = 16183

main ∷ IO ()
main =
  defaultMain
    [ bench "isPrime (declarative)" $ whnf IP.isPrime primeNumber, -- whnf = weak head normal form
      bench "isPrime (unfolded)" $ whnf IPU.isPrime primeNumber,
      bench "isPrime (rewritten)" $ whnf isPrime primeNumber
    ]
