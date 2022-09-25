module IsPrimeUnfolded where

isPrime ∷ Integer → Bool
isPrime n = go 2
  where
    go x = (x > n - 1) || ((n `mod` x /= 0) && go (x + 1))
