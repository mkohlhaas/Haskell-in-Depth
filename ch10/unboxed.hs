{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

sumProd ∷ Num a ⇒ a → a → (# a, a #)
sumProd a b = (# a + b, a*b #)

test_sumProd ∷ IO ()
test_sumProd = case sumProd (5 ∷ Int) 6 of
                  (# s, p #) → print (s + p)

fibNext ∷ Int → (Integer, Integer) → (Integer, Integer)
fibNext 0 p = p
fibNext n (a, b) = fibNext (n-1) (b, a+b)

test_fibNext ∷ IO ()
test_fibNext = case fibNext 100 (0, 1) of
         (a, _) → print a

fibNext' ∷ Int → (# Integer, Integer #) → (# Integer, Integer #)
fibNext' 0 p = p
fibNext' n (# a, b #) = fibNext' (n-1) (# b, a+b #)

test_fibNext' ∷ IO ()
test_fibNext' = case fibNext' 100 (# 0, 1 #) of
         (# a, _ #) → print a

type Coordinates = (# Double | (# Double, Double #) #)

smallest ∷ Coordinates → Double
smallest (# d | #) = d
smallest (# | (# x, y #) #) = min x y

test_smallest ∷ IO ()
test_smallest = do
  print $ smallest (# 5.4 | #)
  print $ smallest (# | (# 2.3, 1.2 #) #)

main ∷ IO ()
main = do
  test_fibNext
  test_fibNext'
  test_sumProd
  test_smallest
