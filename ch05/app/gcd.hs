import Control.Monad.Writer (MonadWriter (tell), Sum (Sum, getSum), Writer, mapWriter, void, execWriter, runWriter)

gcd' ∷ Integral a ⇒ a → a → a
gcd' x 0 = x
gcd' x y = gcd y (x `mod` y)

-- >>> gcd' 27 36
-- 9

-- >>> gcd' 36 27
-- 9

-- turn a pure computation into a monadic one ⇒ you can use Writer monad down the line
gcdM ∷ (Integral a, Monad m) ⇒ (a → a → m ()) → a → a → m a
gcdM stepFn x 0 = stepFn x 0 >> pure x
gcdM stepFn x y = stepFn x y >> gcdM stepFn y (x `mod` y)

gcdPrint ∷ (Show a, Integral a) ⇒ a → a → IO a
gcdPrint = gcdM (curry print) -- `print` prints a tuple

gcdLogSteps ∷ Integral a ⇒ a → a → Writer [(a, a)] a
gcdLogSteps = gcdM (\a b → tell [(a, b)])

gcdCountSteps ∷ Integral a ⇒ a → a → Writer (Sum Int) a
gcdCountSteps = gcdM (\_ _ → tell $ Sum 1)

gcdCountSteps' ∷ Integral a ⇒ a → a → Writer (Sum Int) a
gcdCountSteps' a b = mapWriter mapper $ gcdLogSteps a b
  where
    mapper (v, w) = (v, Sum $ length w)

gcdCountSteps'' ∷ Integral a ⇒ a → a → Writer Int a
gcdCountSteps'' x y = mapWriter (length <$>) (gcdLogSteps x y)

-- pointfree
gcdCountSteps''' ∷ Integral a ⇒ a → a → Writer Int a
gcdCountSteps''' = (mapWriter (length <$>) .) . gcdLogSteps

-- >>> execWriter (gcdLogSteps 27 36)
-- [(27,36),(36,27),(27,9),(9,0)]
--
-- >>> runWriter (gcdLogSteps 27 36)
-- (9,[(27,36),(36,27),(27,9),(9,0)])
--
-- >>> runWriter (gcdCountSteps 27 36)
-- (9,Sum {getSum = 4})
--
-- >>> runWriter (gcdCountSteps' 27 36)
-- (9,Sum {getSum = 4})
--
-- >>> runWriter (gcdCountSteps'' 27 36)
-- (9,4)
--
-- >>> getSum (execWriter $ gcdCountSteps 27 36)
-- 4
--
-- >>> execWriter $ gcdCountSteps'' 27 36
-- 4
--
-- >>> execWriter $ gcdCountSteps''' 27 36
-- 4

main ∷ IO ()
main = do
  void $ gcdPrint 27 36                              -- (27,36) (36,27) (27,9) (9,0)
  print $ execWriter (gcdLogSteps 27 36)             -- [(27,36),(36,27),(27,9),(9,0)]
  print $ runWriter (gcdLogSteps 27 36)              -- (9,[(27,36),(36,27),(27,9),(9,0)])
  print $ runWriter (gcdCountSteps 27 36)            -- (9,Sum {getSum = 4})
  print $ runWriter (gcdCountSteps' 27 36)           -- (9,Sum {getSum = 4})
  print $ runWriter (gcdCountSteps'' 27 36)          -- (9,Sum {getSum = 4})
  print $ getSum (execWriter $ gcdCountSteps 27 36)  -- 4
  print $ execWriter $ gcdCountSteps'' 27 36         -- 4
