import Control.Monad.Writer (MonadWriter (tell), Sum (Sum), Writer, mapWriter)

gcd' ∷ Integral a ⇒ a → a → a
gcd' x 0 = x
gcd' x y = gcd y (x `mod` y)

-- turn a pure computation into a monadic one ⇒ you can use Writer monad down the line
gcdM ∷ (Integral a, Monad m) ⇒ (a → a → m ()) → a → a → m a
gcdM step x 0 = step x 0 >> pure x
gcdM step x y = step x y >> gcdM step y (x `mod` y)

gcdPrint ∷ (Show a, Integral a) ⇒ a → a → IO a
gcdPrint = gcdM (curry print)

-- gcdPrint = gcdM (\a b → print (a, b))
-- `print` prints a tuple

gcdCountSteps ∷ Integral a ⇒ a → a → Writer (Sum Int) a
gcdCountSteps = gcdM (\_ _ → tell $ Sum 1)

gcdLogSteps ∷ Integral a ⇒ a → a → Writer [(a, a)] a
gcdLogSteps = gcdM (\a b → tell [(a, b)])

gcdCountSteps' ∷ Integral a ⇒ a → a → Writer (Sum Int) a
gcdCountSteps' a b = mapWriter mapper $ gcdLogSteps a b
  where
    mapper (v, w) = (v, Sum $ length w)

-- https://wiki.haskell.org/Pointfree
-- f = (g.) . h ⇔ f x x0 = g (h x x0)
gcdCountSteps'' ∷ Integral a ⇒ a → a → Writer (Sum Int) a
gcdCountSteps'' = (mapWriter (Sum . length <$>) .) . gcdLogSteps

main ∷ IO ()
main = print "OK"

-- in GHCi
-- ghci> gcdPrint 6 18
