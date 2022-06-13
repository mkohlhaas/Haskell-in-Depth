Writer Monad
```
$ cabal repl gcd
ghci> import Control.Monad.Writer
ghci> gcdPrint 27 36
ghci> execWriter $ gcdLogSteps 27 36
ghci> runWriter $ gcdLogSteps 27 36
ghci> getSum $ execWriter $ gcdCountSteps 27 36
```
