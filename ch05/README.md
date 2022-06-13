Writer Monad
```
$ cabal repl gcd
ghci> import Control.Monad.Writer
ghci> gcdPrint 27 36
ghci> execWriter $ gcdLogSteps 27 36
ghci> runWriter $ gcdLogSteps 27 36
ghci> getSum $ execWriter $ gcdCountSteps 27 36
```

IORef
```
$ cabal -v0 run filecount -- ..
$ find .. -type f | wc
$ cabal -v0 run filecount -- .. . ../.. ~/Temp/
```
