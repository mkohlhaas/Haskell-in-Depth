- Order of Code Examples
  - `app/maybe.hs` (Maybe monad)
  - `app/reader.hs` (Reader monad)
  - `app/genSQL.hs` (Writer monad)
  - `app/gcd.hs` (Writer monad)
  - `app/sumlist.hs` (State monad)
  - `app/weapons.hs` (State monad)
  - `../expr/libs/ShuntingYard.hs` (State monad)
  - `app/dicegame.hs` (RWS monad)
  - `app/ioref.hs` (mutable references in IO monad)
  - `app/filecount.hs` (mutable references in IO monad)
  - `app/stref.hs` (mutable references in ST monad; ST = Strict State Threads)
  - `app/countzeros.hs` (mutable references in ST monad)

Writer Monad
```
$ cabal repl gcd
ghci> import Control.Monad.Writer
ghci> gcdPrint 27 36                              -- (27,36) (36,27) (27,9) (9,0) 9
ghci> execWriter $ gcdLogSteps 27 36              -- [(27,36),(36,27),(27,9),(9,0)]
ghci> runWriter $ gcdLogSteps 27 36               -- (9,[(27,36),(36,27),(27,9),(9,0)])
ghci> getSum $ execWriter $ gcdCountSteps 27 36   -- 4
```

IORef
```
$ cabal -v0 run filecount -- ..
$ find .. -type f | wc
$ cabal -v0 run filecount -- .. . ../.. ~/Temp/
```
