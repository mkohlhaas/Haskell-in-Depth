- Order of Code Examples
  - ./apps/radar/radar.hs, ./libs/radar/Radar.hs
  - ./tests/radar/Test.hs
  - ./apps/person/, ./libs/person/Person.hs
  - ../expr/libs/Expr.hs
  - ./libs/contexts/Contexts.hs

``` haskell
cabal run person-implemented
cabal run person-derived
cabal run person-text
cabal run radar
cabal run radar -- -r ../data/turns.txt North
cabal run radar -- -o ../data/dirs.txt
cabal test radar-test
```

``` haskell
cabal repl radar-test
ghci> writeRandomFile 10 randomDirections "dirs.txt"
ghci> writeRandomFile 10 randomTurns "turns.txt"
```

``` haskell
cabal repl
ghci > sumN 5
ghci > runWriter $ sumN 5
ghci > cartesianProduct [1..2] [1..3]
ghci > traverse addNumber [1..3]
```
