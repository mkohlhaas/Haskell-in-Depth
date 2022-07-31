- Order of Code Examples
  - `./apps/radar/radar.hs`, `./libs/radar/Radar.hs`
  - `./tests/radar/Test.hs`
  - `./apps/person/`, `./libs/person/Person.hs`
  - `../expr/libs/Expr.hs`
  - `./libs/contexts/Contexts.hs`

- List of imported functions in chapter 2:
  - TextShow: `showb`, `fromString`, `printT`
  - Fmt: `fmt`, `fmtLn`, `nameF`, `unwordsF`, `+||`, `||+`
  - System.Environment: `getArgs`, `getProgName`
  - Control.Monad.Writer: `tell`, `writer`
  - Data.String: `IsString`
  - Control.Monad: `replicateM`, `unless`, `when`
  - Data.List: `nub`, `sort`
  - System.Exit: `exitFailure`
  - System.Random: `getStdRandom`, `uniform`
  - System.Random.Stateful: `uniformM`, `uniformRM`

- List of used type classes in chapter 2:
  - [ `Bounded` ](https://hackage.haskell.org/package/base-4.16.3.0/docs/Prelude.html#t:Bounded)
  - [ `Buildable` ](https://hackage.haskell.org/package/formatting-7.1.3/docs/Formatting-Buildable.html)
  - [ `Enum` ](https://hackage.haskell.org/package/base-4.16.3.0/docs/GHC-Enum.html#t:Enum)
  - [ `Eq` ](https://hackage.haskell.org/package/ghc-prim-0.8.0/docs/GHC-Classes.html#t:Eq)
  - [ `IsString` ](https://hackage.haskell.org/package/base-4.16.3.0/docs/Data-String.html#t:IsString)
  - [ `Monoid` ](https://hackage.haskell.org/package/base-4.16.3.0/docs/Prelude.html#t:Monoid)
  - [ `Ord` ](https://hackage.haskell.org/package/ghc-prim-0.8.0/docs/GHC-Classes.html#t:Ord)
  - [ `Read` ](https://hackage.haskell.org/package/base-4.16.3.0/docs/GHC-Read.html)
  - [ `Semigroup` ](https://hackage.haskell.org/package/base-4.16.3.0/docs/Prelude.html#t:Semigroup)
  - [ `Show` ](https://hackage.haskell.org/package/ghc-9.2.4/docs/GHC-Prelude.html#t:Show)
  - [ `TextShow` ](https://hackage.haskell.org/package/text-show-3.9.7/docs/TextShow.html#t:TextShow)
  - [ `Uniform` ](https://hackage.haskell.org/package/random-1.2.1.1/docs/System-Random.html#t:Uniform)
  - [ `UniformRange` ](https://hackage.haskell.org/package/random-1.2.1.1/docs/System-Random.html#t:UniformRange)

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
