- Order of Code Examples
  - Chapter 8.1
    - `./lookup/IPTypes.hs`
    - `./lookup/ParseIP.hs`
    - `./lookup/LookupIP.hs`
    - `./lookup/Main.hs`
  - Chapter 8.2
    - `./tests/iplookup/ParseIPSpec.hs`
    - `./tests/iplookup/LookupIPSpec.hs`
    - `./tests/iplookup/GoldenTests.hs`
    - `./tests/iplookup/Test.hs`

- Testing Libraries Used in This Chapter:
  - [`doctest` is a tool that checks examples and properties in Haddock comments](https://hackage.haskell.org/package/doctest)
  - [`hedgehog` automatically generates a comprehensive array of test cases, exercising your software in ways human testers would never imagine](https://hackage.haskell.org/package/hedgehog)
  - [`hspec`](https://hackage.haskell.org/package/hspec)
  - [`tasty` lets you combine your unit tests, golden tests, QuickCheck/SmallCheck properties,...](https://hackage.haskell.org/package/tasty)

```
$ cabal -v0 run iplookup -- data/ipranges.txt 5.12.120.250     # 5.12.120.250: True
$ cabal -v0 run iplookup -- data/ipranges.txt 127.10.10.1      # 127.10.10.1: False
$ cabal test iplookup-test                                     # the golden files are the reference files, e.g. 2.out.golden
$ cabal run iplookup-simulation -- +RTS -s                     # statistics
$ cabal run iplookup-simulation --enable-profiling -- +RTS -P  # create report
$ cabal run iplookup-simulation --enable-profiling -- +RTS -h  # heap allocation
$ hp2pretty iplookup-simulation.hp
```
