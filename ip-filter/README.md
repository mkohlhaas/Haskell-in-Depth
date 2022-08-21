- Order of Code Examples
  - Chapter 8.1
    - `./lookup/IPTypes.hs`
    - `./lookup/ParseIP.hs`
    - `./lookup/LookupIP.hs`
    - `./lookup/Main.hs`
  - Chapter 8.2
    - `./tests/iplookup/Test.hs`
    - `./tests/iplookup/ParseIPSpec.hs` (hspec)
    - `./tests/iplookup/LookupIPSpec.hs` (hspec)
    - `./tests/iplookup/GoldenTests.hs` (golden)
    - `./tests/iplookup/Props.hs` (hedgehog)
    - `./gen/Main.hs` (hedgehog)
    - `./gen/GenIP.hs` (hedgehog)

- Testing Libraries Used in This Chapter:
  - [`hedgehog` implements property-based testing](https://hackage.haskell.org/package/hedgehog)
  - [`hspec` implements specification-based testing](https://hackage.haskell.org/package/hspec)
  - [A `golden test` is an IO action that writes its result to a file. To pass the test, this output file should be identical to the corresponding `golden` file](https://hackage.haskell.org/package/tasty-golden)
  - [`tasty` lets you combine your unit tests, golden tests, QuickCheck/SmallCheck properties,...](https://hackage.haskell.org/package/tasty)
  - [`doctest`](https://github.com/sol/doctest#readme)
```
$ cabal -v0 run iplookup -- data/ipranges.txt 5.12.120.250     # 5.12.120.250: True
$ cabal -v0 run iplookup -- data/ipranges.txt 127.10.10.1      # 127.10.10.1: False
$ cabal test iplookup-test                                     # the golden files are the reference files, e.g. 2.out.golden
$ cabal run ipgen -- 200 data/genips.txt                       # uses hedgehog
$ cabal run iplookup-simulation -- +RTS -s                     # statistics
$ cabal run iplookup-simulation --enable-profiling -- +RTS -P  # create report
$ cabal run iplookup-simulation --enable-profiling -- +RTS -h  # heap allocation
$ hp2pretty iplookup-simulation.hp
```
