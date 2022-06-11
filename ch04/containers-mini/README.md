```shell
$ cabal clean
$ cabal build
$ cabal test queue
$ cabal test stack
$ cabal bench bench
```
TODO:
look again into 'unfoldr' (from Bench.hs)

Setup.hs is needed for distribution, not for building.
