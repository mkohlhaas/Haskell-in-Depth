```shell
$ cabal clean
$ cabal build
$ cabal test queue
$ cabal test stack
$ cabal bench bench
```
TODO:
look again into 'unfoldr' (from Bench.hs)
  - [Anamorphisms aka Unfolds Explained](https://functional.works-hub.com/learn/number-anamorphisms-aka-unfolds-explained-50e1a)
  - [You Can Unfold So Much More with Monoid: Examples](https://functional.works-hub.com/learn/you-can-unfold-so-much-more-with-monoid-examples-766f2)

Setup.hs is needed for distribution, not for building.
