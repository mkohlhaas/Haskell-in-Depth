- Order of Code Examples
  - `benchmarks/primcheck.hs`
  - `../ip-filter/benchmarks/`

```
$ cd ../ip-filter
$ cabal bench iplookup-bench
```

```
# Run benchmark
$ cabal bench primcheck
```

```
# See the many arguments we can give to the default main function provided by Criterion
$ cabal bench primcheck --benchmark-options="--help"
```

```
# Graphical output
$ cabal bench primcheck --benchmark-options="--output isprime.html"
```

```
# Run only `rewritten` benchmark
$ cabal bench primcheck --benchmark-options="-m pattern rewritten"
```
