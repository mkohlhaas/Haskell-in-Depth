#### See the many arguments we can give to the default main function provided by Criterion
```
$ cabal bench primcheck --benchmark-options="--help"
```

#### Graphical output
```
$ cabal bench primcheck --benchmark-options="--output isprime.html"
```

#### Run only rewritten benchmark
```
$ cabal bench primcheck --benchmark-options="-m pattern rewritten"
```
