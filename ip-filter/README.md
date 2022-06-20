```
$ cabal run iplookup-simulation -- +RTS -s                     # statistics
$ cabal run iplookup-simulation --enable-profiling -- +RTS -P  # create report
$ cabal run iplookup-simulation --enable-profiling -- +RTS -h  # heap allocation
$ hp2pretty iplookup-simulation.hp
```
