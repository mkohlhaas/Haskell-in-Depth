- Order of Code Examples
  - Chapter 8.1
    - `./lookup/IPTypes.hs`
    - `./lookup/ParseIP.hs`
    - `./lookup/LookupIP.hs`
    - `./lookup/Main.hs`

```
$ cabal -v0 run iplookup -- data/ipranges.txt 5.12.120.250     # 5.12.120.250: True
$ cabal -v0 run iplookup -- data/ipranges.txt 127.10.10.1      # 127.10.10.1: False
$ cabal test iplookup-test                                     # the golden files are the reference files, e.g. 2.out.golden
$ cabal run iplookup-simulation -- +RTS -s                     # statistics
$ cabal run iplookup-simulation --enable-profiling -- +RTS -P  # create report
$ cabal run iplookup-simulation --enable-profiling -- +RTS -h  # heap allocation
$ hp2pretty iplookup-simulation.hp
```
