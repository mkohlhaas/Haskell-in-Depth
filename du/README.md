- Order of Code Examples
  - `./app/App.hs`
  - `./app/AppTypes.hs`
  - `./app/AppRWST.hs`
  - `./app/AppRTWTST.hs`
  - `./app/Utils.hs`
  - `./app/DirTree.hs`
  - `./app/FileCounter.hs`
  - `./app/DiskUsage.hs`
  - `./app/Main.hs`

#### Disk Usage
```
$ cabal -v0 run du
$ cabal -v0 run du -- --help
$ cabal -v0 run du -- ~/Temp/
$ cabal -v0 run du -- ../data -d1 -e txt
$ cabal -v0 run du -- ../data -d1
```
