- Order of Code Examples
  - `../du/app/AppTypes.hs`
  - `../du/app/AppRWST.hs`
  - `../du/app/Utils.hs`
  - `../du/app/DirTree.hs`
  - `../du/app/Main.hs`
  - `../du/app/FileCounter.hs`
  - `../du/app/DiskUsage.hs`
  - `../du/app/Main.hs`
  - `../du/app/AppRTWTST.hs`

#### Disk Usage
```
$ cabal -v0 run du
$ cabal -v0 run du -- --help
$ cabal -v0 run du -- ~/Temp/
$ cabal -v0 run du -- ../data -d1 -e txt
$ cabal -v0 run du -- ../data -d1
```
