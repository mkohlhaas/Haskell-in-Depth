- Order of Code Examples
  - `app/maybe.hs` (Maybe monad)
  - `app/reader.hs` (Reader monad)
  - `app/genSQL.hs` (Writer monad)
  - `app/gcd.hs` (Writer monad)
  - `app/sumlist.hs` (State monad)
  - `app/weapons.hs` (State monad)
  - `../expr/libs/ShuntingYard.hs` (State monad)
  - `app/dicegame.hs` (RWS monad)
  - `app/ioref.hs` (mutable references in IO monad)
  - `app/filecount.hs` (mutable references in IO monad)
  - `app/stref.hs` (mutable references in ST monad; ST = Strict State Threads)
  - `app/countzeros.hs` (mutable references in ST monad)

IORef
```
$ cabal -v0 run filecount -- ..
$ find .. -type f | wc
$ cabal -v0 run filecount -- .. . ../.. ~/Temp/
```
