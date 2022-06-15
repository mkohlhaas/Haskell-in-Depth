#### Shunting Yard Youtube videos:
- https://www.youtube.com/watch?v=A-SSrZUHYSk
- https://www.youtube.com/watch?v=HJOnJU77EUs

#### Shunting Yard in the REPL
```
$ cabal repl
ghci> import ShuntingYard
ghci> convertToExpr "2+3"
ghci> convertToExpr "(2+3) * (3+2)"
```

#### EvalRPNTrans
```
$ cabal repl
ghci > import EvalRPNTrans
ghci > evalRPN "2 3 +"
ghci > evalRPN "2 3"
```
