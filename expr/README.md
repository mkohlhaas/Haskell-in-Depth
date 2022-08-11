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

```shell
$ cabal run prefix-postfix
```

#### EvalRPNs

```
$ cabal repl
ghci > import EvalRPNTrans
ghci > evalRPN "2 3 +"
ghci > evalRPN "2 3"
```

```shell
$ cabal run evalrpn1 -- 2 3 + 4 \*
⇒ 2 3 + 4 * = 20
$ cabal run evalrpn2 -- 2 3 + 4 \*
⇒ 2 3 + 4 * = Just 20
$ cabal run evalrpn3 -- 2 3 + 4 \*
⇒ 2 3 + 4 * = Just 20
```

```shell
$ cabal run evalrpn1 -- 2 3 + 4 \* 5
⇒ 2 3 + 4 * 5 = 5
$ cabal run evalrpn2 -- 2 3 + 4 \* 5
⇒ 2 3 + 4 * 5 = Nothing
$ cabal run evalrpn3 -- 2 3 + 4 \* 5
⇒ 2 3 + 4 * 5 = Nothing
```

#### Exception Handling

```
$ cabal run rpnexpr
```

- Programmable Exceptions (libraries; mainly Monad Transformers -> esp. `ExceptT` from `mtl` package)
- Extensible Exceptions (GHC runtime; also at library level; mainly used in IO)
  - GHC runtime exceptions: we stick with interfaces defined in the `base` and `exceptions` packages.

It’s a good idea to use ExceptT for monad stacks based on anything except for the IO monad.

Exception in the context of lazy evaluation, mulit-threading, already acquired resources:
- Unfortunately, no general solution to these problems exists.
- The only thing we can do is adhere to some sort of a programming discipline.
- In short, we should do the right things and shouldn’t do the wrong things.

Exception handling introduces significant complexity and potential performance drawbacks:
- If we can avoid using exceptions at all, we should avoid using them!

The `-W` option of the GHC compiler enables a lot of helpful warnings.
There are also `-Wall` and `-Weverything` for enabling most of the warnings and all of them, respectively.
Set these options for your project via the `ghc-option` field of the Cabal file.
