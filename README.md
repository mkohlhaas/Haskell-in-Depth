- Format `CSV` files on the command line:
  ``` shell
  $ cabal install tablize
  $ tablize tt.csv
  ```

- Page 17:
  - Use `Text` type for processing textual information instead of `String`.
  - Enable the `OverloadedStrings` extension to make it more convenient to use string literals as `Text` values.
    - [Overloaded string literals](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html)
    - Uses `IsString` type class to convert a `String` to target type (in this caes `Text`).
    - The extension `OverloadedStrings` replaces every string literal in the source code with a call `fromString`.
  - Use favorite package for representing data in text: `formatting` and `fmt` are good candidates.

- Page 19:
  - _"An experienced Haskeller often looks for a type class first and then starts coding."_

- Page 22:
  - Get info from `GHCi`:
    ``` shell
    ghci> :info Eq
    ghci> :doc Eq
    ```
- Page 27:
  - When encountering a new data type, it is always a good idea to look for provided instances.
  ``` shell
  ghci> :info Text
  instance Monoid Text -- Defined in 'Data.Text'
  instance Semigroup Text -- Defined in 'Data.Text'
  ```

- Page 28:
  - Enabling and disabling GHC extensions in GHCi
    - Source Code:
      ``` haskell
      {-# LANGUAGE OverloadedStrings #-}
      ````
    - GHCi:
      ``` haskell
      ghci> :set -XOverloadedStrings
      ghci> :set -XNoOverloadedStrings
      ```
- Page 32:
  ``` haskell
  rotateFromFile ∷ Direction → FilePath → IO ()
  rotateFromFile dir fname = do
    f ← readFile fname
    let turns = map read $ lines f
        finalDir = rotateMany dir turns
        dirs = rotateManySteps dir turns
    fmtLn $ "Final direction: " +|| finalDir ||+ "" -- using Show instance of Direction (for 'finalDir') !!!
    fmt $ nameF "Intermediate directions" (unwordsF dirs) -- using Buildable instance of Direction (for 'dirs') !!!
  ```
  - We don't use `putStr` or a similar function: the `fmt` function is clever enough to print the given value in the context, where `IO ()` is expected.
  - This is also implemented with type classes and instances.
  - The `fmt` result has the type `FromBuilder b => b`.
  - The `IO ()` instance of `FromBuilder` prints the given value.
  - The `FromBuilder` type class also has an instance for `Text`, so it can be used to return a `Text` value as well.

- Page 32: **Polymorphic Values**
  - The values of the `C a => a` type are called polymorphic, because they can be used in many forms, depending on the required type.
  - For example, we can use numeric values polymorphically without specifying a type, such as `Num a => a`.
  - `String` literals become polymorphic `IsString s => s` if we enable the `OverloadedStrings` GHC extension.
  - The `FromBuilder b => b` type follows the same idea.

- [List of all Extensions](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts.html)
- Used extensions in Chapter 2:
  - [DeriveAnyClass](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/derive_any_class.html)
  - [OverloadedStrings](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/overloaded_strings.html)
  - [StandaloneDeriving](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/standalone_deriving.html)
  - [UnicodeSyntax](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/unicode_syntax.html)
    - nvim shorthand `<leader>sh` for unicode support (sh = substitute Haskell)
- [Ormolu Magic Comments](https://github.com/tweag/ormolu#magic-comments): `{- ORMOLU_ENABLE -}`, `{- ORMOLU_DISABLE -}`

- `error` can be used anywhere as the output is any type:
  ``` haskell
  error :: ∀ a. HasCallStack => [Char] -> a
  ```

- Trick: Import type explicitly so you don't need to qualify it in signatures.
  ``` haskell
  import Data.Text (Text)             -- little trick
  import qualified Data.Text as T

  extractVocab ∷ Text → Vocabulary    -- instead of:
  extractVocab ∷ T.Text → Vocabulary
  ```

- Page 35: **Random Generators & Testing**
  - Creating a random `a` in a monadic context with `getStdRandom uniform`, e.g. in IO:
    ``` haskell
    randomA ∷ Uniform a ⇒ IO a
    randomA = getStdRandom uniform
    -- many as:
    randomAs :: Uniform a => Int -> IO [a]
    randomAs n = replicateM n randomA
    -- fix a type to generate specific random values, e.g. `Turns`:
    randomTurns ∷ Int → IO [Turn]
    randomTurns = uniformsIO
    ```
  - One also needs to implement a `Uniform` instance which is easy to implement with a `UniformRange` instance.
  - Just leverage existing instances for `Int`.
    ``` haskell
    instance UniformRange Turn where
      uniformRM (lo, hi) rng = toEnum <$> uniformRM (fromEnum lo, fromEnum hi) rng
    instance Uniform Turn where
      uniformM rng = uniformRM (minBound, maxBound) rng
    ```

- numeric types and most important functions
- fixed precision (2.2.3)
  - error function from Ch. 1

- Page 46:
  - The best advice on `Show` and `Read` is to avoid implementing them manually.
  - Derived `Show` and `Read` instances may still be used for simple cases when debugging or exploring code in `GHCi`.
  - It's always better to use some formatting (for `Show`) or a parsing library (for `Read`) instead.
