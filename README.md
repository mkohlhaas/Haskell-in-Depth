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
  - The `fmt` result has the type `FromBuilder b ⇒ b`.
  - The `IO ()` instance of `FromBuilder` prints the given value.
  - The `FromBuilder` type class also has an instance for `Text`, so it can be used to return a `Text` value as well.

- Page 32: **Polymorphic Values**
  - The values of the `C a ⇒ a` type are called polymorphic, because they can be used in many forms, depending on the required type.
  - For example, we can use numeric values polymorphically without specifying a type, such as `Num a ⇒ a`.
  - `String` literals become polymorphic `IsString s ⇒ s` if we enable the `OverloadedStrings` GHC extension.
  - The `FromBuilder b ⇒ b` type follows the same idea.

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
  error ∷ ∀ a. HasCallStack ⇒ [Char] → a
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
    randomAs ∷ Uniform a ⇒ Int → IO [a]
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

- Page 39:
  - Hierarchy of numeric type classes:
  - ![Hierarchy of numeric type classes](ch02/numeric-classes.png)
  - `Word`s are strictly positive!
  - `Int`s are positive and negative.
    ``` shell
    ghci> (minBound, maxBound) ∷ (Word, Word)
    (0,18446744073709551615)
    ghci> (minBound, maxBound) ∷ (Int, Int)
    (-9223372036854775808,9223372036854775807)
    ```
  - One problem with this type is that the argument now can be Complex a, but the radius cannot be a complex number - it must be real.
    ``` haskell
    circleArea ∷ Floating a ⇒ a → a
    circleArea r = pi * r * r
    ```
  - realToFrac ∷ (Real a, Fractional b) ⇒ a → b
    - `Floating` extends `Fractional` ⇒ we can use it to get a value of any type `b` with the `Floating` instance
      ``` haskell
      circleArea ∷ (Real a, Floating b) ⇒ a → b
      circleArea r = pi * realToFrac (r * r)
      ```
  - fromIntegral ∷ (Integral a, Num b) ⇒ a → b
    ``` haskell
    xs ∷ [Int]
    xs = [1,2,3,4,5]
    ```
    ``` shell
    ghci> fromIntegral (sum xs) / fromIntegral (length xs)
    3.0
    ```

- Page 42:
  - Fixed Precision
    ``` shell
    ghci> import Data.Fixed
    ghci> 3.141592653589793 :: Deci
    3.1
    ghci> 3.141592653589793 :: Centi
    3.14
    ghci> 3.141592653589793 :: Milli
    3.141
    ghci> 3.141592653589793 :: Micro
    3.141592
    ghci> 3.141592653589793 :: Nano
    3.141592653
    ghci> 3.141592653589793 :: Pico
    3.141592653589
    ```
  - Define own resolution:
    ``` haskell
    instance HasResolution E4 where
      resolution _ = 10000
    type Fixed4 = Fixed E4
    ```
    ``` shell
    ghci> 3.141592653589793 :: Fixed4
    3.1415
    ```

- Page 46:
  - The best advice on `Show` and `Read` is to avoid implementing them manually.
  - Derived `Show` and `Read` instances may still be used for simple cases when debugging or exploring code in `GHCi`.
    - `show` and `read` are inverse operations: read (show a) == a; show (read a) == a
  - But it is always better to use some formatting (for `Show`) or a parsing library (for `Read`) instead.
  - `String` is very inefficient and should never be used in production code. Use `Text` instead!

- Page 47:
  - `TextShow` supports converting recursive data types to `Text` with parentheses, depending on precedence!

- Page 51:
  - ![Abstract Computations](ch02/monad_traversable.png)
