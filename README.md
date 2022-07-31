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
  - Use favorite package for representing data in text: `formatting` and `fmt` are good candidates.

- Page 19:
  - _"An experienced Haskeller often looks for a type class first and then starts coding."_

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

- random lib
- numeric types and most important functions
- fixed precision (2.2.3)
  - error function from Ch. 1

- Page 46:
  - The best advice on `Show` and `Read` is to avoid implementing them manually.
  - Derived `Show` and `Read` instances may still be used for simple cases when debugging or exploring code in `GHCi`.
  - It's always better to use some formatting (for `Show`) or a parsing library (for `Read`) instead.
