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
      uniformRM (lo, hi) = toEnum <$> uniformRM (fromEnum lo, fromEnum hi)
    instance Uniform Turn where
      uniformM = uniformRM (minBound, maxBound)
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
  - One problem with this type is that the argument now can be `Complex a`, but the radius cannot be a complex number - it must be real.
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
    ghci> sum xs / fromIntegral (length xs)
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

- Page 77:
  - `FromField (..)` in the import list for the `Data.Csv` module refers to the `FromField` type class and **every method** of this type class.
    ``` haskell
    import Data.Csv (FromField (..), FromNamedRecord)
    ```

- Page 82:
  - Nice application of [unzip3](https://hackage.haskell.org/package/base-4.16.3.0/docs/Prelude.html#v:unzip3):
    ``` haskell
    (candles, closings, volumes) = unzip3 $
      [ (Candle day low open 0 close high,
        (day, close),
        (day, [volume])) | QuoteData {..} ← toList quotes ]
    ```

- Page 99: Chapter 4
  - Special package, `base`, contains the definitions from the standard library (as defined by the Haskell 2010 Language Report) together with GHC-specific additions.
  - Import the whole module, `import Data.List`
  - Import only specific names from the module by listing them in parentheses after the module name, `import Data.Char (toLower, toUpper)`
  - Import no names at all with an empty list of names (note that this IMPORTS ALL THE INSTANCES because they have no names in Haskell), `import Data.Char ()`
  - Import names with optional or mandatory qualification with an alias (`as`) or a full module name to avoid name clashes, `import qualified MyModuleWithAVeryLongModuleName as Shorty`,  `import qualified MyModule`
  - Import all names except those listed after the hiding keyword. `import Data.Char hiding (toLower, toUpper)`

  - Exports:
    ``` haskell
    module ModuleName (
      module X,                 -- reexports everything from module X
      DataType1,                -- only the type constructor is exported.
      DataType2 (..),           -- exports the type constructor with all data constructors
      DataType3 (Cons1, Cons2), -- exports the type constructor with the two mentioned data constructors
      TypeClass1,
      TypeClass2 (..),
      TypeClass3 (method1, method2),
      fun1, fun2, fun3
    ) where
    ...
    ```
  - Remember also that instances of type classes are always exported.
  - The module name can be hierarchical, with components separated by dots (e.g., Graphics.Rendering.Chart.Backend.Cairo). The Haskell Report does not set out a meaning for this hierarchy, ALTHOUGH EXISTING IMPLEMENTATIONS normally use it as an instruction for finding module source code files in subdirectories.
  - Note that a module is always imported by its full name, regardless of whether we import it from the same subdirectory (as in S.A) or a neighboring subdirectory (as in T.B ).
    ![Module Hierarchy](data/pics/module_hierarchy.png)
  - Whenever we write import in a program, we import a particular module but not the whole module subhierarchy. Hierarchical module names are about naming only, and we shouldn’t expect anything else.

  - Provide include/module root path with -i`path` (no spaces after flag):
    ```shell
    $ ghci B.hs -i..
    ```

  - Do not import standard `Prelude`:
    ```haskell
    -- do not import Prelude
    {-# LANGUAGE NoImplicitPrelude #-}
    ```

  - Custom preludes:
    - https://hackage.haskell.org/packages/#cat:Prelude
    - https://hackage.haskell.org/packages/search?terms=prelude
    - relude (most stars on Github and very active; from kowainik); integration with [Summoner](https://kowainik.github.io/projects/summoner).
    - foundation
    - protolude
    - classy-prelude
    - universum

  - The role of `Setup.hs`:
    ```shell
    # download and unpack package
    $ curl http://hackage.haskell.org/package/timeit-2.0/timeit-2.0.tar.gz \ --output timeit-2.0.tar.gz
    $ tar -xf timeit-2.0.tar.gz
    $ cd timeit-2.0
    # configure, build and install package
    $ runhaskell Setup.hs configure
    $ runhaskell Setup.hs build
    $ runhaskell Setup.hs install
    ```

  - GHC operates two package databases by default: the global one and a user-specific one, arranged in a stack with the user-specific database on top.
  - It starts searching for packages at the top of the stack and continues all the way to the bottom.
  - We can specify additional databases or compile our project against a completely different stack of package databases using the `GHC_PACKAGE_PATH` environment variable.

  - Besides using GHC package databases, we can maintain our own package databases.
  - We can create a package database for a specific project.
  - The packages we have there don't intervene with anything within a system.

    ```shell
    $ ghc-pkg --help
    $ ghc-pkg list
    $ ghc-pkg list --user
    $ ghc-pkg find-module '*time*'
    $ ghc-pkg describe time
    ```

  - No matter which project management tool is used, `Cabal` as a library is always used to manipulate Haskell packages.
  - Cooking a package out of the source files is called *cabalizing*.

    ```shell
    $ cabal init # starting point for new project
    $ cabal-bounds --help
    $ cabal-bounds format
    $ cabal-bounds update
    ```

  - Haskell Package Versioning Policy - PVP - https://pvp.haskell.org/
  - Version numbers: https://pvp.haskell.org/#version-numbers
  - base-4.11.0.1 -> 4.11 is `major` version number, 0.1 is `minor` version number
  - Some people use semantic versioning: https://semver.org/
  - Author supports PVP.

  - Tip: Use hpack: https://github.com/sol/hpack

  - Cabal hell is history! But version of `base` package linked to GHC version!

  - Three approaches:
    - Curated sets of packages -> `Stack`
    - Sandboxing -> `old Cabal`
    - Persistent storage of uniquely identified package builds -> `Nix`, `new Cabal` (>=1.24, default since 3.0)

  - Cabal keeps every once-required version of every package built against every once-required set of dependencies in a shared environment, at the cost of greater storage requirements.
  - Cabal always tries to get the newest versions available. If not wanted use `cabal freeze`.

  - Check for outdated packages:
    ```shell
    cabal outdated
    ```

  - Use Hackage Dependency Monitor: https://packdeps.haskellers.com/

  - A package consisting of several packages is called a project and uses a `cabal.project` file, e.g. https://github.com/timbod7/haskell-chart/

  - `cabal.project.local` keeps special developer box settings, e.g.
    ```
    package gtk
      flags: +have-quartz-gtk
    ```

  - Many compiler options can be specified in `cabal.project`, either manually or by running the `cabal configure` command.

  - Use `cabal gen-bounds` to generate and test for version boundaries.

  - For starting a new project you can use the Summoner, https://kowainik.github.io/projects/summoner
