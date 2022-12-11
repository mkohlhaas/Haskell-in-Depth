{-# LANGUAGE GADTs #-}

-- regular algebraic data type
-- data Dyn = S String | C Char | B Bool

-- We can think of the S, C, B data constructors as functions.
-- The types here demonstrate that we lose type information. We have only Dyn afterward.
-- S ∷ String → Dyn
-- C ∷ Char → Dyn
-- B ∷ Bool → Dyn

-- GADTs allow data constructors to return a data type parameterized by the specific types.
-- GADTs allow keeping and using information about types after constructing a value.
-- `a` can be String, Char, Bool. Compare this to e.g. `Maybe a` that doesn't restrict `a` in any way, shape or form.
-- GADTs allow data constructors to return a data type parameterized by specific types!
-- In comparison, e.g. a `Maybe a` always returns a `Maybe a` for all `a`!
data DynValue a where ----------- GADTs come with a new syntax, `data/where`.
  S ∷ String → DynValue String -- returns a String parameterized dynamic value
  C ∷ Char → DynValue Char
  B ∷ Bool → DynValue Bool

-- With a GADT we can easily and safely get back to a value of the original type.
getValue ∷ DynValue a → a
getValue (S s) = s
getValue (B b) = b
getValue (C c) = c

-- Different equations of the function definition return DIFFERENT types depending on the constructor(!):

-- >>> getValue (S "hello")
-- "hello"

-- >>> getValue (B True)
-- True

-- >>> getValue (C 'c')
-- 'c'

printValue ∷ DynValue a → IO ()
printValue (S s) = print s -- s has type String, and GHC knows how to print String
printValue (B b) = print b -- b has type Bool,   and GHC knows how to print Bool
printValue (C c) = print c -- c has type Char,   and GHC knows how to print Char

-- works in the REPL
-- >>> mapM_ printValue [S "hello", S "bye"]
-- "hello"
-- "bye"

-- >>> map getValue [S "hello", S "bye"]
-- ["hello","bye"]

-- does NOT work
-- >>> mapM_ printValue [S "hello", S "bye", B True]
-- Couldn't match type ‘Bool’ with ‘[Char]’
-- Expected type: DynValue String
--   Actual type: DynValue Bool

-- >>> map getValue [S "hello", S "bye", B True]
-- Couldn't match type ‘Bool’ with ‘[Char]’
-- Expected type: DynValue String
--   Actual type: DynValue Bool

-- Is it possible to write a function of  type `a → DynValue a` ?
-- In fact, we cannot provide `DynValue a` for all types. We are limited to `a` being String, Char and Bool(!)

-- Workaround Technique
-- Existential wrapper: The `a` type variable used in the Wrap constructor is traditionally called EXISTENTIAL: it exists somewhere inside a value.
data WrappedDynValue where
  Wrap ∷ DynValue a → WrappedDynValue -- there is no `a` in WrappedDynValue

-- The WrappedDynValue GADT can be used to create a dynamic value.
fromString ∷ String → WrappedDynValue
fromString str
  | str `elem` ["y", "yes", "true"] = Wrap (B True)
  | str `elem` ["n", "no", "false"] = Wrap (B False)
  | length str == 1 = Wrap (C $ head str)
  | otherwise = Wrap (S str)

-- Do something with the wrapped value by simply pattern matching:
printWDValue ∷ WrappedDynValue → IO ()
printWDValue (Wrap dv) = printValue dv

-- Compiler Error: Couldn't match expected type ‘p’ with actual type ‘DynValue a’ because type variable ‘a’ would escape its scope.
-- getWDValue (Wrap dv) = dv

main ∷ IO ()
main = mapM_ (printWDValue . fromString) ["y", "no", "xxx", "c"]
-- True
-- False
-- "xxx"
-- 'c'
--
-- We get additional control by keeping specific types as parameters to a GADT type constructor.
-- We can always get back to the original types and do whatever they support.
-- We can use existential typing techniques to build GADTs from other types and pass those wrapped types around until we need the power of original types.
