{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

-- Type operators are just infix versions of types!

-- sum type
-- Type is `+`, type variables are `a` and `b`
data a + b = Inl !a | Inr !b
  deriving Show

-- This would be the corresponding data type in prefix notation.
-- data Sumtype a b = Inl !a | Inr !b
--   deriving Show

-- product type (with custom data constructor `:*:`)
data a * b = a :*: b -- infix data constructors names are required to start with ':'
  deriving Show

-- We can give precedences and associativity to types!
infixl 6 +
infixl 7 *

-- now the following types are allowed
boolOrInt1 ∷ Bool + Int
boolOrInt1 = Inl False

boolOrInt2 ∷ Bool + Int
boolOrInt2 = Inr 100

stringAndInteger ∷ String * Maybe Integer
stringAndInteger = "Hello" :*: Nothing

-- getters for products
first ∷ a * b → a
first (a :*: _) = a

second ∷ a * b → b
second (_ :*: b) = b

-- Type operators make type signatures shorter and more evident.

-- Compare the type `Either Int (Bool, Bool)` (a sum type) with the following:
val1 ∷ Int + Bool * Bool
val1 = Inl 0

val2 ∷ Int + Bool * Bool
val2 = Inr (True :*: False)

-- store a point in 1D-, 2D- or 3D-space
type Point a = a + a * a + a * a * a

-- Left associativity with the specified precedence means that `a + a * a + a * a * a` is parsed as follows:
-- handle precedence: a + (a * a) + (a * a * a)
-- handle left associativity for `*`: a + (a * a) + ((a * a) * a)
-- handle left associativity for `+`: (a + (a * a)) + ((a * a) * a)
--                                     │
--                                    1-D   │___│
--                                           2-D
--                                                    │___________│
--                                                         3-D

-- origin in 1D-space
zero1D ∷ Point Int
zero1D = Inl (Inl 0)

-- origin in 2D-space
zero2D ∷ Point Int
zero2D = Inl (Inr (0 :*: 0))

-- origin in 3D-space
zero3D ∷ Point Int
zero3D = Inr (0 :*: 0 :*: 0)

main ∷ IO ()
main = do
  print val1
  print val2
  print zero2D
