{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-deriv #-}

import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

newtype Age = Age Int
  deriving (Show)

toAges ∷ [Int] → [Age]
toAges = map Age

-- >>> toAges [20..25]
-- [Age 20,Age 21,Age 22,Age 23,Age 24,Age 25]

-- At runtime [Int] and [Age] are EXACTLY the same.
-- So toAges' should not do anything!
toAges' ∷ [Int] → [Age]
toAges' = coerce

-- >>> toAges' [20..25]
-- [Age 20,Age 21,Age 22,Age 23,Age 24,Age 25]

-- `fmap`/`map` + constructor should always replaced with `coerce` (unless the functor instance is polymorphic)!

-- Just as the type system ensures terms are used correctly,
-- and the kind system ensures types are logical,
-- THE ROLE SYSTEM ENSURES COERCIONS ARE SAFE.

-- Every type parameter for a given type constructor is assigned a role.
-- PHANTOM:          Two types are always phantomly equal to one another.
-- REPRESENTATIONAL: Types `a` and `b` are representationally equal iff it’s safe to reinterpret the memory of an `a` as a `b`.
-- NOMINAL:          The everyday notion of type-equality, corresponding to the `a ∼ b` constraint. E.g. Int is nominally equal only to itself.

-- There is an inherent ordering in roles:
-- phantom < representational < nominal
-- Upgrading from a weaker role to a stronger one is known as STRENGTHENING.

-- Just like types, roles are automatically inferred by the compiler, though they can be specified explicitly.
-- This inference works as follows:

-- 1. All type parameters are assumed to be at role PHANTOM.
-- 2. The type constructor (→) has two REPRESENTATIONAL roles; any type parameter applied to a (→) gets
--    upgraded to representational. Data constructors count as applying (→).
-- 3. The type constructor (∼) has two NOMINAL roles; any type parameter applied to a (∼) gets upgraded to
--    nominal. GADTs and type families count as applying (∼).

-- It's possible to strengthen an inferred role to a less permissive one by providing a role signature.

-- It's only possible to STRENGTHEN inferred roles.

-- `ageType` has representational role
data Student ageType = Student !String !ageType deriving (Show)

-- prohibits coercions between `Student Age` and `Student Int`
-- type role Student nominal

-- We can declare phantom roles as representational and representational roles as nominal, but not the other way around.

check ∷ Student Int → Student Age
check = coerce

-- >>> check (Student "Michael" 33)
-- Student "Michael" (Age 33)

-- ageType has representational role
data Student1 ageType = Student1 !String !(Maybe ageType) deriving (Show)

check1 ∷ Student1 Int → Student1 Age
check1 = coerce

-- >>> check1 (Student1 "Michael" (Just 33))
-- Student1 "Michael" (Just (Age 33))

-- ageType has nominal role bc we know nothing about `m`
data Student2 m ageType = Student2 !String !(m ageType)

-- Cannot coerce:
-- check2 ∷ Student2 Maybe Int → Student2 Maybe Age
-- check2 = coerce

type family Id t

type instance Id t = t

-- ageType has nominal role bc variables in type families and type classes are always nominal
data Student3 ageType = Student3 !String !(Id ageType)

-- Cannot coerce:
-- check3 ∷ Student3 Int → Student3 Age
-- check3 = coerce

-- When Haskell's role system is too weak, but we are sure that it's okay to coerce,
-- we can use the `unsafeCoerce` function from `Unsafe.Coerce` instead.
check3' ∷ Student3 Int → Student3 Age
check3' = unsafeCoerce

main ∷ IO ()
main = do
  print $ toAges [20, 30, 40, 50] --- [Age 20,Age 30,Age 40,Age 50]
  print $ toAges' [20, 30, 40, 50] -- [Age 20,Age 30,Age 40,Age 50]
