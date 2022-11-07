{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-deriv #-}

import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

newtype Age = Age Int
  deriving (Show)

toAges ∷ [Int] → [Age]
toAges = map Age

-- >>> toAges [1..5]
-- [Age 1,Age 2,Age 3,Age 4,Age 5]

-- >>> toAges [20, 30, 40, 50]
-- [Age 20,Age 30,Age 40,Age 50]

toAges' ∷ [Int] → [Age]
toAges' = coerce

-- >>> toAges' [1..5]
-- [Age 1,Age 2,Age 3,Age 4,Age 5]

-- >>> toAges' [20, 30, 40, 50]
-- [Age 20,Age 30,Age 40,Age 50]

-- `fmap`/`map` should always replaced with `coerce` (unless the functor instance is polymorphic)!

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
  print $ toAges' [20, 30, 40, 50] --  [Age 20,Age 30,Age 40,Age 50]
