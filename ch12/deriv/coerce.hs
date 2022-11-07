{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-deriv #-}

import Data.Coerce
import Unsafe.Coerce

newtype Age = Age Int
  deriving (Show)

toAges ∷ [Int] → [Age]
toAges = map Age

toAges' ∷ [Int] → [Age]
toAges' = coerce

-- ageType has representational role
data Student ageType = Student !String !ageType

-- prohibits coercions between Student Age and Student Int
-- type role Student nominal

-- We can declare phantom roles as representational and representational roles as nominal, but not the other way around.

check ∷ Student Int → Student Age
check = coerce

-- ageType has representational role
data Student1 ageType = Student1 !String !(Maybe ageType)

check1 ∷ Student1 Int → Student1 Age
check1 = coerce

-- ageType has nominal role bc we know nothing about `m`
data Student2 m ageType = Student2 !String !(m ageType)

-- Can't coerce:
{-
check2 ∷ Student2 Maybe Int → Student2 Maybe Age
check2 = coerce
-}

type family Id t

type instance Id t = t

-- ageType has nominal role bc variables in type families and type classes are always nominal
data Student3 ageType = Student3 !String !(Id ageType)

{-
-- Can't coerce:
check3 ∷ Student3 Int → Student3 Age
check3 = coerce
-}

-- When Haskell's role system is too weak, but we are sure that it's okay to coerce, we can use the unsafeCoerce function from Unsafe.Coerce instead.
check3' ∷ Student3 Int → Student3 Age
check3' = unsafeCoerce

-- |
-- >>> toAges [20, 30, 40, 50]
-- [Age 20,Age 30,Age 40,Age 50]

-- |
-- >>> toAges' [20, 30, 40, 50]
-- [Age 20,Age 30,Age 40,Age 50]

main ∷ IO ()
main = do
  print $ toAges [20, 30, 40, 50]
  print $ toAges' [20, 30, 40, 50]
