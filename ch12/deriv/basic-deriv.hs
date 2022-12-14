{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -ddump-deriv #-}

import GHC.Generics

type Name = String

type Age = Int

data Student = Student !Name !Age
  deriving (Eq, Generic)

-- For representable types of kind * use Generic, for representable types of kind * → * use Generic1 when deriving!

-- For Student we use Generic.
-- >>> :kind Student
-- Student ∷ Type

-- Pass `-ddump-deriv` flag to GHC or use `kind!` to see the generated code.
-- >>> :kind! Rep Student
-- Rep Student ∷ Type → Type
-- = D1
--     ('MetaData "Student" "Main" "main" 'False)
--     (C1
--        ('MetaCons "Student" 'PrefixI 'False)
--        (S1
--           ('MetaSel
--              'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--           (Rec0 Name)
--         :*: S1
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--               (Rec0 Age)))

-- removing meta-information:
--
-- Rep Student ∷ Type → Type
-- = D1 (C1 (S1 (Rec0 Name) :*: S1 (Rec0 Age)))
--
-- D = data type
-- C = constructor
-- S = selector
-- Rec0 is a type alias for K1: type Rec0 = K1 R. K1 houses actual values.
-- not used in this example but omnipresent in Generics:
-- M = meta-information
-- K = constants (, additional params and recursion of kind *)

-- Derived class instances from dump removing full qualification and randomly generated names:
--
-- instance Student where
--   (==) (Student a1 a2) (Student b1 b2) = (((a1 == b1)) && ((a2 == b2)))

-- Generics form an isomorphism (from . to = to . from = id):
-- instance Generic Student where
--   from x
--     = M1 (case x of {
--            Student g1 g2 → M1 ((:*:) (M1 (K1 g1)) (M1 (K1 g2)))
--            })
--
--   to (M1 x)
--     = case x of {
--         (M1 ((:*:) (M1 (K1 g1)) (M1 (K1 g2)))) → Student g1 g2
--        }

-- Rep is an asscociated type of type class Generic.

-- >>> :info Generic
-- type Generic ∷ Type → Constraint
-- class Generic a where
--   type Rep ∷ Type → Type → Type
--   type family Rep a
--   from ∷ a → Rep a x
--   to ∷ Rep a x → a

-- /= is not derived but filled in with a default implementation.
-- instance Eq Student where
--   /= = $dm/= @Student
--          |       |
--        splice    |
--             type annotation

-- >>> let st1 = Student "Jane Doe" 20
-- >>> let st2 = Student "John Doe" 22
-- >>> st1 == st2
-- >>> st1 /= st2
-- False
-- True

main ∷ IO ()
main = do
  let st1 = Student "Jane Doe" 20
      st2 = Student "John Doe" 22
  print $ st1 == st2 -- False
  print $ st1 /= st2 -- True
