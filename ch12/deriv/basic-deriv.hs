{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -ddump-deriv #-}

import GHC.Generics

type Name = String

type Age = Int

data Student = Student !Name !Age
  deriving (Eq, Generic)

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

-- >>> let st1 = Student "Jane Doe" 20
-- >>> let st2 = Student "John Doe" 22
-- >>> st1 == st2
-- >>> st1 /= st2
-- False
-- True

main :: IO ()
main = do
  let st1 = Student "Jane Doe" 20
      st2 = Student "John Doe" 22
  print $ st1 == st2 -- False
  print $ st1 /= st2 -- True
