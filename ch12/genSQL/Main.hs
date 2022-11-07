{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import GHC.Generics
import GenericSQL

data Student = Student {studentId ∷ !Int, name ∷ !Text, year ∷ !Int}
  deriving (Generic, Show, ToSQL) -- GHC will use the anyclass strategy for ToSQL.
  -- deriving stock (Generic, Show)
  -- deriving anyclass (ToSQL)

data Course = Course {courseId ∷ !Int, title ∷ !Text, instructor ∷ !Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToSQL)

-- >>> from (Student 18265 "John Doe" 2)
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 18265}} :*: (M1 {unM1 = K1 {unK1 = "John Doe"}} :*: M1 {unM1 = K1 {unK1 = 2}})}}

-- >>> from (Course 1 "Math" "Jane Doe")
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 1}} :*: (M1 {unM1 = K1 {unK1 = "Math"}} :*: M1 {unM1 = K1 {unK1 = "Jane Doe"}})}}

-- >>> toColumnsValues (from (Student 18265 "John Doe" 2))
-- (["studentId","name","year"],["18265","\"John Doe\"","2"])

-- >>> toColumnsValues (from (Course 1 "Math" "Jane Doe"))
-- (["courseId","title","instructor"],["1","\"Math\"","\"Jane Doe\""])

-- >>> :type from (Student 18265 "John Doe" 2)
-- from (Student 18265 "John Doe" 2)
--   :: D1
--        ('MetaData "Student" "Main" "main" 'False)
--        (C1
--           ('MetaCons "Student" 'PrefixI 'True)
--           (S1
--              ('MetaSel
--                 ('Just "studentId")
--                 'NoSourceUnpackedness
--                 'SourceStrict
--                 'DecidedStrict)
--              (Rec0 Int)
--            :*: (S1
--                   ('MetaSel
--                      ('Just "name") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                   (Rec0 Text)
--                 :*: S1
--                       ('MetaSel
--                          ('Just "year") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                       (Rec0 Int))))
--        x

-- >>> :kind! Rep Student
-- Rep Student :: Type -> Type
-- = D1
--     ('MetaData "Student" "Main" "main" 'False)
--     (C1
--        ('MetaCons "Student" 'PrefixI 'True)
--        (S1
--           ('MetaSel
--              ('Just "studentId")
--              'NoSourceUnpackedness
--              'SourceStrict
--              'DecidedStrict)
--           (Rec0 Int)
--         :*: (S1
--                ('MetaSel
--                   ('Just "name") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                (Rec0 Text)
--              :*: S1
--                    ('MetaSel
--                       ('Just "year") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                    (Rec0 Int))))

-- >>> insertInto "students" (Student 18265 "John Doe" 2)
-- "INSERT INTO students (studentId, name, year) VALUES (18265, \"John Doe\", 2)"

-- >>> insertInto "courses" (Course 1 "Math" "Jane Doe")
-- "INSERT INTO courses (courseId, title, instructor) VALUES (1, \"Math\", \"Jane Doe\")"

-- Doesn't compile bc we don't support constructors (`C1`):
{-
data Status = Ok | Err
  deriving stock Generic
  deriving anyclass ToSQL

-- No instance for (ToColumnsValuesLists (C1 ('MetaCons "Ok" 'PrefixI 'False) U1 :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1))
-- arising from the 'deriving' clause of a data type declaration
-}

main ∷ IO ()
main = do
  TIO.putStrLn $ insertInto "students" (Student 18265 "John Doe" 2)
  TIO.putStrLn $ insertInto "courses" (Course 1 "Math" "Jane Doe")
