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

-- >>> toColumnsValues (from (Student 18265 "John Doe" 2))
-- (["studentId","name","year"],["18265","\"John Doe\"","2"])
--
-- >>> toColumnsValues (from (Course 1 "Math" "Jane Doe"))
-- (["courseId","title","instructor"],["1","\"Math\"","\"Jane Doe\""])

-- >>> insertInto "students" (Student 18265 "John Doe" 2)
-- "INSERT INTO students (studentId, name, year) VALUES (18265, \"John Doe\", 2)"

-- >>> insertInto "courses" (Course 1 "Math" "Jane Doe")
-- "INSERT INTO courses (courseId, title, instructor) VALUES (1, \"Math\", \"Jane Doe\")"

-- Doesn't compile because we don't support sum types (:+:).

data Status = Ok | Err
  deriving stock Generic
  deriving anyclass ToSQL

-- >>> insertInto "status" Ok
-- ToSQL does not support sum types.

-- >>> from (Student 18265 "John Doe" 2)
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 18265}} :*: (M1 {unM1 = K1 {unK1 = "John Doe"}} :*: M1 {unM1 = K1 {unK1 = 2}})}}

main ∷ IO ()
main = do
  TIO.putStrLn $ insertInto "students" (Student 18265 "John Doe" 2)
  TIO.putStrLn $ insertInto "courses" (Course 1 "Math" "Jane Doe")
  -- Does not compile with error message.
  -- TIO.putStrLn $ insertInto "status" Ok
