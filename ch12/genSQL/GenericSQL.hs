{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

module GenericSQL where

import Data.Text (Text)
import GHC.Generics (Generic (Rep, from), K1 (K1), M1 (M1), S, Selector (selName), U1, type (:*:) (..))
import TextShow (Builder, TextShow (showb), fromString, fromText, showbCommaSpace, singleton, toText)

{-

INSERT INTO table_name (column1, column2, column3, ...)
VALUES (value1, value2, value3, ...);

-}

-- 1. Define a Class that will be used in the deriving statement: `... deriving anyclass (ToSQL)`. (See genSQL/Main.hs.)
-- This class always uses a default which must then be implemented for the anyclass deriving mechanism.
class ToSQL a where
  insertInto ∷ Text → a → Text
  default insertInto ∷ (Generic a, ToColumnsValuesLists (Rep a)) ⇒ Text → a → Text
  insertInto = insertIntoDefault

buildersToList ∷ [Builder] → Builder
buildersToList [] = "()"
buildersToList (x : xs) = singleton '(' <> x <> go xs
  where
    go (y : ys) = showbCommaSpace <> y <> go ys
    go [] = singleton ')'

-- Usage: insertInto "students" (Student 18265 "John Doe" 2)
-- More interesting would be to derive the table name from the data structure, e.g. Student → students.
-- Could be done by pattern matching over D1.
insertIntoDefault ∷ (Generic a, ToColumnsValuesLists (Rep a)) ⇒ Text → a → Text
insertIntoDefault table val = toText $ "INSERT INTO " <> fromText table <> " " <> buildersToList columns <> " VALUES " <> buildersToList values
  where
    (columns, values) = toColumnsValues (from val) -- `from` generates the generic Rep(resentation)

-- `a` in insertIntoDefault could be Student:
-- >>> :type from (Student 18265 "John Doe" 2)
-- from (Student 18265 "John Doe" 2)
--   ∷ D1
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
--

-- here K1 shows up
-- >>> from (Student 18265 "John Doe" 2)
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 18265}} :*: (M1 {unM1 = K1 {unK1 = "John Doe"}} :*: M1 {unM1 = K1 {unK1 = 2}})}}

-- go over run-time representation and collect lists for field names and for the corresponding values
class ToColumnsValuesLists f where
  toColumnsValues ∷ f a → ([Builder], [Builder])

-- U1 is used for constructors without arguments → no names, no values
-- It's the unit type. Houses a single value. (Not used in Student but might be in other data structures.)
instance ToColumnsValuesLists U1 where
  toColumnsValues ∷ U1 a → ([Builder], [Builder])
  toColumnsValues _ = ([], [])

-- product types
-- (Student is a product type.)
instance (ToColumnsValuesLists a, ToColumnsValuesLists b) ⇒ ToColumnsValuesLists (a :*: b) where
  toColumnsValues ∷ (ToColumnsValuesLists a, ToColumnsValuesLists b) ⇒ (:*:) a b a1 → ([Builder], [Builder])
  toColumnsValues (a :*: b) = (columns1 <> columns2, values1 <> values2)
    where
      (columns1, values1) = toColumnsValues a
      (columns2, values2) = toColumnsValues b

-- The M1 type has the following four type parameters:
-- newtype M1 (i ∷ Type) (c ∷ Meta) (f ∷ k → Type) (p ∷ k)
-- The first one corresponds to a represented component (the data type itself (D1), constructor (C1), or selector (S1)).

-- S1, C1, D1 are just type aliases for M1 (defined in GHC.Generics).
-- type D1 = M1 D → data types
-- type C1 = M1 C → constructors
-- type S1 = M1 S → record selectors

-- The second parameter contains metainformation, which clearly depends on a component.
-- The third  parameter specifies a type whose representation we are constructing.
-- The fourth parameter is the represented type itself.

-- M1 generic
instance (ToColumnsValuesLists a) ⇒ ToColumnsValuesLists (M1 _1 _2 a) where
  toColumnsValues ∷ ToColumnsValuesLists a ⇒ M1 _1 _2 a a1 → ([Builder], [Builder])
  toColumnsValues (M1 a) = toColumnsValues a

-- S1 (this instance overlaps with the previous one)
instance {-# OVERLAPPING #-} (ToColumnsValuesLists a, Selector c) ⇒ ToColumnsValuesLists (M1 S c a) where
  toColumnsValues ∷ (ToColumnsValuesLists a, Selector c) ⇒ M1 S c a a1 → ([Builder], [Builder])
  toColumnsValues s@(M1 a) = (fromString (selName s) : columns, values)
    where
      (columns, values) = toColumnsValues a

-- K1 (values)
-- Used in Student for the Integer and String values.
-- Doesn't show up in the Rep above because it's a type(!), there are no values.
-- Uncomment this instance and run `cabal run generic-sql` to see for yourself.
instance TextShow a ⇒ ToColumnsValuesLists (K1 _1 a) where
  toColumnsValues ∷ TextShow a ⇒ K1 _1 a a1 → ([Builder], [Builder])
  toColumnsValues (K1 a) = ([], [showb a])
