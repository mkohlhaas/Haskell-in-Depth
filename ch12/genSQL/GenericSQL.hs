{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericSQL where

import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, Symbol, TypeError, symbolVal)
import qualified GHC.TypeLits as Err
import TextShow (Builder, TextShow (showb), fromString, fromText, showbCommaSpace, singleton, toText)

{-

INSERT INTO table_name (column1, column2, column3, ...)
VALUES (value1, value2, value3, ...);

-}

-- 1. Define a type class that will be used in the deriving statement: `... deriving anyclass (ToSQL)`. (See genSQL/Main.hs.)
-- This class provides a default implementation using Rep's working for all kinds of data, Student, Course,...
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
    (columns, values) = toColumnsValues (from val)

-- Compare with ../deriv/basic-deriv.hs where we used type aliases!
-- >>> :kind! Rep Student
-- Rep Student ∷ Type → Type
-- = D1
--              not Strings, but Symbols
--          get their values with `symbolVal`
--                    |       |      |
--     ('MetaData "Student" "Main" "main" 'False)
--     (C1
--        ('MetaCons "Student" 'PrefixI 'True)
--        (S1
--           ('MetaSel
--              ('Just "studentId")
--              'NoSourceUnpackedness
--              'SourceStrict
--              'DecidedStrict)
--           (Rec0 Int) ← here is the student id!!!
--         :*: (S1
--                ('MetaSel
--                   ('Just "name") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                (Rec0 Text) ← here is the name!!!
--              :*: S1
--                    ('MetaSel
--                       ('Just "year") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                    (Rec0 Int)))) ← here is the year!!!

-- basically the same
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
--              (Rec0 Int) ← here is the student id!!!
--            :*: (S1
--                   ('MetaSel
--                      ('Just "name") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                   (Rec0 Text) ← here is the name!!!
--                 :*: S1
--                       ('MetaSel
--                          ('Just "year") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                       (Rec0 Int)))) ← here is the year!!!
--        x

-- go over run-time representation and collect lists for field names and for the corresponding values
class ToColumnsValuesLists f where
  toColumnsValues ∷ f a → ([Builder], [Builder])

-- U1
-- U1 (unit) is used for constructors without arguments → no names, no values
instance ToColumnsValuesLists U1 where
  toColumnsValues ∷ U1 a → ([Builder], [Builder])
  toColumnsValues _ = ([], [])

-- Product Types
-- `:*:` is a type operator and its constructor.
instance (ToColumnsValuesLists a, ToColumnsValuesLists b) ⇒ ToColumnsValuesLists (a :*: b) where
  toColumnsValues ∷ (ToColumnsValuesLists a, ToColumnsValuesLists b) ⇒ (:*:) a b a1 → ([Builder], [Builder])
  toColumnsValues (a :*: b) = (columns1 <> columns2, values1 <> values2)
    where
      (columns1, values1) = toColumnsValues a
      (columns2, values2) = toColumnsValues b

-- Sum Types
instance (TypeError (Err.Text "ToSQL does not support sum types.")) ⇒ ToColumnsValuesLists (a :+: b) where
  toColumnsValues = error "ToSQL does not support sum types." -- will never be called as it will not be compiled

-- M1 definition
-- Meta-information (constructor names, etc.)
-- newtype M1 (i ∷ Type) (c ∷ Meta) (f ∷ k → Type) (p ∷ k) =
--     M1 { unM1 ∷ f p }

-- Four type parameters:
-- 1.) Corresponds to the represented component (the data type itself (D1), constructor (C1), selector (S1), ...).

-- S1, C1, D1 are just type aliases for M1 (defined in GHC.Generics).
-- type D1 = M1 D → data types
-- type C1 = M1 C → constructors
-- type S1 = M1 S → record selectors

-- 2.) contains metainformation, which clearly depends on a component.
-- 3.) specifies a type whose representation we are constructing.
-- 4.) is the represented type itself.

-- M1 generic
instance (ToColumnsValuesLists a) ⇒ ToColumnsValuesLists (M1 _1 _2 a) where
  toColumnsValues ∷ ToColumnsValuesLists a ⇒ M1 _1 _2 a a1 → ([Builder], [Builder])
  toColumnsValues (M1 a) = toColumnsValues a

-- S1 (this instance overlaps with the previous one)
-- We have to match on S1 because we need the column names.
instance {-# OVERLAPPING #-} (ToColumnsValuesLists a, Selector c) ⇒ ToColumnsValuesLists (M1 S c a) where
  toColumnsValues ∷ (ToColumnsValuesLists a, Selector c) ⇒ M1 S c a a1 → ([Builder], [Builder])
  toColumnsValues s@(M1 a) = (fromString (selName s) : columns, values)
    where
      (columns, values) = toColumnsValues a

-- K1 (values)
-- Used in Student for the Integer and String values.
-- Shows up in the Rep as Rec0 which is a type alias for K1: type Rec0 = K1 R
instance TextShow a ⇒ ToColumnsValuesLists (K1 _1 a) where
  toColumnsValues ∷ TextShow a ⇒ K1 _1 a a1 → ([Builder], [Builder])
  toColumnsValues (K1 a) = ([], [showb a])

-- cleaned up from above
-- >>> :type from (Student 18265 "John Doe" 2)
-- from (Student 18265 "John Doe" 2) ∷ D1 (C1 (S1 (Rec0 Int) :*: (S1 (Rec0 Text) :*: S1 (Rec0 Int))))

-- K1 shows up here (copied from Main.hs)
-- >>> from (Student 18265 "John Doe" 2)
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 18265}} :*: (M1 {unM1 = K1 {unK1 = "John Doe"}} :*: M1 {unM1 = K1 {unK1 = 2}})}}
