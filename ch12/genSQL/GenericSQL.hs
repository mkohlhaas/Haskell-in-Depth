{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module GenericSQL where

import Data.Text (Text)
import GHC.Generics (Generic (Rep, from), K1 (K1), M1 (M1), S, Selector (selName), U1, type (:*:) (..))
import TextShow (Builder, TextShow (showb), fromString, fromText, showbCommaSpace, singleton, toText)

{-
INSERT INTO table_name (column1, column2, column3, ...)
VALUES (value1, value2, value3, ...);
-}

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

insertIntoDefault ∷ (Generic a, ToColumnsValuesLists (Rep a)) ⇒ Text → a → Text
insertIntoDefault table val = toText $ "INSERT INTO " <> fromText table <> " " <> buildersToList columns <> " VALUES " <> buildersToList values
  where
    (columns, values) = toColumnsValues (from val)

-- go over run-time representation and collect lists for field names and for the corresponding values
class ToColumnsValuesLists f where
  toColumnsValues ∷ f a → ([Builder], [Builder])

-- U1 is used for constructors without arguments → no names, no values
instance ToColumnsValuesLists U1 where
  toColumnsValues _ = ([], [])

-- multiple fields
instance (ToColumnsValuesLists a, ToColumnsValuesLists b) ⇒ ToColumnsValuesLists (a :*: b) where
  toColumnsValues (a :*: b) = (columns1 <> columns2, values1 <> values2)
    where
      (columns1, values1) = toColumnsValues a
      (columns2, values2) = toColumnsValues b

instance (ToColumnsValuesLists a) ⇒ ToColumnsValuesLists (M1 i c a) where
  toColumnsValues (M1 a) = toColumnsValues a

-- this instance overlaps with the previous one
-- match on selector (= field)
instance {-# OVERLAPPING #-} (ToColumnsValuesLists a, Selector c) ⇒ ToColumnsValuesLists (M1 S c a) where
  toColumnsValues s@(M1 a) = (fromString (selName s) : columns, values)
    where
      (columns, values) = toColumnsValues a

-- add value
instance TextShow a ⇒ ToColumnsValuesLists (K1 i a) where
  toColumnsValues (K1 a) = ([], [showb a])
