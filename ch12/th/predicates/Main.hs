{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- In general, the term reification means turning something abstract into something concrete.
-- In Template Haskell, we reify names into full information about the entities behind those names.
-- As a result, we can inspect this information and use it to generate code.

-- We can generate code based on the code we already have.

module Main where

import Predicates (mkPredicates)
import Language.Haskell.TH

-- This is the data we already have.
data Shape
  = Circle !Double
  | Square !Double
  | Triangle !Double !Double !Double

-- The built-in syntax 'f and ''T can be used to construct names.
-- The expression 'f gives a Name which refers to the value f.
-- ''T gives a Name which refers to the type T.

-- 3. Use TH splices to actually generate code for predicates.
-- We refer to the name of the data type in the TH splice using the '' prefix.
$(mkPredicates ''Shape)

-- >>> :type mkPredicates ''Shape
-- mkPredicates ''Shape ∷ Q [Dec]

-- This is the code we generate from our existing data.
--
-- This will be generated by mkPredicates:
-- isCircle ∷ Shape → Bool
-- isCircle = \case
--   Circle _ → True
--   _ → False
--
-- isSquare ∷ Shape → Bool
-- isSquare = \case
--   Square _ → True
--   _ → False
--
-- isTriangle ∷ Shape → Bool
-- isTriangle = \case
--   Triangle _ _ _ → True
--   _ → False

-- >>> s1 = Circle 4
-- >>> s2 = Square 10
-- >>> s3 = Triangle 1 1 1
-- >>> [isCircle s1, isSquare s2, isTriangle s3]
-- >>> [isCircle s2, isSquare s3, isTriangle s1]
-- [True,True,True]
-- [False,False,False]

main ∷ IO ()
main = do
  mapM_ print [isCircle s1, isSquare s2, isTriangle s3] -- [True,True,True]
  mapM_ print [isCircle s2, isSquare s3, isTriangle s1] -- [False,False,False]
  where
    s1 = Circle 4
    s2 = Square 10
    s3 = Triangle 1 1 1
